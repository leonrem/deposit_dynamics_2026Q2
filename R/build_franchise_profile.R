# Deposit-franchise profile for the 15 banks in the memo's Table 1 (2026Q2), plus a price-leader gap table by peer group.
# Everything comes from Call Reports (via the deposit panel) and the FDIC Summary of Deposits; no vendor data.
#
# Usage: Rscript R/build_franchise_profile.R
# Writes: data/analysis/franchise_profile_2026Q2.csv, data/analysis/price_leader_gap_2026Q2.csv

suppressMessages({library(dplyr); library(tidyr)})
setwd(if (file.exists("R/ffiec_data.R")) "." else "..")

TABLE1 <- c("PNFP", "EWBC", "FCNCA", "USB", "FHN", "TFC", "HBAN", "CFG", "PNC", "WAL", "KEY", "MTB", "ZION", "FITB", "RF")
Q <- as.Date("2026-06-30"); UP0 <- as.Date("2021-12-31"); UP1 <- as.Date("2024-09-30")

panel <- arrow::read_parquet("data/analysis/deposit_panel_firm.parquet")
at <- function(d) panel |> filter(report_date == d)
beta <- function(var, d0, d1) {
  a <- at(d0) |> select(ticker, c0 = all_of(var), e0 = effr_avg)
  b <- at(d1) |> select(ticker, c1 = all_of(var), e1 = effr_avg)
  inner_join(a, b, by = "ticker") |> transmute(ticker, b = (c1 - c0) / (e1 - e0))
}
betas <- beta("cost_total_deposits", UP0, UP1) |> rename(beta_up = b) |>
  left_join(beta("cost_total_deposits", UP1, Q) |> rename(beta_down = b), by = "ticker") |>
  left_join(beta("cost_ib_deposits", UP0, UP1) |> rename(ib_beta_up = b), by = "ticker") |>
  left_join(beta("cost_ib_deposits", UP1, Q) |> rename(ib_beta_down = b), by = "ticker")

# NIB-share migration over the hiking cycle (percent of the starting share that left)
mig <- at(UP0) |> select(ticker, nib0 = nib_share) |>
  inner_join(at(UP1) |> select(ticker, nib1 = nib_share), by = "ticker") |>
  transmute(ticker, nib_lost_pct = 100 * (nib1 / nib0 - 1))

# Branch reliance: SOD deposits as a share of Call deposits (June 2026), and the top three SOD markets (capped version)
sod <- arrow::read_parquet("data/analysis/sod_firm_year.parquet") |> filter(year == 2026, basis == "current")
sod_all <- sod |> filter(version == "all_branches") |> select(ticker, sod_all = sod_deposits, n_branches, metro_pct)
sod_cap <- sod |> filter(version == "capped") |> select(ticker, sod_capped = sod_deposits, top5_market_pct, dw_gsib_other_share)
mk <- arrow::read_parquet("data/analysis/sod_firm_market.parquet") |>
  filter(year == 2026, version == "capped", basis == "current") |>
  group_by(ticker) |> arrange(desc(firm_deposits), .by_group = TRUE) |>
  summarise(top_markets = paste(sprintf("%s (%.0f%%)", sub(",.*", "", head(market_name, 3)),
                                        100 * head(firm_deposits, 3) / sum(firm_deposits)), collapse = "; "), .groups = "drop")

prof <- at(Q) |> filter(ticker %in% TABLE1) |>
  transmute(ticker, deposits_B = deposits / 1e6, assets_B = assets / 1e6, nib_pct = nib_share,
            cost_total = cost_total_deposits, cost_ib = cost_ib_deposits, time_pct = time_share,
            uninsured_pct = uninsured_share, brokered_pct = brokered_share, ls_to_dep = loans_sec_to_deposits,
            wholesale_pct = 100 - deposits_to_liabilities) |>
  left_join(betas, by = "ticker") |> left_join(mig, by = "ticker") |>
  left_join(sod_all, by = "ticker") |> left_join(sod_cap, by = "ticker") |> left_join(mk, by = "ticker") |>
  mutate(sod_all_pct_of_call = 100 * sod_all / (deposits_B * 1e6), sod_capped_pct_of_call = 100 * sod_capped / (deposits_B * 1e6)) |>
  select(-sod_all, -sod_capped) |> arrange(match(ticker, TABLE1))
readr::write_csv(prof |> mutate(across(where(is.numeric), ~ round(.x, 3))), "data/analysis/franchise_profile_2026Q2.csv")

# Price-leader gap: interest-bearing deposit cost by peer group, 2026Q2, against the effective fed funds rate
gap <- at(Q) |> mutate(group = case_when(ticker %in% TABLE1 ~ "Table 1 regionals", TRUE ~ peer_group)) |>
  group_by(group) |> summarise(n = n(), median_ib_cost = median(cost_ib_deposits), median_total_cost = median(cost_total_deposits),
                               median_nib = median(nib_share), effr_avg = first(effr_avg), .groups = "drop") |>
  mutate(ib_cost_minus_effr_bp = 100 * (median_ib_cost - effr_avg))
readr::write_csv(gap |> mutate(across(where(is.numeric), ~ round(.x, 3))), "data/analysis/price_leader_gap_2026Q2.csv")
print(as.data.frame(prof |> mutate(across(where(is.numeric), ~ round(.x, 1)))), row.names = FALSE, right = FALSE)
print(as.data.frame(gap |> mutate(across(where(is.numeric), ~ round(.x, 2)))), row.names = FALSE)
