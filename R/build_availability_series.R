# Deposit availability: where the balances went since the first rate cut, who gained them, and how they were funded.
# Call Report based (pro forma firm panel, so 2025-26 acquisitions do not masquerade as growth). Dollars summed to bank or group,
# ratios recomputed from the sums.
#
# Windows: first cut = 2024Q3 (2024-09-30) -> 2026Q2; last cut = 2025Q4 (2025-12-31) -> 2026Q2.
#   core        = all deposits less domestic brokered and domestic time deposits (a rough "relationship" bucket)
#   wholesale   = liabilities not funded by deposits
# Usage: Rscript R/build_availability_series.R
# Writes: data/analysis/availability_by_bank.csv, availability_by_group.csv

suppressMessages({library(dplyr); library(tidyr)})
setwd(if (file.exists("R/ffiec_data.R") ) "." else "..")

TABLE1 <- c("PNFP", "EWBC", "FCNCA", "USB", "FHN", "TFC", "HBAN", "CFG", "PNC", "WAL", "KEY", "MTB", "ZION", "FITB", "RF")
LEADERS <- c("COF", "AXP", "SYF", "ALLY"); GSIBS <- c("JPM", "BAC", "WFC", "C")
D0 <- as.Date("2024-09-30"); D1 <- as.Date("2025-12-31"); D2 <- as.Date("2026-06-30")

pf <- arrow::read_parquet("data/analysis/deposit_panel_firm_proforma.parquet") |>
  filter(report_date %in% c(D0, D1, D2)) |>
  mutate(group = case_when(ticker %in% TABLE1 ~ "Regionals (15)", ticker %in% GSIBS ~ "GSIBs", ticker %in% LEADERS ~ "Direct banks (leaders)", TRUE ~ NA_character_),
         core = deposits - brokered - time_deposits,         # all deposits less domestic brokered and domestic time
         wholesale = liabilities - deposits)

measures <- function(d) d |> group_by(report_date) |> summarise(across(c(deposits, nib, core, time_deposits, brokered, uninsured_est, dep_domestic, loans,
                                                                         securities_htm, securities_afs, liabilities, wholesale), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
pack <- function(m) {
  g <- function(v, d) m[[v]][m$report_date == d]
  tibble(deposits_B = g("deposits", D2) / 1e6,
         dep_g_since_first_cut = 100 * (g("deposits", D2) / g("deposits", D0) - 1),
         dep_g_since_last_cut  = 100 * (g("deposits", D2) / g("deposits", D1) - 1),
         # contribution to deposit growth since the first cut, in percentage points of starting deposits (core + time + brokered = total growth)
         contrib_core_pts      = 100 * (g("core", D2) - g("core", D0)) / g("deposits", D0),
         contrib_time_pts      = 100 * (g("time_deposits", D2) - g("time_deposits", D0)) / g("deposits", D0),
         contrib_brokered_pts  = 100 * (g("brokered", D2) - g("brokered", D0)) / g("deposits", D0),
         nib_share_first = 100 * g("nib", D0) / g("deposits", D0), nib_share_last = 100 * g("nib", D1) / g("deposits", D1), nib_share_now = 100 * g("nib", D2) / g("deposits", D2),
         nib_g_since_first_cut = 100 * (g("nib", D2) / g("nib", D0) - 1),
         brokered_share_first = 100 * g("brokered", D0) / g("dep_domestic", D0), brokered_share_now = 100 * g("brokered", D2) / g("dep_domestic", D2),
         loan_g_since_first_cut = 100 * (g("loans", D2) / g("loans", D0) - 1),
         ls_dep_first = 100 * (g("loans", D0) + g("securities_htm", D0) + g("securities_afs", D0)) / g("deposits", D0),
         ls_dep_now = 100 * (g("loans", D2) + g("securities_htm", D2) + g("securities_afs", D2)) / g("deposits", D2),
         wholesale_g_since_first_cut = 100 * (g("wholesale", D2) / g("wholesale", D0) - 1),
         wholesale_share_now = 100 * g("wholesale", D2) / g("liabilities", D2),
         uninsured_share_first = 100 * g("uninsured_est", D0) / g("dep_domestic", D0), uninsured_share_now = 100 * g("uninsured_est", D2) / g("dep_domestic", D2))
}
# All firms in the panel (the memo scores the funding gap for the comparison banks too); Table 1 banks first
by_bank <- pf |> group_by(ticker) |> group_modify(~ pack(measures(.x))) |> ungroup() |> arrange(match(ticker, TABLE1), ticker)
by_group <- pf |> filter(!is.na(group)) |> group_by(group) |> group_modify(~ pack(measures(.x))) |> ungroup()
rnd <- function(d) d |> mutate(across(where(is.numeric), ~ round(.x, 1)))
readr::write_csv(rnd(by_bank), "data/analysis/availability_by_bank.csv")
readr::write_csv(rnd(by_group), "data/analysis/availability_by_group.csv")
print(as.data.frame(rnd(by_group)), row.names = FALSE)
print(as.data.frame(rnd(by_bank)), row.names = FALSE)
