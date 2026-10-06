# Shared metrics for the deposit-pressure study (betas, mix, positions, sensitivity), sourced by the memo
# (reports/2026_Q2_deposit_pressure_memo.Rmd) and by the exhibit scripts in R/. The memo overrides parts of the
# scoring (indicator set, 15 banks, percentile ranks) in its own setup chunk.
#
# Expects in the calling environment: p() (path builder from project root),
# up_start, up_end, down_end (Dates), shock (bp) and sod_year_structure (year). Defines: palette, FIRM_NAMES,
# panel, rates, sod_fy, sod_fm, betas, sens, fr, and formatting helpers.

library(dplyr)

# ---- palette: validated categorical slots (dataviz reference palette, light mode) ----
GROUP_COLS <- c("Focus regional" = "#2a78d6", "GSIB universal" = "#eb6834",
                "Smaller regional" = "#1baf7a", "Direct / card / brokerage" = "#eda100")
INK <- "#0b0b0b"; INK2 <- "#52514e"; GRID <- "#e6e5e1"

FIRM_NAMES <- c(
  CFG = "Citizens", FITB = "Fifth Third", FCNCA = "First Citizens", HBAN = "Huntington", KEY = "KeyCorp",
  MTB = "M&T", PNC = "PNC", RF = "Regions", TFC = "Truist", USB = "U.S. Bancorp",
  JPM = "JPMorgan", BAC = "Bank of America", C = "Citigroup", WFC = "Wells Fargo",
  ZION = "Zions", PNFP = "Pinnacle", EWBC = "East West", WBS = "Webster", CBSH = "Commerce", WAL = "Western Alliance",
  ONB = "Old National", COLB = "Columbia", VLY = "Valley", FNB = "F.N.B.", FHN = "First Horizon",
  SSB = "SouthState", ASB = "Associated", COF = "Capital One", AXP = "American Express",
  SYF = "Synchrony", ALLY = "Ally", SCHW = "Schwab", BK = "BNY", STT = "State Street",
  NTRS = "Northern Trust", GS = "Goldman Sachs", MS = "Morgan Stanley")

study <- readr::read_csv(p("data/reference/deposit_study_banks.csv"), show_col_types = FALSE)
matched <- study |> filter(!is.na(matched_to)) |> distinct(peer = ticker, focus = matched_to)

panel <- arrow::read_parquet(p("data/analysis/deposit_panel_firm.parquet")) |>
  mutate(grp = if_else(grepl("^Smaller regional", peer_group), "Smaller regional", peer_group),
         firm = unname(FIRM_NAMES[ticker]))
rates <- arrow::read_parquet(p("data/analysis/rates_quarterly.parquet")) |> filter(complete_quarter)
sod_fy <- arrow::read_parquet(p("data/analysis/sod_firm_year.parquet"))
sod_fm <- arrow::read_parquet(p("data/analysis/sod_firm_market.parquet"))

MAIN_GROUPS <- c("Focus regional", "GSIB universal", "Smaller regional")

# ---- cumulative betas: change in cost / change in average EFFR over a window ----
at <- function(d) panel |> filter(report_date == d)
cum_beta <- function(var, d0, d1) {
  a <- at(d0) |> select(ticker, c0 = all_of(var), e0 = effr_avg)
  b <- at(d1) |> select(ticker, c1 = all_of(var), e1 = effr_avg)
  inner_join(a, b, by = "ticker") |> transmute(ticker, beta = (c1 - c0) / (e1 - e0))
}
betas <- panel |> distinct(ticker, firm, grp) |>
  left_join(cum_beta("cost_total_deposits", up_start, up_end) |> rename(up_total = beta), by = "ticker") |>
  left_join(cum_beta("cost_total_deposits", up_end, down_end) |> rename(down_total = beta), by = "ticker") |>
  left_join(cum_beta("cost_ib_deposits", up_start, up_end) |> rename(up_ib = beta), by = "ticker") |>
  left_join(cum_beta("cost_ib_deposits", up_end, down_end) |> rename(down_ib = beta), by = "ticker")

e_up0 <- at(up_start)$effr_avg[1]; e_up1 <- at(up_end)$effr_avg[1]; e_dn1 <- at(down_end)$effr_avg[1]
med <- function(g, v) median(betas[[v]][betas$grp == g], na.rm = TRUE)
fmt2 <- function(x) formatC(x, format = "f", digits = 2)
fmt1 <- function(x) formatC(x, format = "f", digits = 1)
qlab <- function(d) paste0(format(d, "%Y"), "Q", lubridate::quarter(d))
fmt_mm <- function(x) formatC(round(x), format = "d", big.mark = ",")
# ---- +shock sensitivity (Section 6), computed here so the summary can cite it ----
lat <- at(down_end) |> select(ticker, firm, grp, avg_total_deposits, nii_q, ann, cost_total_deposits)
sens <- betas |> select(ticker, up_total, down_total) |> inner_join(lat, by = "ticker") |>
  filter(grp %in% c(MAIN_GROUPS, "Direct / card / brokerage")) |>
  mutate(beta_lo = pmin(up_total, down_total), beta_hi = pmax(up_total, down_total),
         d_cost_lo = beta_lo * shock, d_cost_hi = beta_hi * shock,                         # bp
         d_ie_lo = d_cost_lo / 1e4 * avg_total_deposits / 1e3,                            # $mm per year
         d_ie_hi = d_cost_hi / 1e4 * avg_total_deposits / 1e3,
         nii_ann = nii_q * ann / 1e3,                                                      # $mm per year
         pct_lo = 100 * d_ie_lo / nii_ann, pct_hi = 100 * d_ie_hi / nii_ann,
         grp = factor(grp, levels = names(GROUP_COLS))) |>
  arrange(grp, pct_hi)
fr <- sens |> filter(grp == "Focus regional")

# ---- market position, 2026 (pro forma, capped) ----
fy26 <- sod_fy |> filter(year == 2026, version == "capped", basis == "pro_forma")
fy19 <- sod_fy |> filter(year == 2019, version == "capped", basis == "pro_forma") |> select(ticker, dw19 = dw_market_share)
# Rank markets by deposits in retail branches (booking branches above the cap excluded),
# which locates the branch franchise; report the capped market share
fm26 <- sod_fm |> filter(year == 2026, basis == "pro_forma", version == "capped") |>
  select(ticker, market_id, market_name, firm_deposits_retail, share_capped = share)
top_mkt <- fm26 |> group_by(ticker) |> slice_max(firm_deposits_retail, n = 1, with_ties = FALSE) |>
  transmute(ticker, top_market = sub(",.*$", "", market_name), top_share = share_capped)
pos <- fy26 |> left_join(fy19, by = "ticker") |> left_join(top_mkt, by = "ticker") |>
  mutate(grp = if_else(grepl("^Smaller regional", peer_group), "Smaller regional", peer_group), firm = unname(FIRM_NAMES[ticker])) |>
  filter(grp %in% MAIN_GROUPS) |>
  left_join(matched, by = c("ticker" = "peer")) |>
  mutate(grp = factor(grp, levels = MAIN_GROUPS), match = if_else(is.na(focus), "", unname(FIRM_NAMES[focus]))) |>
  arrange(grp, desc(sod_deposits))

# ---- up-cycle betas vs. market structure (current basis, start of hiking cycle) and mix ----
struct <- sod_fy |> filter(year == sod_year_structure, version == "capped", basis == "current") |>
  select(ticker, dw_market_hhi, dw_market_share)
sc <- betas |> filter(grp %in% MAIN_GROUPS) |>
  left_join(struct, by = "ticker") |>
  left_join(at(up_start) |> select(ticker, nib0 = nib_share), by = "ticker") |>
  mutate(grp = factor(grp, levels = MAIN_GROUPS))

# ---- funding structure and YTD changes (pro forma, so 2026 acquisitions don't masquerade as growth) ----
ytd_start <- lubridate::floor_date(down_end, "year") - 1          # prior year-end (2025-12-31)
pf <- arrow::read_parquet(p("data/analysis/deposit_panel_firm_proforma.parquet")) |>
  mutate(grp = if_else(grepl("^Smaller regional", peer_group), "Smaller regional", peer_group))
pf_at <- function(d) pf |> filter(report_date == d)
ytd <- pf_at(ytd_start) |>
  select(ticker, ltd0 = loans_to_deposits, l0 = loans, d0 = deposits, n0 = nib, c0 = cost_total_deposits) |>
  inner_join(pf_at(down_end) |> select(ticker, ltd1 = loans_to_deposits, l1 = loans, d1 = deposits, n1 = nib,
                                      c1 = cost_total_deposits, merger_end = merger_in_quarter), by = "ticker") |>
  transmute(ticker, d_ltd = ltd1 - ltd0, loan_g = 100 * (l1 / l0 - 1), dep_g = 100 * (d1 / d0 - 1),
            ib_dep_g = 100 * ((d1 - n1) / (d0 - n0) - 1), d_cost_ytd_bp = 100 * (c1 - c0),
            merger_end = !is.na(merger_end))
funding <- at(down_end) |>
  select(ticker, firm, grp, nib1 = nib_share, dep_liab = deposits_to_liabilities, ltd = loans_to_deposits) |>
  left_join(at(up_start) |> select(ticker, nib0 = nib_share), by = "ticker") |>
  left_join(ytd, by = "ticker") |>
  filter(grp %in% MAIN_GROUPS) |>
  mutate(grp = factor(grp, levels = MAIN_GROUPS)) |> arrange(grp, desc(ltd))
# Which growth drives the YTD cost change? Excludes firms whose latest quarter contains a merger close.
ytd_ok <- funding |> filter(!merger_end, is.finite(d_cost_ytd_bp))
ytd_fit <- list(
  n = nrow(ytd_ok),
  r_ibdep = cor(ytd_ok$ib_dep_g, ytd_ok$d_cost_ytd_bp), r_loan = cor(ytd_ok$loan_g, ytd_ok$d_cost_ytd_bp),
  r_ltd = cor(ytd_ok$d_ltd, ytd_ok$d_cost_ytd_bp),
  slope_ibdep = coef(lm(d_cost_ytd_bp ~ ib_dep_g, data = ytd_ok))[[2]],
  p_ibdep = summary(lm(d_cost_ytd_bp ~ ib_dep_g, data = ytd_ok))$coefficients[2, 4]
)

# ---- management vs data (2026Q2 calls) ----
guide <- readr::read_csv(p("data/reference/mgmt_guidance_summary_2026Q2.csv"), show_col_types = FALSE)
cut_start <- as.Date("2024-06-30")   # managements measure cutting-cycle betas from 2Q24 average fed funds
ib_beta_cut <- at(cut_start) |> select(ticker, i0 = cost_ib_deposits, e0 = effr_avg) |>
  inner_join(at(down_end) |> select(ticker, i1 = cost_ib_deposits, e1 = effr_avg), by = "ticker") |>
  transmute(ticker, measured_ib_beta = 100 * (i1 - i0) / (e1 - e0))
mvd <- guide |> left_join(ib_beta_cut, by = "ticker") |> left_join(funding |> select(ticker, ib_dep_g, d_cost_ytd_bp), by = "ticker") |>
  mutate(firm = unname(FIRM_NAMES[ticker]))

# ---- deposit-pressure susceptibility score (focus regionals) ----
# Indicators were kept only where the data showed a link to deposit costs/betas:
#   Pillar A (deposit-base sensitivity): low NIB share (r = -0.45 with beta), own through-cycle
#     beta, time-deposit share, uninsured share.
#   Pillar B (funding need): loans/deposits, wholesale funding share, YTD interest-bearing
#     deposit growth (r = +0.47 with YTD cost change).
# Each indicator is z-scored across the ten focus banks, signed so higher = more pressure,
# averaged within pillar; score = 50/50 pillar average (decision 2026-10-02).
# Management tone and GSIB market share are shown but not scored (no measured link).
FOCUS_TICKERS <- c("PNC", "USB", "TFC", "FITB", "HBAN", "CFG", "RF", "MTB", "KEY", "FCNCA")
PRESSURE_INDICATORS <- tibble::tribble(
  ~var,        ~label,                         ~pillar, ~sign,
  "nib",       "NIB share of deposits (%)",    "A",     -1,
  "beta",      "Through-cycle beta",           "A",      1,
  "time",      "Time deposits (%)",            "A",      1,
  "unins",     "Uninsured (%)",                "A",      1,
  "ltd",       "Loans / deposits (%)",         "B",      1,
  "wholesale", "Wholesale funding (%)",        "B",      1,
  "ib_dep_g",  "YTD IB deposit growth (%)",    "B",      1
)
zs <- function(v) (v - mean(v, na.rm = TRUE)) / stats::sd(v, na.rm = TRUE)
pressure_inputs <- at(down_end) |>
  select(ticker, firm, grp, nib = nib_share, time = time_share, unins = uninsured_share,
         ltd = loans_to_deposits, depliab = deposits_to_liabilities) |>
  left_join(betas |> select(ticker, up_total, down_total), by = "ticker") |>
  left_join(funding |> select(ticker, ib_dep_g), by = "ticker") |>
  left_join(guide |> select(ticker, tone_q2), by = "ticker") |>
  left_join(sod_fy |> filter(year == max(year), version == "capped", basis == "pro_forma") |>
              select(ticker, gsib_mkt = dw_gsib_other_share), by = "ticker") |>
  mutate(beta = (up_total + down_total) / 2, wholesale = 100 - depliab)
pressure_z <- function(d, wA = 0.5) {
  for (i in seq_len(nrow(PRESSURE_INDICATORS))) {
    v <- PRESSURE_INDICATORS$var[i]
    d[[paste0("z_", v)]] <- PRESSURE_INDICATORS$sign[i] * zs(d[[v]])
  }
  zA <- paste0("z_", PRESSURE_INDICATORS$var[PRESSURE_INDICATORS$pillar == "A"])
  zB <- paste0("z_", PRESSURE_INDICATORS$var[PRESSURE_INDICATORS$pillar == "B"])
  d |> mutate(pillar_A = rowMeans(across(all_of(zA))), pillar_B = rowMeans(across(all_of(zB))),
              score = wA * pillar_A + (1 - wA) * pillar_B, rank = rank(-score, ties.method = "min"))
}
pressure <- pressure_inputs |> filter(ticker %in% FOCUS_TICKERS) |> pressure_z() |> arrange(rank)
# Rank robustness: pillar A only, pillar B only, 50/50, and equal weight on all seven indicators
pressure_robust <- pressure |> select(ticker, firm, rank_5050 = rank) |>
  left_join(pressure_inputs |> filter(ticker %in% FOCUS_TICKERS) |> pressure_z(wA = 1) |> select(ticker, rank_A = rank), by = "ticker") |>
  left_join(pressure_inputs |> filter(ticker %in% FOCUS_TICKERS) |> pressure_z(wA = 0) |> select(ticker, rank_B = rank), by = "ticker") |>
  left_join(pressure_inputs |> filter(ticker %in% FOCUS_TICKERS) |> pressure_z(wA = 4 / 7) |> select(ticker, rank_eq = rank), by = "ticker")
# Peer-group medians of the raw indicators, for reference rows
pressure_ref <- pressure_inputs |> filter(grp %in% c("GSIB universal", "Smaller regional")) |>
  group_by(grp) |> summarise(across(c(nib, beta, time, unins, ltd, wholesale, ib_dep_g, gsib_mkt), ~ median(.x, na.rm = TRUE)), .groups = "drop")
