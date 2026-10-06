# Firm-level quarterly deposit panel for the deposit-competition study.
#
# Bank-level Call Report dollars are summed to the firm (ticker) per quarter and
# ratios are recomputed from the sums -- bank ratios are never averaged.
#
# Inputs : data/parquet/call_values (full store), data/analysis/rates_quarterly.parquet,
#          data/reference/deposit_study_banks.csv (ticker, peer_group, idrssd, bank_name)
# Outputs: data/analysis/deposit_panel_bank.parquet, data/analysis/deposit_panel_firm.parquet
#
# Method notes (each is a choice; see README "Deposit panel"):
#   - RI income items are year-to-date; quarterly = YTD - prior-quarter YTD (Q1 = YTD).
#   - Annualization: quarterly flow x 365 / days in quarter (validated against UBPRE701 below).
#   - Cost of deposits uses BALANCE-SHEET two-point averages (beginning + end of quarter,
#     schedule RC): total deposits for cost_total_deposits, interest-bearing deposits for
#     cost_ib_deposits (decision 2026-10-02). RC-K averages are kept as *_rck columns (they
#     reproduce UBPR exactly) but are NOT used: banks classify zero-rate savings/MMDA
#     inconsistently between RC-K (interest-bearing) and RC (noninterest-bearing), which
#     double-counted those balances and understated costs and betas (JPM and BAC throughout,
#     MTB until a 2025Q1 reclassification). The balance-sheet basis matches management-
#     reported costs (TFC 1.55%, FCNCA 2.07%, 2026) and M&T's stated 56% beta.
#   - Product-level costs (savings, time, IB transaction) still use RC-K by necessity.
#   - Consolidated items: RCFD where reported, else RCON (041/051 filers). Foreign-office
#     items (RCFN, RIAD4172) treated as 0 when a bank reports none.

library(dplyr)
source("R/ffiec_data.R")

# item -> role. Consolidated items listed as RCFD; RCON fallback is added automatically.
DEPOSIT_ITEMS <- tibble::tribble(
  ~item,       ~var,                  ~kind,
  # Interest expense on deposits (YTD, $000)
  "RIAD4508",  "ie_transaction",      "ytd",
  "RIAD0093",  "ie_savings",          "ytd",
  "RIADHK03",  "ie_time_le250",       "ytd",
  "RIADHK04",  "ie_time_gt250",       "ytd",
  "RIAD4172",  "ie_foreign",          "ytd_foreign",
  "RIAD4073",  "ie_total",            "ytd",
  "RIAD4074",  "nii",                 "ytd",
  # Quarterly average balances (RC-K, $000)
  "RCON3485",  "avg_ib_transaction",  "stock",
  "RCONB563",  "avg_savings",         "stock",
  "RCONHK16",  "avg_time_le250",      "stock",
  "RCONHK17",  "avg_time_gt250",      "stock",
  "RCFN3404",  "avg_ib_foreign",      "stock_foreign",
  "RCFD3368",  "avg_assets",          "stock_consol",
  # Quarter-end balances ($000)
  "RCON2200",  "dep_domestic",        "stock",
  "RCFN2200",  "dep_foreign",         "stock_foreign",
  "RCON6631",  "nib_domestic",        "stock",
  "RCFN6631",  "nib_foreign",         "stock_foreign",
  "RCON6636",  "ib_domestic",         "stock",
  "RCFN6636",  "ib_foreign",          "stock_foreign",
  "RCON2215",  "transaction_accts",   "stock",
  "RCON2210",  "demand_deposits",     "stock",
  "RCON6810",  "mmda",                "stock",
  "RCON0352",  "other_savings",       "stock",
  "RCON6648",  "time_lt100",          "stock",
  "RCONJ473",  "time_100_250",        "stock",
  "RCONJ474",  "time_gt250",          "stock",
  "RCON2365",  "brokered",            "stock",
  "RCONJH83",  "reciprocal",          "stock",
  "RCON5597",  "uninsured_est",       "stock",
  "RCFD3190",  "other_borrowed",      "stock_consol",
  "RCFDF055",  "fhlb_le1y",           "stock_consol",
  "RCFDF056",  "fhlb_1to3y",          "stock_consol",
  "RCFDF057",  "fhlb_3to5y",          "stock_consol",
  "RCFDF058",  "fhlb_gt5y",           "stock_consol",
  "RCONB993",  "ff_purchased",        "stock",
  "RCFDB995",  "repo",                "stock_consol",
  "RCFD2122",  "loans",               "stock_consol",
  "RCFD1754",  "securities_htm",      "stock_consol",   # RC-B held-to-maturity, amortized cost
  "RCFD1773",  "securities_afs",      "stock_consol",   # RC-B available-for-sale, fair value
  "RCFD2170",  "assets",              "stock_consol",
  "RCFD2948",  "liabilities",         "stock_consol"
)

pull_bank_items <- function(ids) {
  consol <- DEPOSIT_ITEMS$item[DEPOSIT_ITEMS$kind == "stock_consol"]
  all_codes <- c(DEPOSIT_ITEMS$item, sub("^RCFD", "RCON", consol))
  long <- ffiec_open("call_values") |>
    dplyr::filter(idrssd %in% ids, item %in% all_codes) |>
    dplyr::select(idrssd, report_date, item, value) |>
    dplyr::collect()

  # RCFD preferred, RCON fallback, mapped to the RCFD code's variable name
  long <- long |>
    dplyr::mutate(base = dplyr::if_else(item %in% sub("^RCFD", "RCON", consol),
                                        sub("^RCON", "RCFD", item), item),
                  pref = dplyr::if_else(startsWith(item, "RCFD") | item == base, 1L, 2L)) |>
    dplyr::arrange(idrssd, report_date, base, pref) |>
    dplyr::distinct(idrssd, report_date, base, .keep_all = TRUE) |>
    dplyr::left_join(DEPOSIT_ITEMS, by = c("base" = "item"))

  wide <- long |>
    dplyr::select(idrssd, report_date, var, value) |>
    tidyr::pivot_wider(names_from = var, values_from = value)

  # Make sure every variable exists even if no bank reports it
  for (v in setdiff(DEPOSIT_ITEMS$var, names(wide))) wide[[v]] <- NA_real_
  foreign_vars <- DEPOSIT_ITEMS$var[DEPOSIT_ITEMS$kind %in% c("stock_foreign", "ytd_foreign")]
  wide |> dplyr::mutate(dplyr::across(dplyr::all_of(foreign_vars), ~ dplyr::coalesce(.x, 0)))
}

# YTD -> quarterly. Requires the bank's prior quarter in the same year.
deaccumulate <- function(df) {
  ytd_vars <- DEPOSIT_ITEMS$var[DEPOSIT_ITEMS$kind %in% c("ytd", "ytd_foreign")]
  df |>
    dplyr::arrange(idrssd, report_date) |>
    dplyr::group_by(idrssd, yr = lubridate::year(report_date)) |>
    dplyr::mutate(
      q = lubridate::quarter(report_date),
      prior_ok = q == 1 | dplyr::lag(q) == q - 1,
      dplyr::across(dplyr::all_of(ytd_vars),
                    ~ dplyr::if_else(q == 1, .x, .x - dplyr::lag(.x)), .names = "{.col}_q")
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("_q"), ~ dplyr::if_else(prior_ok %in% TRUE, .x, NA_real_))) |>
    dplyr::select(-yr, -prior_ok)
}

add_components <- function(df) {
  df |>
    dplyr::mutate(
      days_in_q = as.numeric(report_date - (lubridate::floor_date(report_date, "quarter") - 1)),
      ann = 365 / days_in_q,
      ie_deposits_q = ie_transaction_q + ie_savings_q + ie_time_le250_q + ie_time_gt250_q + ie_foreign_q,
      avg_ib_deposits = avg_ib_transaction + avg_savings + avg_time_le250 + avg_time_gt250 + avg_ib_foreign,
      deposits = dep_domestic + dep_foreign,
      nib = nib_domestic + nib_foreign,
      ib = ib_domestic + ib_foreign,
      time_deposits = time_lt100 + time_100_250 + time_gt250,
      # Savings, MMDA + NOW built from the reported line items: interest-bearing transaction accounts (RC-E 2215 less noninterest-bearing 6631),
      # money market (6810), other savings (0352) and foreign-office interest-bearing deposits (RCFN6636). Memo Table 2 uses this, not a remainder.
      sav_mmda_now = (transaction_accts - nib_domestic) + mmda + other_savings + ib_foreign,
      # Accounting identity: total deposits = noninterest-bearing + time + savings/MMDA/NOW; the gap should be zero up to rounding ($000)
      deposit_identity_gap = deposits - nib - time_deposits - sav_mmda_now,
      fhlb = rowSums(dplyr::across(c(fhlb_le1y, fhlb_1to3y, fhlb_3to5y, fhlb_gt5y)), na.rm = TRUE)
    )
}

# Ratios from (possibly summed) dollars; used at both bank and firm level
add_ratios <- function(df, group_col) {
  df |>
    dplyr::arrange(.data[[group_col]], report_date) |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::mutate(
      avg_nib_2pt = (nib + dplyr::lag(nib)) / 2,
      avg_ib_2pt = (ib + dplyr::lag(ib)) / 2,
      avg_total_deposits = (deposits + dplyr::lag(deposits)) / 2,
      avg_total_deposits_rck = avg_ib_deposits + avg_nib_2pt,
      dep_growth_qoq = 100 * (deposits / dplyr::lag(deposits) - 1),
      dep_growth_yoy = 100 * (deposits / dplyr::lag(deposits, 4) - 1)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      cost_ib_deposits = 100 * ie_deposits_q * ann / avg_ib_2pt,
      cost_total_deposits = 100 * ie_deposits_q * ann / avg_total_deposits,
      cost_ib_deposits_rck = 100 * ie_deposits_q * ann / avg_ib_deposits,           # = UBPRE701 basis
      cost_total_deposits_rck = 100 * ie_deposits_q * ann / avg_total_deposits_rck,
      cost_time = 100 * (ie_time_le250_q + ie_time_gt250_q) * ann / (avg_time_le250 + avg_time_gt250),
      cost_savings = 100 * ie_savings_q * ann / avg_savings,
      cost_ib_transaction = 100 * ie_transaction_q * ann / avg_ib_transaction,
      nib_share = 100 * nib / deposits,
      time_share = 100 * time_deposits / dep_domestic,
      mmda_share = 100 * mmda / dep_domestic,
      uninsured_share = 100 * uninsured_est /
        (if ("dep_domestic_unins_rptg" %in% names(.data)) dep_domestic_unins_rptg else dep_domestic),
      brokered_share = 100 * brokered / dep_domestic,
      loans_to_deposits = 100 * loans / deposits,
      # (loans + securities) / deposits: funding need once the securities book is counted (banking-book securities only)
      loans_sec_to_deposits = 100 * (loans + securities_htm + securities_afs) / deposits,
      deposits_to_liabilities = 100 * deposits / liabilities,
      fhlb_to_assets = 100 * fhlb / assets,
      deposits_to_assets = 100 * deposits / assets
    )
}

# Sum bank dollars to firm. A firm-quarter is complete only if every bank the firm
# has in that quarter reported all components (n_banks recorded for transparency).
sum_to_firm <- function(bank) {
  dollar_vars <- c(setdiff(grep("_q$", names(bank), value = TRUE), "days_in_q"),
                   DEPOSIT_ITEMS$var[!DEPOSIT_ITEMS$kind %in% c("ytd", "ytd_foreign")],
                   "ie_deposits_q", "avg_ib_deposits", "deposits", "nib", "ib", "time_deposits", "sav_mmda_now", "deposit_identity_gap", "fhlb")
  dollar_vars <- unique(intersect(dollar_vars, names(bank)))
  # RC-O uninsured estimate is not reported by small/uninsured trust banks: sum it
  # over reporting banks only and keep their domestic deposits as the denominator
  bank |>
    dplyr::mutate(dep_domestic_unins_rptg = dplyr::if_else(is.na(uninsured_est), 0, dep_domestic)) |>
    dplyr::group_by(ticker, peer_group, report_date, days_in_q, ann) |>
    dplyr::summarise(n_banks = dplyr::n(),
                     dplyr::across(dplyr::all_of(setdiff(dollar_vars, "uninsured_est")), ~ sum(.x)),
                     uninsured_est = sum(uninsured_est, na.rm = TRUE),
                     dep_domestic_unins_rptg = sum(dep_domestic_unins_rptg),
                     .groups = "drop") |>
    dplyr::mutate(uninsured_coverage = 100 * dep_domestic_unins_rptg / dep_domestic)
}

build_deposit_panel <- function(banks) {
  rates <- arrow::read_parquet("data/analysis/rates_quarterly.parquet") |>
    dplyr::filter(complete_quarter) |>
    dplyr::select(report_date, effr_avg, effr_end, target_upper_end, tbill_3m_avg)

  bank <- pull_bank_items(unique(banks$idrssd)) |>
    deaccumulate() |>
    add_components() |>
    dplyr::inner_join(dplyr::distinct(banks, idrssd, ticker, peer_group, bank_name), by = "idrssd")

  bank_ratios <- add_ratios(bank, "idrssd") |> dplyr::left_join(rates, by = "report_date")
  firm <- sum_to_firm(bank) |> add_ratios("ticker") |> dplyr::left_join(rates, by = "report_date") |>
    flag_merger_quarters()
  list(bank = bank_ratios, firm = firm)
}

# Quarters in which a bank merged into one of the firm's banks. Interest expense covers
# only part of such a quarter while two-point balance averages assume all of it, so
# quarter-on-quarter cost changes are unreliable there (e.g. PNC 2026Q2, FirstBank 6/18).
flag_merger_quarters <- function(firm) {
  ev <- readr::read_csv("data/reference/deposit_study_predecessors.csv", show_col_types = FALSE) |>
    dplyr::filter(!is.na(idrssd)) |>
    dplyr::mutate(report_date = lubridate::ceiling_date(event_date, "quarter") - 1) |>
    dplyr::group_by(ticker, report_date) |>
    dplyr::summarise(merger_in_quarter = paste(unique(bank_name), collapse = "; "), .groups = "drop")
  firm |> dplyr::left_join(ev, by = c("ticker", "report_date"))
}

# Pro forma firm panel: today's banks plus banks merged into them since 2019 (from
# deposit_study_predecessors.csv; failed-bank acquisitions excluded), attributed to
# today's firm in every quarter. Used for changes that span an acquisition (e.g. YTD
# loans/deposits for FITB-Comerica, HBAN-Cadence, PNC-FirstBank, PNFP-Synovus).
pro_forma_banks <- function(banks) {
  preds <- readr::read_csv("data/reference/deposit_study_predecessors.csv", show_col_types = FALSE) |>
    dplyr::filter(!is.na(idrssd), ticker %in% banks$ticker) |>
    dplyr::distinct(idrssd, .keep_all = TRUE) |>
    dplyr::left_join(dplyr::distinct(banks, ticker, peer_group), by = "ticker") |>
    dplyr::transmute(ticker, idrssd, bank_name, peer_group)
  dplyr::bind_rows(dplyr::select(banks, ticker, idrssd, bank_name, peer_group),
                   dplyr::filter(preds, !idrssd %in% banks$idrssd))
}

# Deposit identity check: the three buckets built from line items must add to total deposits (memo Table 2 shares sum to 100%).
# Tolerance 0.001 percentage points of deposits (the gap is rounding in $000); checked for 2021Q4 onward, the window the memo uses.
check_deposit_identity <- function(firm, label, from = as.Date("2021-12-31"), tol_pts = 1e-3) {
  g <- firm |> dplyr::filter(report_date >= from) |> dplyr::mutate(pts = 100 * abs(deposit_identity_gap) / deposits)
  message(label, ": largest deposit-identity gap ", signif(max(g$pts, na.rm = TRUE), 3), " pts of deposits over ", nrow(g), " firm-quarters; missing: ", sum(is.na(g$pts)))
  stopifnot(all(is.na(g$pts) | g$pts < tol_pts))
}

if (sys.nframe() == 0) {
  banks <- readr::read_csv("data/reference/deposit_study_banks.csv", show_col_types = FALSE)
  out <- build_deposit_panel(banks)
  check_deposit_identity(out$firm, "current structure")
  arrow::write_parquet(out$bank, "data/analysis/deposit_panel_bank.parquet")
  arrow::write_parquet(out$firm, "data/analysis/deposit_panel_firm.parquet")
  pf <- build_deposit_panel(pro_forma_banks(banks))
  check_deposit_identity(pf$firm, "pro forma")
  arrow::write_parquet(pf$firm, "data/analysis/deposit_panel_firm_proforma.parquet")
  message("bank rows: ", nrow(out$bank), "  firm rows: ", nrow(out$firm),
          "  firms: ", dplyr::n_distinct(out$firm$ticker))
}
