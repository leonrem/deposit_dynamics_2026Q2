# Runoff pool, costly categories and capacity at 2026Q2 (Table 3 of the memo). No modeling: positions only.
#
# Extra Call Report items beyond the deposit panel, pulled for the pro forma bank set (today's banks plus banks merged in since 2019)
# and summed to the firm; combined with the pro forma firm panel for deposits, uninsured, brokered, time, FHLB and securities.
#   RCON HK11 + K222   time deposits with remaining maturity of one year or less ($250K or less; over $250K)   RC-E memo
#   RCFD/RCON 0010     cash and balances due from depository institutions                                        RC-A
#   RCFD/RCON 0416     securities pledged (book value)                                                          RC-B memo
#   RCFD/RCON 1772     AFS securities, amortized cost; 1773 AFS fair value (panel); 1754 HTM amortized cost; 1771 HTM fair value   RC-B
#   RCFA/RCOA P859     common equity tier 1 capital (bank-level, summed to the firm; not holding-company CET1)   RC-R
#   RCFA/RCOA P844     net unrealized gains (losses) on AFS securities removed from CET1 (nonzero = AOCI opt-out)  RC-R
# Consolidated items use RCFD where reported, else RCON. Missing items are NA, not zero.
# Usage: Rscript R/build_runoff_capacity.R        Writes: data/analysis/runoff_capacity_by_bank.csv

suppressMessages({library(dplyr); library(tidyr)})
setwd(if (file.exists("R/ffiec_data.R")) "." else "..")
source("R/build_deposit_panel.R")   # pro_forma_banks(); the main block is skipped when sourced

D2 <- as.Date("2026-06-30")
banks <- readr::read_csv("data/reference/deposit_study_banks.csv", show_col_types = FALSE)
pfb <- pro_forma_banks(banks)

consol <- c("0010", "0416", "1772", "1771", "1754", "1773")
codes <- c(paste0("RCFD", consol), paste0("RCON", consol), "RCONHK11", "RCONK222", "RCFAP859", "RCOAP859", "RCFAP844", "RCOAP844")
long <- ffiec_open("call_values") |>
  dplyr::filter(idrssd %in% pfb$idrssd, report_date == D2, item %in% codes) |>
  dplyr::select(idrssd, item, value) |> dplyr::collect()

# one value per bank and measure: RCFD first, RCON as the fallback; CET1 from whichever of RCFA/RCOA is reported
bank_vals <- long |>
  dplyr::mutate(measure = sub("^(RCFD|RCON|RCFA|RCOA)", "", item),
                pref = dplyr::case_when(startsWith(item, "RCFD") | startsWith(item, "RCFA") ~ 1L, TRUE ~ 2L)) |>
  dplyr::arrange(idrssd, measure, pref) |> dplyr::distinct(idrssd, measure, .keep_all = TRUE) |>
  dplyr::select(idrssd, measure, value) |>
  tidyr::pivot_wider(names_from = measure, values_from = value, names_prefix = "m_")
for (v in c("m_0010", "m_0416", "m_1772", "m_1771", "m_1754", "m_1773", "m_HK11", "m_K222", "m_P859", "m_P844")) if (!v %in% names(bank_vals)) bank_vals[[v]] <- NA_real_

sum_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
firm_extra <- pfb |> dplyr::distinct(idrssd, ticker) |> dplyr::left_join(bank_vals, by = "idrssd") |>
  dplyr::group_by(ticker) |>
  dplyr::summarise(cash = sum_na(m_0010), pledged = sum_na(m_0416), afs_ac = sum_na(m_1772), htm_fv = sum_na(m_1771),
                   htm_ac = sum_na(m_1754), afs_fv = sum_na(m_1773), cd_le250_le1y = sum_na(m_HK11), cd_gt250_le1y = sum_na(m_K222),
                   cet1 = sum_na(m_P859), aoci_afs_filter = sum_na(m_P844), .groups = "drop")

pf <- arrow::read_parquet("data/analysis/deposit_panel_firm_proforma.parquet") |> dplyr::filter(report_date == D2) |>
  dplyr::select(ticker, deposits, dep_domestic, time_deposits, uninsured_est, brokered, uninsured_share, brokered_share, cost_time, fhlb, fhlb_le1y, assets, loans, securities_htm, securities_afs)

out <- pf |> dplyr::left_join(firm_extra, by = "ticker") |>
  dplyr::mutate(
    cd_le1y_dollars = cd_le250_le1y + cd_gt250_le1y,
    cd_le1y_pct_dep = 100 * cd_le1y_dollars / dep_domestic,                 # CDs maturing within a year, % of domestic deposits
    cd_le1y_pct_cd  = 100 * cd_le1y_dollars / time_deposits,                # ... as a share of all CDs (the repricing wall)
    fhlb_pct_assets = 100 * fhlb / assets,
    fhlb_le1y_pct   = dplyr::if_else(fhlb > 0, 100 * fhlb_le1y / fhlb, NA_real_),
    cash_pct_dep    = 100 * cash / deposits,
    securities_book = htm_ac + afs_fv,
    unpledged_pct_dep = 100 * (securities_book - pledged) / deposits,       # pledged is book value, so this is approximate
    # runoff pool: uninsured + brokered + CDs of $250K or less due within a year (large CDs are mostly uninsured, so they are left out to limit double counting;
    # brokered CDs under $250K still sit in both, so the pool is an upper bound). Coverage = cash + unpledged securities over the pool.
    runoff_pool_pct_dep = 100 * (uninsured_est + brokered + cd_le250_le1y) / deposits,
    coverage_x = (cash + securities_book - pledged) / (uninsured_est + brokered + cd_le250_le1y),
    unrealized_net  = (afs_fv - afs_ac) + (htm_fv - htm_ac),                # negative = unrealized loss; $000
    unrealized_loss_pct_sec  = -100 * unrealized_net / securities_book,
    unrealized_loss_pct_cet1 = -100 * unrealized_net / cet1) |>
  dplyr::select(ticker, uninsured_pct = uninsured_share, brokered_pct = brokered_share, cd_le1y_pct_dep, cd_le1y_pct_cd, cd_cost = cost_time,
                runoff_pool_pct_dep, coverage_x, fhlb_pct_assets, fhlb_le1y_pct, cash_pct_dep, unpledged_pct_dep, unrealized_loss_pct_sec, unrealized_loss_pct_cet1, dplyr::everything())
readr::write_csv(out |> dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2))), "data/analysis/runoff_capacity_by_bank.csv")
print(as.data.frame(out |> dplyr::select(ticker:unrealized_loss_pct_cet1) |> dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 1)))), row.names = FALSE)
