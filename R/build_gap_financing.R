# How the funding gap was financed: the balance-sheet identity behind memo Table 1a and Appendix E.
#
# Funding gap = (change in loans - change in deposits) / starting deposits, first cut (2024Q3) to 2026Q2, pro forma for deals.
# Because assets = liabilities + equity and assets = loans + securities + cash and other assets:
#   change in loans - change in deposits = change in wholesale funding + change in equity - change in securities - change in goodwill - change in cash and other assets
#   wholesale funding = total liabilities - total deposits; equity = total assets - total liabilities (total equity capital, incl. noncontrolling interests)
#   securities = held-to-maturity at amortized cost + available-for-sale at fair value; goodwill = RC 3163; cash and other = total assets - loans - securities - goodwill
# All terms are expressed in points of starting deposits, so they add up to the gap exactly.
#
# Equity is split with Call Report Schedule RC items (consolidated RCFD where reported, else RCON), summed over the firm's banks:
#   retained earnings    RC 3632 (undivided profits and capital reserves)
#   AOCI                 RC B530 (accumulated other comprehensive income; includes unrealized gains and losses on AFS securities)
#   paid-in capital      RC 3838 perpetual preferred + 3230 common stock + 3839 surplus + A130 other components of equity capital
#   noncontrolling       RC G105 (total equity capital including noncontrolling interests) less 3210 (bank equity capital)
# These are bank-level figures: dividends paid up to the holding company reduce retained earnings, and capital the parent injects shows up in paid-in capital.
#
# Usage: Rscript R/build_gap_financing.R      Writes: data/analysis/gap_financing_by_bank.csv

suppressMessages({library(dplyr); library(tidyr)})
setwd(if (file.exists("R/ffiec_data.R")) "." else "..")
source("R/build_deposit_panel.R")   # pro_forma_banks(); the main block is skipped when sourced

D0 <- as.Date("2024-09-30"); D1 <- as.Date("2026-06-30")
TABLE1 <- c("PNFP", "EWBC", "FCNCA", "USB", "FHN", "TFC", "HBAN", "CFG", "PNC", "WAL", "KEY", "MTB", "ZION", "FITB", "RF")
banks <- readr::read_csv("data/reference/deposit_study_banks.csv", show_col_types = FALSE); pfb <- pro_forma_banks(banks)

items <- c("G105", "3210", "3632", "B530", "3230", "3839", "3838", "A130", "3163")
codes <- c(paste0("RCFD", items), paste0("RCON", items))
long <- ffiec_open("call_values") |>
  dplyr::filter(idrssd %in% pfb$idrssd, report_date %in% c(D0, D1), item %in% codes) |>
  dplyr::select(idrssd, report_date, item, value) |> dplyr::collect()
w <- long |> mutate(m = sub("^(RCFD|RCON)", "", item), pref = if_else(startsWith(item, "RCFD"), 1L, 2L)) |>
  arrange(idrssd, report_date, m, pref) |> distinct(idrssd, report_date, m, .keep_all = TRUE) |>
  select(idrssd, report_date, m, value) |> pivot_wider(names_from = m, values_from = value)
for (v in items) if (!v %in% names(w)) w[[v]] <- 0
eq <- pfb |> distinct(idrssd, ticker) |> inner_join(w, by = "idrssd") |> group_by(ticker, report_date) |>
  summarise(across(all_of(items), ~ sum(.x, na.rm = TRUE)), .groups = "drop") |>
  mutate(re = `3632`, aoci = B530, paid = `3838` + `3230` + `3839` + A130, nci = G105 - `3210`, eq_total = G105, goodwill = `3163`) |> select(ticker, report_date, re, aoci, paid, nci, eq_total, goodwill)

pf <- arrow::read_parquet("data/analysis/deposit_panel_firm_proforma.parquet") |> filter(ticker %in% TABLE1, report_date %in% c(D0, D1)) |>
  transmute(ticker, report_date, loans, deposits, liabilities, assets, wholesale = liabilities - deposits, equity = assets - liabilities,
            securities = securities_htm + securities_afs, other_assets = assets - loans - (securities_htm + securities_afs))
x <- pf |> inner_join(eq, by = c("ticker", "report_date")) |> mutate(cash_other = other_assets - goodwill)   # cash and other excludes goodwill
a <- x |> filter(report_date == D0); b <- x |> filter(report_date == D1) |> slice(match(a$ticker, ticker))
stopifnot(identical(a$ticker, b$ticker))
d <- function(v) b[[v]] - a[[v]]
out <- tibble::tibble(ticker = a$ticker,
  gap = 100 * (d("loans") - d("deposits")) / a$deposits, d_wholesale = 100 * d("wholesale") / a$deposits, d_equity = 100 * d("equity") / a$deposits,
  d_securities = 100 * d("securities") / a$deposits, d_goodwill = 100 * d("goodwill") / a$deposits, d_cash_other = 100 * d("cash_other") / a$deposits,
  eq_retained = 100 * d("re") / a$deposits, eq_aoci = 100 * d("aoci") / a$deposits, eq_paid_in = 100 * d("paid") / a$deposits, eq_noncontrolling = 100 * d("nci") / a$deposits,
  d_loans = 100 * d("loans") / a$deposits, d_deposits = 100 * d("deposits") / a$deposits)
chk1 <- max(abs(out$gap - (out$d_wholesale + out$d_equity - out$d_securities - out$d_goodwill - out$d_cash_other)))
chk2 <- max(abs(out$d_equity - (out$eq_retained + out$eq_aoci + out$eq_paid_in + out$eq_noncontrolling)))
message("max identity error (points): gap ", signif(chk1, 3), "; equity split ", signif(chk2, 3))
stopifnot(chk1 < 1e-6, chk2 < 0.05)
readr::write_csv(out |> mutate(across(where(is.numeric), ~ round(.x, 3))), "data/analysis/gap_financing_by_bank.csv")
print(as.data.frame(out |> mutate(across(where(is.numeric), ~ round(.x, 1))) |> arrange(desc(gap))), row.names = FALSE)
