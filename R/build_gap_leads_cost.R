# Does a bank's funding gap lead its cost relative to peers? Back-test behind the "need to gather" score (memo Table 1a).
#
# For each of the 15 Table 1a banks (pro forma firm panel) at a window end t:
#   gap   = (change in loans less change in deposits) over the four quarters to t, in points of starting deposits (the Table 1a definition)
#   outcome = change over the next four quarters in the bank's interest-bearing cost relative to the 15-bank median that quarter (bp)
# Both are demeaned by quarter. Windows do not overlap: December year-ends 2020-2024 and June 2021-2025 (75 bank-observations each).
#
# Usage: Rscript R/build_gap_leads_cost.R      Writes: data/analysis/gap_leads_cost.csv

suppressMessages({library(dplyr); library(tidyr)})
setwd(if (file.exists("R/ffiec_data.R")) "." else "..")
TABLE1 <- c("PNFP", "EWBC", "FCNCA", "USB", "FHN", "TFC", "HBAN", "CFG", "PNC", "WAL", "KEY", "MTB", "ZION", "FITB", "RF")
d <- arrow::read_parquet("data/analysis/deposit_panel_firm_proforma.parquet") |> filter(ticker %in% TABLE1) |> arrange(ticker, report_date) |>
  group_by(ticker) |>
  mutate(cost = 100 * cost_ib_deposits, gap4 = 100 * ((loans - lag(loans, 4)) - (deposits - lag(deposits, 4))) / lag(deposits, 4)) |> ungroup() |>
  group_by(report_date) |> mutate(rel = cost - median(cost)) |> ungroup() |> group_by(ticker) |>
  mutate(d_rel_f4 = lead(rel, 4) - rel) |> ungroup()
win <- function(label, month, from, to) {
  x <- d |> filter(as.integer(format(report_date, "%m")) == month, report_date >= from, report_date <= to, !is.na(gap4), !is.na(d_rel_f4)) |>
    group_by(report_date) |> mutate(g = gap4 - mean(gap4), y = d_rel_f4 - mean(d_rel_f4)) |> ungroup()
  tibble::tibble(window = label, n = nrow(x), corr = cor(x$g, x$y), slope_bp_per_pt = sum(x$g * x$y) / sum(x$g^2))
}
out <- bind_rows(win("December year-ends 2020-2024", 12, as.Date("2020-12-31"), as.Date("2024-12-31")),
                 win("June year-ends 2021-2025", 6, as.Date("2021-06-30"), as.Date("2025-06-30")))
readr::write_csv(out |> mutate(across(where(is.numeric), ~ round(.x, 2))), "data/analysis/gap_leads_cost.csv")
print(as.data.frame(out), row.names = FALSE)
