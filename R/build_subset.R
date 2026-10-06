# Cut a bank subset out of the full Parquet store: every Call and UBPR item, all
# quarters, for a list of IDRSSDs. Writes long Parquet (lossless) and wide RDS.
#
# Usage: Rscript R/build_subset.R
#   input : data/reference/stress_test_bank_universe.csv (from build_bank_universe.R)
#   output: data/subsets/stress_domestic24_pnfp_zion/

library(dplyr)
source("R/ffiec_data.R")

# Wide = one row per bank-quarter, one column per item, numeric values only.
# Items that are text-only for these banks (CONF, true/false, free text) are
# absent from wide; they remain in the long Parquet's value_text.
to_wide <- function(long, banks) {
  long |>
    dplyr::filter(!is.na(value)) |>
    dplyr::select(idrssd, report_date, item, value) |>
    tidyr::pivot_wider(names_from = item, values_from = value) |>
    dplyr::left_join(dplyr::select(banks, idrssd, ticker, bank_name, relationship, event_date),
                     by = "idrssd") |>
    dplyr::relocate(ticker, idrssd, bank_name, relationship, event_date, report_date) |>
    dplyr::arrange(ticker, idrssd, report_date)
}

build_subset <- function(banks, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  ids <- unique(banks$idrssd)

  call_long <- ffiec_open("call_values") |> dplyr::filter(idrssd %in% ids) |> dplyr::collect()
  ubpr_long <- ffiec_open("ubpr_values") |> dplyr::filter(idrssd %in% ids) |> dplyr::collect()

  arrow::write_parquet(call_long, file.path(out_dir, "call_values.parquet"), compression = "zstd")
  arrow::write_parquet(ubpr_long, file.path(out_dir, "ubpr_values.parquet"), compression = "zstd")
  saveRDS(to_wide(call_long, banks), file.path(out_dir, "call_wide.rds"))
  saveRDS(to_wide(ubpr_long, banks), file.path(out_dir, "ubpr_wide.rds"))
  readr::write_csv(banks, file.path(out_dir, "banks.csv"))

  tibble::tibble(
    table = c("call", "ubpr"),
    banks = c(dplyr::n_distinct(call_long$idrssd), dplyr::n_distinct(ubpr_long$idrssd)),
    bank_quarters = c(nrow(dplyr::distinct(call_long, idrssd, report_date)),
                      nrow(dplyr::distinct(ubpr_long, idrssd, report_date))),
    items = c(dplyr::n_distinct(call_long$item), dplyr::n_distinct(ubpr_long$item)),
    long_rows = c(nrow(call_long), nrow(ubpr_long))
  )
}

if (sys.nframe() == 0) {
  universe <- readr::read_csv("data/reference/stress_test_bank_universe.csv", show_col_types = FALSE)
  # Current structure only: banks each firm owns today, with their full history.
  # Merged predecessors / failed-bank acquisitions / former subsidiaries stay in
  # the reference table but are left out of the subset (decision 2026-10-01).
  banks <- dplyr::filter(universe, n_quarters > 0,
                         relationship %in% c("current subsidiary", "current subsidiary (uninsured)"))
  message("Current subsidiaries with Call data: ", nrow(banks), " of ", nrow(universe), " in universe")
  print(build_subset(banks, "data/subsets/stress_domestic24_pnfp_zion"))
}
