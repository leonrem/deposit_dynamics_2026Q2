# Download FFIEC bulk files and (re)build the Parquet store.
#
# Usage (from the project root):
#   Rscript R/build_ffiec_store.R                 # default window below
#   Rscript R/build_ffiec_store.R 2021 2026       # first_year last_year
#
# Call Reports are pulled quarter by quarter; UBPR Ratio by calendar year (four
# quarters per file). Raw zips are kept unmodified in data/raw/; downloads skip
# files already present, so re-running only fetches what's new. The current
# year's UBPR file grows during the year: delete it (or pass overwrite) to refresh.

library(dplyr)
source("R/fetch_ffiec_bulk.R")
source("R/parse_ffiec_bulk.R")

args <- commandArgs(trailingOnly = TRUE)
first_year <- if (length(args) >= 1) as.integer(args[1]) else 2021L
last_year  <- if (length(args) >= 2) as.integer(args[2]) else as.integer(format(Sys.Date(), "%Y"))

raw_call <- "data/raw/call"
raw_ubpr <- "data/raw/ubpr"
parquet_dir <- "data/parquet"
provenance_file <- "data/provenance_downloads.csv"
build_log_file <- "data/provenance_builds.csv"

append_csv <- function(df, path) {
  readr::write_csv(df, path, append = file.exists(path))
}

# Rebuild a period only if its Parquet is missing or older than its source zip,
# so a re-run after a failure resumes instead of starting over.
needs_build <- function(zip_file, table, tag) {
  out <- file.path(parquet_dir, table, paste0(table, "_", tag, ".parquet"))
  !file.exists(out) || file.mtime(out) < file.mtime(zip_file)
}

# ---- Call Reports: every offered quarter-end inside the window ---------------
call_periods <- ffiec_list_periods("call")$periods |>
  dplyr::mutate(date = as.Date(label, "%m/%d/%Y")) |>
  dplyr::filter(as.integer(format(date, "%Y")) >= first_year,
                as.integer(format(date, "%Y")) <= last_year) |>
  dplyr::arrange(date)

for (d in as.character(call_periods$date)) {
  prov <- fetch_ffiec_bulk("call", d, raw_call)
  append_csv(dplyr::mutate(prov, retrieved_at = format(retrieved_at)), provenance_file)
  if (!needs_build(prov$file, "call_values", format(as.Date(d), "%Y%m%d"))) next
  stats <- build_call_quarter(prov$file, parquet_dir)
  append_csv(dplyr::mutate(stats, built_at = format(Sys.time())), build_log_file)
  message(sprintf("call %s: %d banks, %d rows", d, stats$banks, stats$value_rows))
  Sys.sleep(2)  # be polite to the FFIEC server
}

# ---- UBPR Ratio: one file per year -----------------------------------------
for (yr in first_year:last_year) {
  prov <- fetch_ffiec_bulk("ubpr_ratio", yr, raw_ubpr)
  append_csv(dplyr::mutate(prov, retrieved_at = format(retrieved_at)), provenance_file)
  if (!needs_build(prov$file, "ubpr_values", yr)) next
  stats <- build_ubpr_year(prov$file, parquet_dir)
  append_csv(dplyr::mutate(stats, built_at = format(Sys.time())), build_log_file)
  message(sprintf("ubpr %d: quarters %s, %d banks, %d rows",
                  yr, stats$quarters, stats$banks, stats$value_rows))
  Sys.sleep(2)
}
