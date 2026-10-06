# Download policy and money-market rates and build quarterly tables for
# deposit-beta work.
#
# Sources (FRED's CSV endpoint blocks scripted clients, so we go to the publishers):
#   NY Fed Markets API -- Effective Federal Funds Rate (EFFR) + target range, business days.
#     The NY Fed administers EFFR; FRED's DFF republishes it.
#   U.S. Treasury daily bill rates -- 13-week bill, coupon-equivalent yield (an annual
#     yield comparable to a deposit rate, unlike the bank-discount quote FRED's DTB3 uses).
#
# Quarterly averages are calendar-day weighted: each business-day rate is carried
# over the following weekend/holiday, matching how interest accrues and FRED's DFF
# method. Call Report deposit costs are quarterly flows over average balances, so
# the quarterly *average* rate is the right beta denominator, not the quarter-end.
#
# Usage: Rscript R/fetch_rates.R -> data/raw/rates/*, data/analysis/rates_quarterly.parquet

library(dplyr)

RATES_START <- as.Date("2018-01-01")
RATES_END   <- Sys.Date()
RAW_DIR     <- "data/raw/rates"

provenance_row <- function(path, source_url) {
  tibble::tibble(file = path, source_url = source_url, bytes = file.size(path),
                 md5 = unname(tools::md5sum(path)), retrieved_at = format(Sys.time()))
}

fetch_effr <- function() {
  url <- sprintf("https://markets.newyorkfed.org/api/rates/unsecured/effr/search.json?startDate=%s&endDate=%s",
                 RATES_START, RATES_END)
  dest <- file.path(RAW_DIR, "nyfed_effr.json")
  httr2::request(url) |> httr2::req_retry(max_tries = 3) |> httr2::req_perform(path = dest)
  provenance_row(dest, url)
}

fetch_tbill_year <- function(year) {
  url <- sprintf(paste0("https://home.treasury.gov/resource-center/data-chart-center/interest-rates/",
                        "daily-treasury-rates.csv/%d/all?type=daily_treasury_bill_rates",
                        "&field_tdr_date_value=%d&page&_format=csv"), year, year)
  dest <- file.path(RAW_DIR, sprintf("treasury_bill_rates_%d.csv", year))
  httr2::request(url) |>
    httr2::req_user_agent("Mozilla/5.0 (R httr2; public Treasury download)") |>
    httr2::req_retry(max_tries = 3) |> httr2::req_perform(path = dest)
  provenance_row(dest, url)
}

read_effr <- function(path) {
  x <- jsonlite::fromJSON(path)$refRates
  tibble::tibble(date = as.Date(x$effectiveDate), effr = x$percentRate,
                 target_lower = x$targetRateFrom, target_upper = x$targetRateTo)
}

read_tbill <- function(paths) {
  purrr::map_dfr(paths, function(p) {
    readr::read_csv(p, col_types = readr::cols(.default = "c"), show_col_types = FALSE) |>
      dplyr::transmute(date = as.Date(Date, "%m/%d/%Y"),
                       tbill_3m = as.numeric(`13 WEEKS COUPON EQUIVALENT`))
  })
}

# Carry each business-day value forward over non-business days, then average by quarter
to_quarterly <- function(daily) {
  cal <- tibble::tibble(date = seq(min(daily$date), max(daily$date), by = "day"))
  cal |>
    dplyr::left_join(daily, by = "date") |>
    tidyr::fill(-date, .direction = "down") |>
    dplyr::mutate(report_date = lubridate::ceiling_date(date, "quarter") - 1) |>
    dplyr::group_by(report_date) |>
    dplyr::summarise(
      dplyr::across(-date, list(avg = ~ mean(.x, na.rm = TRUE), end = ~ dplyr::last(.x)),
                    .names = "{.col}_{.fn}"),
      days = dplyr::n(), last_date = max(date), .groups = "drop"
    ) |>
    dplyr::mutate(complete_quarter = last_date == report_date)
}

if (sys.nframe() == 0) {
  dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
  prov <- dplyr::bind_rows(
    fetch_effr(),
    purrr::map_dfr(seq(as.integer(format(RATES_START, "%Y")), as.integer(format(RATES_END, "%Y"))),
                   fetch_tbill_year)
  )
  readr::write_csv(prov, file.path(RAW_DIR, "provenance_rates.csv"))

  effr <- read_effr(prov$file[grepl("effr", prov$file)])
  tbill <- read_tbill(prov$file[grepl("treasury", prov$file)])
  if (anyDuplicated(effr$date) || anyDuplicated(tbill$date)) stop("Duplicate dates in rate data")
  daily <- dplyr::full_join(effr, tbill, by = "date") |> dplyr::arrange(date)

  q <- to_quarterly(daily)
  dir.create("data/analysis", recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(daily, "data/analysis/rates_daily.parquet")
  arrow::write_parquet(q, "data/analysis/rates_quarterly.parquet")
  print(dplyr::select(q, report_date, effr_avg, effr_end, target_upper_end, tbill_3m_avg, complete_quarter),
        n = 40)
}
