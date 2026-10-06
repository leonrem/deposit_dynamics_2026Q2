# FDIC Summary of Deposits (SOD): every insured branch's deposits as of June 30,
# with location, for 2019-2026. Pulled for ALL banks so market totals are complete.
#
# Source: FDIC BankFind API /banks/sod (public, no key), paged 10,000 rows per call.
# Kept fields: identity (bank RSSD, CERT, point-in-time top holder RSSDHCR), branch
# deposits (DEPSUMBR, $000), location (state, county FIPS, CBSA), main-office flag
# (BKMO) and branch service type (BRSERTYP). MSABR is the CBSA code (metro or
# micro, per METROBR/MICROBR flags; 0 = outside any CBSA). The API silently ignores
# unknown field names, so CBSABR/CBSANAMB are not used.
#
# Usage: Rscript R/fetch_sod.R -> data/raw/sod/sod_<year>.csv, data/raw/sod/provenance_sod.csv

library(dplyr)

SOD_YEARS <- 2019:2026
SOD_DIR <- "data/raw/sod"
SOD_FIELDS <- c("YEAR", "RSSDID", "CERT", "NAMEFULL", "RSSDHCR", "NAMEHCR", "UNINUMBR", "BRNUM",
                "BKMO", "BRSERTYP", "DEPSUMBR", "STALPBR", "STCNTYBR", "CNTYNAMB",
                "MSABR", "MSANAMB", "METROBR", "MICROBR", "CITYBR")

fetch_sod_year <- function(year, page_size = 10000) {
  pages <- list(); offset <- 0; total <- Inf
  while (offset < total) {
    resp <- httr2::request("https://api.fdic.gov/banks/sod") |>
      httr2::req_url_query(filters = sprintf("YEAR:%d", year), fields = paste(SOD_FIELDS, collapse = ","),
                           limit = page_size, offset = offset, sort_by = "UNINUMBR", sort_order = "ASC",
                           format = "json") |>
      httr2::req_retry(max_tries = 4) |>
      httr2::req_perform() |>
      httr2::resp_body_json(simplifyVector = TRUE)
    total <- resp$meta$total
    pages[[length(pages) + 1]] <- tibble::as_tibble(resp$data$data)
    offset <- offset + page_size
    Sys.sleep(0.3)
  }
  out <- dplyr::bind_rows(pages)
  if (nrow(out) != total) stop("SOD ", year, ": got ", nrow(out), " rows, API reports ", total)
  if (anyDuplicated(out$UNINUMBR)) stop("SOD ", year, ": duplicate branch ids across pages")
  dest <- file.path(SOD_DIR, sprintf("sod_%d.csv", year))
  readr::write_csv(out, dest)
  tibble::tibble(year = year, rows = nrow(out), api_total = total, file = dest,
                 md5 = unname(tools::md5sum(dest)), retrieved_at = format(Sys.time()))
}

if (sys.nframe() == 0) {
  dir.create(SOD_DIR, recursive = TRUE, showWarnings = FALSE)
  prov <- purrr::map_dfr(SOD_YEARS, function(y) { message("SOD ", y); fetch_sod_year(y) })
  readr::write_csv(prov, file.path(SOD_DIR, "provenance_sod.csv"))
  print(prov)
}
