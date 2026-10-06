# Download bulk files from the FFIEC CDR Public Data Distribution site.
#
# The bulk page is an ASP.NET WebForm: every interaction is a POST that must
# echo back the hidden __VIEWSTATE fields from the previous response. The flow is
#   1. GET the page                         -> initial view state
#   2. POST "select product" (postback)     -> page with that product's period list
#   3. POST "Download" with chosen period   -> zip file in the response body
# Period dropdown values are internal ids (e.g. "152"), so we map them from the
# visible label ("06/30/2026", or "2026" for four-period products) rather than
# hard-coding ids.
#
# Format note: UBPR Ratio -- Single Period is XBRL-only on the site (tab-delimited
# is disabled). UBPR Ratio -- Four Periods is tab-delimited, one file per year, so
# that is the UBPR product used here.

library(dplyr)

FFIEC_BULK_URL <- "https://cdr.ffiec.gov/public/PWS/DownloadBulkData.aspx"

# Friendly names -> the ListBox1 option values on the page
FFIEC_PRODUCTS <- c(
  call       = "ReportingSeriesSinglePeriod",            # period = quarter-end date
  ubpr_ratio = "PerformanceReportingSeriesFourPeriods"   # period = calendar year
)

# Pull every hidden <input> (view state etc.) out of a page as a named list
ffiec_hidden_fields <- function(html) {
  inputs <- xml2::xml_find_all(html, "//input[@type='hidden']")
  vals <- as.list(xml2::xml_attr(inputs, "value"))
  vals[is.na(vals)] <- ""
  stats::setNames(vals, xml2::xml_attr(inputs, "name"))
}

ffiec_post <- function(cookie_path, fields) {
  httr2::request(FFIEC_BULK_URL) |>
    httr2::req_user_agent("Mozilla/5.0 (R httr2; public bulk data download)") |>
    httr2::req_cookie_preserve(cookie_path) |>
    httr2::req_body_form(!!!fields) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_timeout(600) |>
    httr2::req_perform()
}

# Return a tibble of available periods (label + internal id) for a product.
# Also returns the page so the caller can reuse its view state.
ffiec_list_periods <- function(product, cookie_path = tempfile()) {
  stopifnot(product %in% names(FFIEC_PRODUCTS))

  landing <- httr2::request(FFIEC_BULK_URL) |>
    httr2::req_user_agent("Mozilla/5.0 (R httr2; public bulk data download)") |>
    httr2::req_cookie_preserve(cookie_path) |>
    httr2::req_perform() |>
    httr2::resp_body_html()

  fields <- ffiec_hidden_fields(landing)
  fields[["__EVENTTARGET"]] <- "ctl00$MainContentHolder$ListBox1"
  fields[["ctl00$MainContentHolder$ListBox1"]] <- FFIEC_PRODUCTS[[product]]
  fields[["ctl00$MainContentHolder$FormatType"]] <- "TSVRadioButton"

  page <- ffiec_post(cookie_path, fields) |> httr2::resp_body_html()
  opts <- xml2::xml_find_all(page, "//select[@id='DatesDropDownList']/option")

  list(
    periods = tibble::tibble(
      label = trimws(xml2::xml_text(opts)),
      id    = xml2::xml_attr(opts, "value")
    ),
    page = page,
    cookie_path = cookie_path
  )
}

# Convert a period to the dropdown's label: "06/30/2026" for quarterly products,
# "2026" for four-period (annual) products.
ffiec_period_label <- function(product, period) {
  if (product == "ubpr_ratio") return(as.character(as.integer(period)))
  format(as.Date(period), "%m/%d/%Y")
}

# Download one period of one product (tab-delimited) into dest_dir.
# period: quarter-end date for "call", calendar year for "ubpr_ratio".
# Returns a one-row provenance tibble. Skips the download if the file exists.
fetch_ffiec_bulk <- function(product, period, dest_dir, overwrite = FALSE) {
  label <- ffiec_period_label(product, period)
  file_tag <- if (product == "ubpr_ratio") label else format(as.Date(period), "%Y%m%d")
  dest_file <- file.path(dest_dir, sprintf("ffiec_%s_%s.zip", product, file_tag))
  if (file.exists(dest_file) && !overwrite) {
    message("Exists, skipping: ", dest_file)
  } else {
    listing <- ffiec_list_periods(product)
    match_row <- dplyr::filter(listing$periods, .data$label == .env$label)
    if (nrow(match_row) != 1) {
      stop("Period ", label, " not offered for ", product,
           ". Latest available: ", listing$periods$label[1])
    }

    fields <- ffiec_hidden_fields(listing$page)
    fields[["__EVENTTARGET"]] <- ""
    fields[["ctl00$MainContentHolder$ListBox1"]] <- FFIEC_PRODUCTS[[product]]
    fields[["ctl00$MainContentHolder$DatesDropDownList"]] <- match_row$id
    fields[["ctl00$MainContentHolder$FormatType"]] <- "TSVRadioButton"
    fields[["ctl00$MainContentHolder$TabStrip1$Download_0"]] <- "Download"

    resp <- ffiec_post(listing$cookie_path, fields)
    ctype <- httr2::resp_content_type(resp)
    if (!grepl("zip|octet-stream", ctype)) {
      stop("Expected a zip, got content-type '", ctype, "' for ", product, " ", period)
    }
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
    writeBin(httr2::resp_body_raw(resp), dest_file)
    message("Saved ", dest_file)
  }

  tibble::tibble(
    product      = product,
    period       = label,
    file         = dest_file,
    bytes        = file.size(dest_file),
    md5          = unname(tools::md5sum(dest_file)),
    source_url   = FFIEC_BULK_URL,
    retrieved_at = file.mtime(dest_file)
  )
}
