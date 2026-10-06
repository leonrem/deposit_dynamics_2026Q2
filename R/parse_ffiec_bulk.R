# Parse FFIEC CDR bulk tab-delimited files into long Parquet tables.
#
# File layout (verified on 2026-06-30 Call and 2026 UBPR downloads):
#   Call:  row 1 = item codes ("IDRSSD", RCFD2170, ...), row 2 = descriptions,
#          then one row per bank. One file per schedule; wide schedules are split
#          into "(1 of 2)", "(2 of 2)". POR file = bank attributes.
#   UBPR:  row 1 = "Reporting Period", "ID RSSD", item codes (UBPR...),
#          rows 2-3 = short / long descriptions, then one row per bank-quarter.
#
# Parsing rules (why they exist):
#   - Records end in CRLF. Free-text fields (e.g. Schedule RI-E) contain bare LF
#     line breaks and stray double quotes, so we split on CRLF ourselves and read
#     with quoting disabled. A generic CSV reader would mis-split those rows.
#   - Every row must have the header's field count; otherwise we stop. One exception:
#     filers occasionally type TAB characters into free text (seen in NARR,
#     2022-12-31). If a file has exactly one TEXT column and a row has *extra*
#     fields, the surplus can only belong to that column (the other fields are
#     numeric/boolean), so it is rejoined and the tab preserved in value_text.
#   - Blank cells mean "not reported" and are dropped from the long table.
#   - Non-numeric cells (CONF, true/false, free text) go to value_text, value = NA.
#   - Percent strings ("9.2956%", Call Schedule RC-R ratios) -> value 9.2956.

library(dplyr)

LF_PLACEHOLDER  <- "\x1f"  # ASCII unit separator: stands in for embedded LF during parsing
TAB_PLACEHOLDER <- "\x1e"  # ASCII record separator: stands in for embedded TAB

# Split on TAB keeping trailing empty fields (base strsplit drops them)
split_tabs <- function(row) {
  utils::head(strsplit(paste0(row, "\tEND"), "\t", fixed = TRUE)[[1]], -1)
}

# Repair rows with surplus tabs when the file has exactly one TEXT column.
repair_text_tabs <- function(rows, n_tabs, path) {
  header <- split_tabs(rows[1])
  text_idx <- which(startsWith(gsub('"', "", header), "TEXT"))
  bad <- which(n_tabs != n_tabs[1])
  if (length(text_idx) != 1 || any(n_tabs[bad] < n_tabs[1])) {
    stop(basename(path), ": ", length(bad),
         " rows have a different field count than the header (not auto-repairable)")
  }
  n_fields <- length(header)
  for (i in bad) {
    f <- split_tabs(rows[i])
    n_extra <- length(f) - n_fields
    text_span <- text_idx:(text_idx + n_extra)
    merged <- paste(f[text_span], collapse = TAB_PLACEHOLDER)
    rows[i] <- paste(c(f[seq_len(text_idx - 1)], merged,
                       f[-seq_len(text_idx + n_extra)]), collapse = "\t")
  }
  message(basename(path), ": rejoined embedded tabs in ", length(bad),
          " row(s) into ", header[text_idx])
  rows
}

# Read one bulk .txt into a character data.table plus its item dictionary.
# n_desc_rows: number of description rows after the code row (Call 1, UBPR 2).
read_ffiec_txt <- function(path, n_desc_rows) {
  raw_txt <- readChar(path, file.size(path), useBytes = TRUE)
  rows <- strsplit(raw_txt, "\r\n", fixed = TRUE)[[1]]
  rows <- rows[nzchar(rows)]
  rows <- gsub("\n", LF_PLACEHOLDER, rows, fixed = TRUE)

  n_tabs <- lengths(regmatches(rows, gregexpr("\t", rows, fixed = TRUE)))
  if (any(n_tabs != n_tabs[1])) rows <- repair_text_tabs(rows, n_tabs, path)

  dt <- data.table::fread(
    text = paste(rows, collapse = "\n"), sep = "\t", quote = "",
    header = FALSE, colClasses = "character", na.strings = NULL,
    strip.white = FALSE, showProgress = FALSE
  )
  codes <- gsub('"', "", unlist(dt[1]), fixed = TRUE)
  desc <- lapply(seq_len(n_desc_rows), function(i) unlist(dt[1 + i]))
  dt <- dt[-seq_len(1 + n_desc_rows)]
  data.table::setnames(dt, codes)

  dict <- tibble::tibble(
    item = codes,
    description = if (n_desc_rows >= 1) desc[[1]] else NA_character_,
    description_long = if (n_desc_rows >= 2) desc[[2]] else NA_character_
  )
  list(data = dt, dict = dict)
}

# Wide character table -> long (id_cols..., item, value, value_text); blanks dropped.
ffiec_to_long <- function(dt, id_cols) {
  long <- data.table::melt(
    dt, id.vars = id_cols, variable.name = "item", value.name = "raw",
    variable.factor = FALSE
  )
  long <- long[nzchar(raw)]
  # Call Report ratio items arrive as "9.2956%": store as percent points (9.2956),
  # the same scale UBPR uses for its ratios
  long[, value := suppressWarnings(as.numeric(sub("%$", "", raw)))]
  long[, value_text := data.table::fifelse(
    is.na(value),
    gsub(TAB_PLACEHOLDER, "\t", gsub(LF_PLACEHOLDER, "\n", raw, fixed = TRUE), fixed = TRUE),
    NA_character_
  )]
  long[, raw := NULL]
  long[]
}

# Same (bank, date, item) can appear in more than one file. Keep one copy if the
# values agree; stop if they conflict.
dedupe_items <- function(long, key_cols) {
  dup <- long[, .N, by = key_cols][N > 1]
  if (nrow(dup) == 0) return(long)
  distinct_vals <- unique(long[dup, on = key_cols, c(key_cols, "value", "value_text"), with = FALSE])
  conflicts <- distinct_vals[, .N, by = key_cols][N > 1]
  if (nrow(conflicts) > 0) {
    print(utils::head(conflicts))
    stop(nrow(conflicts), " (bank, date, item) keys have conflicting values across files")
  }
  unique(long, by = key_cols)
}

unzip_to_temp <- function(zip_path) {
  exdir <- tempfile("ffiec_")
  utils::unzip(zip_path, exdir = exdir)
  list.files(exdir, pattern = "\\.txt$", full.names = TRUE)
}

# ---- Call Reports: one quarter ---------------------------------------------

build_call_quarter <- function(zip_path, parquet_dir) {
  files <- unzip_to_temp(zip_path)
  on.exit(unlink(dirname(files[1]), recursive = TRUE))

  date_str <- regmatches(basename(zip_path), regexpr("[0-9]{8}", basename(zip_path)))
  report_date <- as.Date(date_str, "%Y%m%d")

  sched_files <- files[grepl("Call Schedule", basename(files))]
  por_file <- files[grepl("Call Bulk POR", basename(files))]
  stopifnot(length(por_file) == 1, length(sched_files) > 0)

  parsed <- lapply(sched_files, function(f) {
    schedule <- sub("^FFIEC CDR Call Schedule (\\S+) .*$", "\\1", basename(f))
    p <- read_ffiec_txt(f, n_desc_rows = 1)
    list(
      long = ffiec_to_long(p$data, "IDRSSD"),
      dict = dplyr::filter(p$dict, item != "IDRSSD") |>
        dplyr::mutate(schedule = schedule)
    )
  })

  values <- data.table::rbindlist(lapply(parsed, `[[`, "long"))
  data.table::setnames(values, "IDRSSD", "idrssd")
  values[, idrssd := as.integer(idrssd)]
  values <- dedupe_items(values, c("idrssd", "item"))
  values[, report_date := report_date]
  data.table::setcolorder(values, c("idrssd", "report_date", "item", "value", "value_text"))

  # Dictionary: one row per item; schedules collapsed when an item appears in several
  dict <- dplyr::bind_rows(lapply(parsed, `[[`, "dict")) |>
    dplyr::group_by(item) |>
    dplyr::summarise(
      description = dplyr::first(description),
      schedule = paste(sort(unique(schedule)), collapse = ";"),
      .groups = "drop"
    ) |>
    dplyr::mutate(report_date = report_date)

  por <- read_ffiec_txt(por_file, n_desc_rows = 0)$data |>
    tibble::as_tibble() |>
    dplyr::rename(idrssd = IDRSSD) |>
    dplyr::mutate(idrssd = as.integer(idrssd), report_date = report_date)

  write_part(values, parquet_dir, "call_values", date_str)
  write_part(dict, parquet_dir, "call_dictionary", date_str)
  write_part(por, parquet_dir, "call_banks", date_str)

  tibble::tibble(
    product = "call", report_date = report_date, banks = dplyr::n_distinct(values$idrssd),
    items = nrow(dict), value_rows = nrow(values), source_zip = basename(zip_path)
  )
}

# ---- UBPR Ratio (four periods): one calendar year ---------------------------

build_ubpr_year <- function(zip_path, parquet_dir) {
  files <- unzip_to_temp(zip_path)
  on.exit(unlink(dirname(files[1]), recursive = TRUE))
  year_str <- regmatches(basename(zip_path), regexpr("[0-9]{4}", basename(zip_path)))
  files <- files[grepl("^FFIEC CDR UBPR Ratios", basename(files))]  # drops Readme.txt

  parsed <- lapply(files, function(f) {
    section <- sub("^FFIEC CDR UBPR Ratios (.*) [0-9]{4}\\.txt$", "\\1", basename(f))
    p <- read_ffiec_txt(f, n_desc_rows = 2)
    list(
      long = ffiec_to_long(p$data, c("Reporting Period", "ID RSSD")),
      dict = dplyr::filter(p$dict, !item %in% c("Reporting Period", "ID RSSD")) |>
        dplyr::mutate(section = section)
    )
  })

  values <- data.table::rbindlist(lapply(parsed, `[[`, "long"))
  data.table::setnames(values, c("Reporting Period", "ID RSSD"), c("period_raw", "idrssd"))
  values[, idrssd := as.integer(idrssd)]
  # "3/31/2026 11:59:59 PM" -> 2026-03-31
  values[, report_date := as.Date(sub(" .*$", "", period_raw), "%m/%d/%Y")]
  if (anyNA(values$report_date)) stop("Unparseable Reporting Period values in ", basename(zip_path))
  values[, period_raw := NULL]
  values <- dedupe_items(values, c("idrssd", "report_date", "item"))
  data.table::setcolorder(values, c("idrssd", "report_date", "item", "value", "value_text"))

  dict <- dplyr::bind_rows(lapply(parsed, `[[`, "dict")) |>
    dplyr::group_by(item) |>
    dplyr::summarise(
      description = dplyr::first(description),
      description_long = dplyr::first(description_long),
      section = paste(sort(unique(section)), collapse = ";"),
      .groups = "drop"
    ) |>
    dplyr::mutate(year = as.integer(year_str))

  write_part(values, parquet_dir, "ubpr_values", year_str)
  write_part(dict, parquet_dir, "ubpr_dictionary", year_str)

  tibble::tibble(
    product = "ubpr_ratio", year = as.integer(year_str),
    quarters = paste(sort(unique(format(values$report_date))), collapse = ","),
    banks = dplyr::n_distinct(values$idrssd), items = nrow(dict),
    value_rows = nrow(values), source_zip = basename(zip_path)
  )
}

# One Parquet file per period inside a table folder; arrow::open_dataset() on the
# folder reads them as a single table. Rebuilding a period overwrites its file.
write_part <- function(df, parquet_dir, table, tag) {
  out_dir <- file.path(parquet_dir, table)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(df, file.path(out_dir, paste0(table, "_", tag, ".parquet")),
                       compression = "zstd")
}
