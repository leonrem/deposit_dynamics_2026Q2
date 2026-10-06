# Analysis-side access to the Parquet store built by parse_ffiec_bulk.R.
#
# Tables (each a folder of per-period Parquet files under data/parquet/):
#   call_values      idrssd, report_date, item, value, value_text   (Call Report, $000s)
#   call_dictionary  item, description, schedule, report_date
#   call_banks       Panel of Reporters: name, city, state, cert, filing type, ...
#   ubpr_values      idrssd, report_date, item, value, value_text   (UBPR; ratios in percent)
#   ubpr_dictionary  item, description, description_long, section, year
#
# Queries are lazy: filter/select run inside Arrow and only matching rows are
# loaded when collect() is called. Keep filters before collect().

library(dplyr)

FFIEC_PARQUET_ROOT <- "data/parquet"

ffiec_open <- function(table, root = FFIEC_PARQUET_ROOT) {
  path <- file.path(root, table)
  if (!dir.exists(path)) stop("No Parquet table at ", path)
  arrow::open_dataset(path, unify_schemas = TRUE)
}

# Search item codes / descriptions. product: "call" or "ubpr".
ffiec_find_items <- function(pattern, product = c("call", "ubpr")) {
  product <- match.arg(product)
  ffiec_open(paste0(product, "_dictionary")) |>
    dplyr::collect() |>
    dplyr::filter(grepl(pattern, item, ignore.case = TRUE) |
                    grepl(pattern, description, ignore.case = TRUE)) |>
    dplyr::distinct(item, .keep_all = TRUE) |>
    dplyr::arrange(item)
}

# Bank-quarter panel for selected items.
#   items: character vector of item codes (Call or UBPR, matching `product`)
#   banks: optional IDRSSD vector; from/to: optional date bounds (inclusive)
#   wide:  TRUE -> one column per item; FALSE -> long (item, value, value_text)
# Bank names come from the Call Report Panel of Reporters for the same quarter.
ffiec_panel <- function(items, product = c("call", "ubpr"), banks = NULL,
                        from = NULL, to = NULL, wide = TRUE) {
  product <- match.arg(product)
  q <- ffiec_open(paste0(product, "_values")) |> dplyr::filter(item %in% items)
  if (!is.null(banks)) q <- dplyr::filter(q, idrssd %in% banks)
  if (!is.null(from))  q <- dplyr::filter(q, report_date >= as.Date(from))
  if (!is.null(to))    q <- dplyr::filter(q, report_date <= as.Date(to))
  long <- dplyr::collect(q)

  missing_items <- setdiff(items, unique(long$item))
  if (length(missing_items) > 0) {
    warning("No data for: ", paste(missing_items, collapse = ", "),
            " (check code/product with ffiec_find_items())")
  }

  names_tbl <- ffiec_open("call_banks") |>
    dplyr::select(idrssd, report_date, name = `Financial Institution Name`,
                  state = `Financial Institution State`) |>
    dplyr::collect()

  out <- if (wide) {
    long |>
      dplyr::select(idrssd, report_date, item, value) |>
      tidyr::pivot_wider(names_from = item, values_from = value)
  } else {
    long
  }
  out |>
    dplyr::left_join(names_tbl, by = c("idrssd", "report_date")) |>
    dplyr::relocate(idrssd, name, state, report_date) |>
    dplyr::arrange(idrssd, report_date)
}

# OPT-IN: combine consolidated (RCFD, foreign + domestic offices; FFIEC 031 filers)
# with domestic-only (RCON) versions of the same line item into one column,
# preferring RCFD where reported. This changes meaning for 031 filers vs 041/051
# filers, so apply it deliberately, per item. Same idea for RCFA/RCOA (RC-R).
#   df: wide output of ffiec_panel(); codes: 4-character item numbers, e.g. "2170"
call_combine_offices <- function(df, codes, prefixes = c("RCFD", "RCON")) {
  for (code in codes) {
    cols <- paste0(prefixes, code)
    present <- intersect(cols, names(df))
    if (length(present) == 0) stop("None of ", paste(cols, collapse = "/"), " in data")
    df[[paste0("combined_", code)]] <- do.call(dplyr::coalesce, unname(as.list(df[present])))
  }
  df
}
