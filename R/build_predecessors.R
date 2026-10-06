# Predecessor banks for pro forma deposit shares: every bank merged (whole-bank,
# unassisted) into a study bank since 2019, followed recursively so chains are
# captured (A merged into B in 2020, B into the study bank in 2023).
#
# Failed-bank acquisitions (FDIC codes 211/216) are deliberately excluded: their
# pre-failure deposits ran off before the acquirer took them, so crediting them to
# the acquirer's history would show a spurious share loss (decision 2026-10-01).
#
# Usage: Rscript R/build_predecessors.R -> data/reference/deposit_study_predecessors.csv

library(dplyr)
source("R/build_bank_universe.R")   # fdic_get()

MERGER_CODES <- c("221", "223", "224")  # absorption, merger, affiliated merger

fdic_cert_for_rssd <- function(rssd) {
  x <- fdic_get("institutions", sprintf("FED_RSSD:%s", paste(rssd, collapse = " OR FED_RSSD:")),
                c("FED_RSSD", "CERT", "NAME"), limit = 1000)
  dplyr::transmute(x, idrssd = as.integer(FED_RSSD), cert = as.integer(CERT), name = NAME)
}

merged_into <- function(cert, since) {
  Sys.sleep(0.5)  # FDIC API throttles bursts
  h <- fdic_get("history", sprintf('ACQ_CERT:%s AND EFFDATE:["%s" TO *]', cert, since),
                c("CHANGECODE", "EFFDATE", "ACQ_CERT", "OUT_CERT", "OUT_INSTNAME"), limit = 2000)
  if (nrow(h) == 0) return(NULL)
  h |>
    dplyr::filter(as.character(CHANGECODE) %in% MERGER_CODES, OUT_CERT != cert) |>
    dplyr::distinct(OUT_CERT, .keep_all = TRUE) |>
    dplyr::transmute(acq_cert = as.integer(cert), cert = as.integer(OUT_CERT),
                     bank_name = OUT_INSTNAME, event_date = as.Date(substr(EFFDATE, 1, 10)),
                     changecode = as.character(CHANGECODE))
}

build_predecessors <- function(study, since = "2019-01-01") {
  roots <- fdic_cert_for_rssd(unique(study$idrssd)) |>
    dplyr::inner_join(dplyr::distinct(study, idrssd, ticker), by = "idrssd")
  frontier <- dplyr::transmute(roots, ticker, cert, depth = 0L)
  found <- list()
  while (nrow(frontier) > 0) {
    nxt <- purrr::map_dfr(seq_len(nrow(frontier)), function(i) {
      m <- merged_into(frontier$cert[i], since)
      if (is.null(m)) return(NULL)
      dplyr::mutate(m, ticker = frontier$ticker[i], depth = frontier$depth[i] + 1L)
    })
    if (nrow(nxt) == 0) break
    nxt <- dplyr::filter(nxt, !cert %in% c(roots$cert, unlist(lapply(found, `[[`, "cert"))))
    if (nrow(nxt) > 0) found[[length(found) + 1]] <- nxt
    frontier <- dplyr::select(nxt, ticker, cert, depth)
  }
  preds <- dplyr::bind_rows(found)
  if (nrow(preds) == 0) return(preds)
  # RSSD for each predecessor cert (inactive institutions included)
  ids <- fdic_get("institutions", sprintf("CERT:%s", paste(unique(preds$cert), collapse = " OR CERT:")),
                  c("FED_RSSD", "CERT"), limit = 2000) |>
    dplyr::transmute(cert = as.integer(CERT), idrssd = as.integer(FED_RSSD))
  preds |>
    dplyr::left_join(ids, by = "cert") |>
    dplyr::arrange(ticker, event_date) |>
    dplyr::mutate(fdic_pulled = Sys.Date())
}

if (sys.nframe() == 0) {
  study <- readr::read_csv("data/reference/deposit_study_banks.csv", show_col_types = FALSE)
  p <- build_predecessors(study)
  readr::write_csv(p, "data/reference/deposit_study_predecessors.csv")
  print(as.data.frame(dplyr::select(p, ticker, bank_name, idrssd, event_date, depth)), right = FALSE)
}
