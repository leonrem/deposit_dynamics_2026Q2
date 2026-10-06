# Build the bank-level universe for a set of holding companies: every Call Report
# filer each firm owns today, plus banks merged into those subsidiaries since a
# start date, plus former/uninsured subsidiaries still tied to the parent.
#
# Why FDIC BankFind: Call/UBPR are bank-level; the parent link (RSSDHCR) and the
# merger history live in the FDIC API (public, no key). The FFIEC NIC site blocks
# scripted access. Parent RSSDs below were looked up from that API on 2026-10-01.
#
# Usage: Rscript R/build_bank_universe.R   -> data/reference/stress_test_bank_universe.csv

library(dplyr)

FDIC_API <- "https://api.fdic.gov/banks"

# 24 domestic firms in the Fed's 2026 stress test (32 participants minus the 8
# foreign-owned IHCs: Barclays, BMO, DB, RBC, Santander, TD, UBS, HSBC), plus
# Pinnacle and Zions. Zions has no holding company: the bank is the top entity.
FIRMS <- tibble::tribble(
  ~ticker, ~firm,                                 ~parent_rssd, ~group,
  "BAC",   "Bank of America Corporation",          1073757L, "Stress test 2026 - Cat I",
  "BK",    "Bank of New York Mellon Corporation",  3587146L, "Stress test 2026 - Cat I",
  "C",     "Citigroup Inc.",                       1951350L, "Stress test 2026 - Cat I",
  "GS",    "Goldman Sachs Group, Inc.",            2380443L, "Stress test 2026 - Cat I",
  "JPM",   "JPMorgan Chase & Co.",                 1039502L, "Stress test 2026 - Cat I",
  "MS",    "Morgan Stanley",                       2162966L, "Stress test 2026 - Cat I",
  "STT",   "State Street Corporation",             1111435L, "Stress test 2026 - Cat I",
  "WFC",   "Wells Fargo & Company",                1120754L, "Stress test 2026 - Cat I",
  "NTRS",  "Northern Trust Corporation",           1199611L, "Stress test 2026 - Cat II",
  "AXP",   "American Express Company",             1275216L, "Stress test 2026 - Cat III",
  "COF",   "Capital One Financial Corporation",    2277860L, "Stress test 2026 - Cat III",
  "SCHW",  "Charles Schwab Corporation",           1026632L, "Stress test 2026 - Cat III",
  "PNC",   "PNC Financial Services Group, Inc.",   1069778L, "Stress test 2026 - Cat III",
  "TFC",   "Truist Financial Corporation",         1074156L, "Stress test 2026 - Cat III",
  "USB",   "U.S. Bancorp",                         1119794L, "Stress test 2026 - Cat III",
  "ALLY",  "Ally Financial Inc.",                  1562859L, "Stress test 2026 - Cat IV",
  "CFG",   "Citizens Financial Group, Inc.",       1132449L, "Stress test 2026 - Cat IV",
  "FITB",  "Fifth Third Bancorp",                  1070345L, "Stress test 2026 - Cat IV",
  "FCNCA", "First Citizens BancShares, Inc.",      1075612L, "Stress test 2026 - Cat IV",
  "HBAN",  "Huntington Bancshares Incorporated",   1068191L, "Stress test 2026 - Cat IV",
  "KEY",   "KeyCorp",                              1068025L, "Stress test 2026 - Cat IV",
  "MTB",   "M&T Bank Corporation",                 1037003L, "Stress test 2026 - Cat IV",
  "RF",    "Regions Financial Corporation",        3242838L, "Stress test 2026 - Cat IV",
  "SYF",   "Synchrony Financial",                  4504654L, "Stress test 2026 - Cat IV",
  "PNFP",  "Pinnacle Financial Partners, Inc.",    6073350L, "Added",
  "ZION",  "Zions Bancorporation, N.A.",           NA_integer_, "Added"
)
ZIONS_BANK_RSSD <- 276579L

# FDIC change codes that move a whole bank into another (branch purchases, 712, excluded)
WHOLE_BANK_CODES <- c(
  "211" = "failed bank acquired", "216" = "failed bank acquired (bridge)",
  "221" = "merged predecessor", "223" = "merged predecessor", "224" = "merged predecessor (affiliate)"
)

fdic_get <- function(endpoint, filters, fields, limit = 10000) {
  resp <- httr2::request(paste0(FDIC_API, "/", endpoint)) |>
    httr2::req_url_query(filters = filters, fields = paste(fields, collapse = ","),
                         limit = limit, format = "json") |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)
  if (resp$meta$total > limit) stop(endpoint, ": ", resp$meta$total, " rows exceed limit ", limit)
  tibble::as_tibble(resp$data$data)
}

build_bank_universe <- function(since = "2019-01-01") {
  inst_fields <- c("NAME", "CERT", "FED_RSSD", "RSSDHCR", "NAMEHCR", "ACTIVE", "ENDEFYMD", "STALP", "ASSET")
  active <- fdic_get("institutions", "ACTIVE:1", inst_fields)
  inactive <- fdic_get("institutions", sprintf('ACTIVE:0 AND ENDEFYMD:["%s" TO *]', since), inst_fields)
  all_inst <- dplyr::bind_rows(active, inactive) |>
    dplyr::mutate(FED_RSSD = as.integer(FED_RSSD), RSSDHCR = as.integer(RSSDHCR))

  # 1. Banks whose top holder is one of the parents (active = current subsidiary;
  #    inactive = former subsidiary, often an uninsured trust bank still filing)
  by_parent <- all_inst |>
    dplyr::inner_join(dplyr::filter(FIRMS, !is.na(parent_rssd)), by = c("RSSDHCR" = "parent_rssd")) |>
    dplyr::mutate(relationship = dplyr::if_else(ACTIVE == 1, "current subsidiary",
                                                "former/uninsured subsidiary"))
  zions <- all_inst |>
    dplyr::filter(FED_RSSD == ZIONS_BANK_RSSD) |>
    dplyr::mutate(ticker = "ZION", relationship = "current subsidiary") |>
    dplyr::left_join(dplyr::select(FIRMS, ticker, firm, group), by = "ticker")
  current <- dplyr::bind_rows(by_parent, zions)

  # 2. Whole-bank mergers/failures into any current subsidiary since `since`
  hist_fields <- c("CHANGECODE", "EFFDATE", "ACQ_CERT", "OUT_CERT", "OUT_INSTNAME")
  acquirers <- dplyr::filter(current, relationship == "current subsidiary")
  merges <- purrr::map_dfr(seq_len(nrow(acquirers)), function(i) {
    h <- fdic_get("history", sprintf('ACQ_CERT:%s AND EFFDATE:["%s" TO *]', acquirers$CERT[i], since),
                  hist_fields, limit = 2000)
    if (nrow(h) == 0) return(NULL)
    h |>
      dplyr::filter(as.character(CHANGECODE) %in% names(WHOLE_BANK_CODES), OUT_CERT != acquirers$CERT[i]) |>
      dplyr::mutate(ticker = acquirers$ticker[i], acquirer_rssd = acquirers$FED_RSSD[i])
  }) |>
    dplyr::distinct(OUT_CERT, .keep_all = TRUE) |>
    dplyr::left_join(dplyr::select(all_inst, CERT, NAME, FED_RSSD, STALP), by = c("OUT_CERT" = "CERT")) |>
    dplyr::left_join(dplyr::select(FIRMS, ticker, firm, group), by = "ticker") |>
    dplyr::mutate(relationship = unname(WHOLE_BANK_CODES[as.character(CHANGECODE)]),
                  event_date = as.Date(substr(EFFDATE, 1, 10)),
                  NAME = dplyr::coalesce(NAME, OUT_INSTNAME))

  # Merges first: an absorbed bank's FDIC record often still carries the parent's
  # RSSDHCR, so it also matches step 1; the merger label is the more specific one.
  universe <- dplyr::bind_rows(
    merges |> dplyr::transmute(ticker, firm, group, idrssd = FED_RSSD, cert = OUT_CERT,
                               bank_name = NAME, state = STALP, relationship,
                               event_date, acquirer_rssd),
    current |> dplyr::transmute(ticker, firm, group, idrssd = FED_RSSD, cert = CERT,
                                bank_name = NAME, state = STALP, relationship,
                                event_date = as.Date(NA), acquirer_rssd = NA_integer_)
  ) |>
    dplyr::distinct(idrssd, cert, .keep_all = TRUE) |>
    dplyr::arrange(match(ticker, FIRMS$ticker), relationship, bank_name)

  # The bridge bank never filed a Call Report; the failed bank it replaced did.
  # SVB failed 2023-03-10 into the bridge, which First Citizens acquired.
  svb <- dplyr::filter(all_inst, grepl("^Silicon Valley Bank$", NAME))
  if (nrow(svb) == 1) {
    universe <- dplyr::bind_rows(universe, tibble::tibble(
      ticker = "FCNCA", firm = FIRMS$firm[FIRMS$ticker == "FCNCA"], group = FIRMS$group[FIRMS$ticker == "FCNCA"],
      idrssd = svb$FED_RSSD, cert = svb$CERT, bank_name = svb$NAME, state = svb$STALP,
      relationship = "failed bank acquired (via bridge)", event_date = as.Date("2023-03-27"),
      acquirer_rssd = current$FED_RSSD[current$NAME == "First-Citizens Bank & Trust Company"][1]
    ))
  }
  universe |> dplyr::mutate(fdic_pulled = Sys.Date())
}

# Add Call Report coverage from the local store; banks with no Call filings in the
# window (e.g. the SVB bridge bank, non-filing trust companies) get n_quarters = 0.
add_call_coverage <- function(universe, parquet_root = "data/parquet") {
  cov <- arrow::open_dataset(file.path(parquet_root, "call_banks"), unify_schemas = TRUE) |>
    dplyr::select(idrssd, report_date) |>
    dplyr::collect() |>
    dplyr::group_by(idrssd) |>
    dplyr::summarise(first_call = min(report_date), last_call = max(report_date),
                     n_quarters = dplyr::n(), .groups = "drop")
  latest <- max(cov$last_call)
  universe |>
    dplyr::left_join(cov, by = "idrssd") |>
    dplyr::mutate(
      n_quarters = dplyr::coalesce(n_quarters, 0L),
      # FDIC marks banks that dropped deposit insurance as inactive, but uninsured
      # trust banks keep filing Call Reports under the same parent
      relationship = dplyr::case_when(
        relationship == "former/uninsured subsidiary" & last_call == latest ~ "current subsidiary (uninsured)",
        relationship == "former/uninsured subsidiary" ~ "former subsidiary",
        TRUE ~ relationship
      )
    )
}

if (sys.nframe() == 0) {
  u <- build_bank_universe() |> add_call_coverage()
  dir.create("data/reference", recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(u, "data/reference/stress_test_bank_universe.csv")
  print(dplyr::count(u, relationship))
  message("Wrote ", nrow(u), " banks for ", dplyr::n_distinct(u$ticker), " firms")
}
