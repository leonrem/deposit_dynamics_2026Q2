# Holding-company (HC) figures from SEC EDGAR XBRL "companyfacts", used to
# cross-check the Call-derived firm panel.
#
# SEC fair-access policy requires a User-Agent naming a contact. Set the environment variable SEC_CONTACT
# (for example "Your Name your.name@example.com") before running; requests are throttled well under the 10/sec limit.
#
# Concepts:
#   us-gaap:InterestExpenseDeposits (duration) -> quarterly deposit interest expense
#   us-gaap:Deposits (instant)                  -> period-end total deposits
# Quarterly values: 3-month facts from 10-Qs; Q4 = annual (10-K) - 9-month YTD.
# Fallbacks (labelled in `how`):
#   - Deposit interest reported only by product (Commerce): sum of the three product concepts.
#   - No InterestExpenseDeposits (M&T tags its savings/checking interest with a
#     dimension, which companyfacts omits): derived = total interest expense
#     (InterestExpense or InterestExpenseOperating) - long-term debt - short-term borrowings.
#   - No Deposits for a quarter (Truist from 2026): interest-bearing + noninterest-bearing.
#
# Usage: Rscript R/fetch_sec_xbrl.R -> data/raw/sec/companyfacts_<cik>.json,
#                                      data/analysis/sec_deposit_facts.parquet

library(dplyr)

SEC_UA <- Sys.getenv("SEC_CONTACT")
if (!nzchar(SEC_UA)) stop("Set the SEC_CONTACT environment variable to a name and email, e.g. SEC_CONTACT=\"Your Name your.name@example.com\" (required by the SEC fair-access policy).")
SEC_DIR <- "data/raw/sec"

# Tickers whose SEC registrant differs from our label, or that changed CIK.
# PNFP: the post-Synovus holding company (2026) is a new registrant; the legacy
# Pinnacle CIK holds the history. Zions files with the SEC under its bank CIK.
# WBS: delisted after the 2026 Santander acquisition, so absent from the ticker map.
CIK_OVERRIDES <- tibble::tribble(
  ~ticker, ~cik,
  "BK",    "0001390777",
  "WBS",   "0000801337",
  "PNFP",  "0002082866",
  "PNFP",  "0001115055"
)

sec_get <- function(url, dest = NULL) {
  Sys.sleep(0.2)
  req <- httr2::request(url) |>
    httr2::req_user_agent(SEC_UA) |>
    httr2::req_retry(max_tries = 3)
  if (is.null(dest)) httr2::req_perform(req) else httr2::req_perform(req, path = dest)
}

sec_ticker_map <- function() {
  dest <- file.path(SEC_DIR, "company_tickers.json")
  sec_get("https://www.sec.gov/files/company_tickers.json", dest)
  x <- jsonlite::fromJSON(dest)
  dplyr::bind_rows(lapply(x, tibble::as_tibble)) |>
    dplyr::transmute(ticker, cik = sprintf("%010d", as.integer(cik_str)), sec_name = title)
}

fetch_companyfacts <- function(cik) {
  dest <- file.path(SEC_DIR, sprintf("companyfacts_%s.json", cik))
  sec_get(sprintf("https://data.sec.gov/api/xbrl/companyfacts/CIK%s.json", cik), dest)
  dest
}

# Pull one us-gaap concept (USD units) as a tibble of facts
concept_facts <- function(facts, concept) {
  node <- facts$facts$`us-gaap`[[concept]]
  if (is.null(node) || is.null(node$units$USD)) return(NULL)
  tibble::as_tibble(node$units$USD) |> dplyr::mutate(concept = concept)
}

# Duration facts -> one value per quarter-end. De-duplicates restatements by
# taking the most recently filed value for each (start, end).
quarterly_flows <- function(df) {
  if (is.null(df)) return(NULL)
  d <- df |>
    dplyr::filter(!is.na(start)) |>
    dplyr::mutate(start = as.Date(start), end = as.Date(end), filed = as.Date(filed),
                  days = as.numeric(end - start)) |>
    dplyr::arrange(dplyr::desc(filed)) |>
    dplyr::distinct(start, end, .keep_all = TRUE)
  q3m <- dplyr::filter(d, days >= 80, days <= 100) |> dplyr::transmute(end, value = val, how = "3-month fact")
  # Q4 = 12-month - 9-month YTD ending at Q3 with the same start
  fy <- dplyr::filter(d, days >= 350, days <= 380)
  ytd9 <- dplyr::filter(d, days >= 260, days <= 290)
  q4 <- fy |>
    dplyr::inner_join(ytd9, by = "start", suffix = c("_fy", "_9m")) |>
    dplyr::transmute(end = end_fy, value = val_fy - val_9m, how = "FY - 9M")
  dplyr::bind_rows(q3m, dplyr::anti_join(q4, q3m, by = "end")) |>
    dplyr::mutate(report_date = lubridate::ceiling_date(end, "quarter") - 1)
}

quarterly_stocks <- function(df) {
  if (is.null(df)) return(NULL)
  if (!"start" %in% names(df)) df$start <- NA_character_
  df |>
    dplyr::filter(is.na(start)) |>
    dplyr::mutate(end = as.Date(end), filed = as.Date(filed)) |>
    dplyr::filter(end == lubridate::ceiling_date(end, "quarter") - 1) |>
    dplyr::arrange(dplyr::desc(filed)) |>
    dplyr::distinct(end, .keep_all = TRUE) |>
    dplyr::transmute(report_date = end, value = val)
}

if (sys.nframe() == 0) {
  dir.create(SEC_DIR, recursive = TRUE, showWarnings = FALSE)
  firms <- readr::read_csv("data/reference/deposit_study_banks.csv", show_col_types = FALSE) |>
    dplyr::distinct(ticker, peer_group)
  map <- sec_ticker_map()
  firms <- dplyr::bind_rows(
    dplyr::filter(firms, !ticker %in% CIK_OVERRIDES$ticker) |> dplyr::left_join(map, by = "ticker"),
    dplyr::inner_join(firms, CIK_OVERRIDES, by = "ticker") |>
      dplyr::left_join(dplyr::distinct(map, cik, .keep_all = TRUE) |> dplyr::select(cik, sec_name), by = "cik")
  )
  if (any(is.na(firms$cik))) message("No SEC CIK for: ", paste(firms$ticker[is.na(firms$cik)], collapse = ", "))

  out <- purrr::map_dfr(which(!is.na(firms$cik)), function(i) {
    f <- jsonlite::fromJSON(fetch_companyfacts(firms$cik[i]))
    ie <- quarterly_flows(concept_facts(f, "InterestExpenseDeposits"))
    # Fallback 1: deposit interest reported only by product (e.g. Commerce Bancshares)
    comps <- c("InterestExpenseNOWAccountsMoneyMarketAccountsAndSavingsDeposits",
               "InterestExpenseTimeDeposits100000OrMore", "InterestExpenseTimeDepositsLessThan100000")
    if (is.null(ie) && all(comps %in% names(f$facts$`us-gaap`))) {
      parts <- lapply(comps, function(cn) quarterly_flows(concept_facts(f, cn)) |>
                        dplyr::select(report_date, end, value))
      ie <- Reduce(function(a, b) dplyr::inner_join(a, b, by = c("report_date", "end")), parts) |>
        dplyr::transmute(report_date, end, value = rowSums(dplyr::across(dplyr::starts_with("value"))),
                         how = "sum of deposit-product components")
    }
    # Fallback 2: derived from total interest expense
    if (is.null(ie) || !any(ie$end >= as.Date("2025-01-01"))) {
      tot <- dplyr::bind_rows(quarterly_flows(concept_facts(f, "InterestExpense")),
                              quarterly_flows(concept_facts(f, "InterestExpenseOperating"))) |>
        dplyr::distinct(report_date, .keep_all = TRUE)
      ltd <- quarterly_flows(concept_facts(f, "InterestExpenseLongTermDebt"))
      stb <- quarterly_flows(concept_facts(f, "InterestExpenseShortTermBorrowings"))
      if (nrow(tot) > 0 && !is.null(ltd) && !is.null(stb)) {
        derived <- tot |>
          dplyr::inner_join(dplyr::select(ltd, report_date, ltd = value), by = "report_date") |>
          dplyr::inner_join(dplyr::select(stb, report_date, stb = value), by = "report_date") |>
          dplyr::transmute(report_date, end = report_date, value = value - ltd - stb,
                           how = "derived: total - LTD - ST borrowings")
        ie <- dplyr::bind_rows(ie, dplyr::anti_join(derived, ie %||% derived[0, ], by = "report_date"))
      }
    }
    dep <- quarterly_stocks(concept_facts(f, "Deposits"))
    ibd <- quarterly_stocks(concept_facts(f, "InterestBearingDepositLiabilities"))
    nibd <- quarterly_stocks(concept_facts(f, "NoninterestBearingDepositLiabilities"))
    if (!is.null(ibd) && !is.null(nibd)) {
      alt <- dplyr::inner_join(ibd, nibd, by = "report_date", suffix = c("_ib", "_nib")) |>
        dplyr::transmute(report_date, value = value_ib + value_nib)
      dep <- dplyr::bind_rows(dep, dplyr::anti_join(alt, dep %||% alt[0, ], by = "report_date"))
    }
    dplyr::bind_rows(
      if (!is.null(ie)) dplyr::transmute(ie, report_date, metric = "ie_deposits_q", value, how),
      if (!is.null(dep)) dplyr::transmute(dep, report_date, metric = "deposits", value, how = "instant")
    ) |>
      dplyr::mutate(ticker = firms$ticker[i], cik = firms$cik[i], sec_name = firms$sec_name[i])
  })
  # A ticker with two CIKs (PNFP): prefer the current registrant where both report
  out <- out |>
    dplyr::filter(report_date >= as.Date("2019-01-01")) |>
    dplyr::arrange(ticker, metric, report_date, dplyr::desc(cik)) |>
    dplyr::distinct(ticker, metric, report_date, .keep_all = TRUE)
  dir.create("data/analysis", recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(out, "data/analysis/sec_deposit_facts.parquet")
  print(out |> dplyr::count(ticker, metric) |> tidyr::pivot_wider(names_from = metric, values_from = n), n = 30)
}
