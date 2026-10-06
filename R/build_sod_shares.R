# Deposit market shares and concentration from the FDIC Summary of Deposits (June 30).
#
# Design (decisions 2026-10-01):
#   - Market = metropolitan statistical area (MSABR); branches outside any MSA are
#     grouped as one non-metro market per state. Focus regionals are 90-98% metro.
#   - Market totals and HHI include EVERY bank. HHI is computed on point-in-time top
#     holders (SOD's RSSDHCR for that year): two banks are competitors only if they
#     had different owners at the time.
#   - Subject firms: 26 branch-based firms. Custody banks (BK, STT, NTRS, GS, MS) and
#     direct/card/brokerage banks (COF, AXP, SCHW, ALLY, SYF) are not analysed as
#     subjects, but their branches stay in market totals.
#   - Firm attribution, two bases:
#       pro_forma = today's banks + banks merged into them since 2019 (all years)
#       current   = today's banks only (matches the quarterly deposit panel)
#   - Two versions: all branches, and "capped": every branch's deposits capped at the
#     99th percentile of all US branches that year (~$1.3B in 2026), in firm AND
#     market totals. Centrally booked (corporate, online, sweep) deposits sit in a few
#     giant booking branches -- 484 branches over $2B held 50.5% of US deposits in 2026
#     -- and not only at the legal main office (PNC Pittsburgh, USB St. Paul, FCNCA
#     Santa Clara, WFC Sioux Falls), so a main-office exclusion was tried and rejected.
#
# Inputs : data/raw/sod/sod_<year>.csv, data/reference/deposit_study_banks.csv,
#          data/reference/deposit_study_predecessors.csv
# Outputs: data/analysis/sod_markets.parquet, sod_firm_market.parquet, sod_firm_year.parquet

library(dplyr)

NON_SUBJECTS <- c("BK", "STT", "NTRS", "GS", "MS", "COF", "AXP", "SCHW", "ALLY", "SYF")
GSIB_HOLDERS <- c(JPM = 1039502L, BAC = 1073757L, WFC = 1120754L, C = 1951350L)

read_sod <- function(years = 2019:2026) {
  purrr::map_dfr(years, function(y) {
    readr::read_csv(sprintf("data/raw/sod/sod_%d.csv", y), col_types = readr::cols(.default = "c"),
                    show_col_types = FALSE)
  }) |>
    dplyr::transmute(
      year = as.integer(YEAR), idrssd = as.integer(RSSDID), branch_id = UNINUMBR,
      holder = dplyr::if_else(is.na(as.integer(RSSDHCR)) | as.integer(RSSDHCR) == 0L,
                              as.integer(RSSDID), as.integer(RSSDHCR)),
      holder_name = dplyr::coalesce(dplyr::na_if(NAMEHCR, ""), NAMEFULL),
      main_office = BKMO == "1",
      deposits = dplyr::coalesce(as.numeric(DEPSUMBR), 0),
      state = STALPBR,
      metro = !MSABR %in% c("0", "", NA),
      market_id = dplyr::if_else(metro, paste0("MSA-", MSABR), paste0("NM-", STALPBR)),
      market_name = dplyr::if_else(metro, MSANAMB, paste("Non-metro", STALPBR))
    )
}

attribution <- function() {
  study <- readr::read_csv("data/reference/deposit_study_banks.csv", show_col_types = FALSE) |>
    dplyr::filter(!ticker %in% NON_SUBJECTS)
  preds <- readr::read_csv("data/reference/deposit_study_predecessors.csv", show_col_types = FALSE) |>
    dplyr::filter(ticker %in% study$ticker, !is.na(idrssd))
  peer <- dplyr::distinct(study, ticker, peer_group)
  dplyr::bind_rows(
    dplyr::transmute(study, ticker, idrssd, basis = "current"),
    dplyr::transmute(study, ticker, idrssd, basis = "pro_forma"),
    dplyr::transmute(preds, ticker, idrssd, basis = "pro_forma")
  ) |>
    dplyr::distinct() |>
    dplyr::left_join(peer, by = "ticker")
}

market_tables <- function(sod) {
  # holder-level shares within market-year -> HHI (0-10,000)
  holder_mkt <- sod |>
    dplyr::group_by(version, year, market_id, holder) |>
    dplyr::summarise(dep = sum(deposits), .groups = "drop") |>
    dplyr::group_by(version, year, market_id) |>
    dplyr::mutate(share = 100 * dep / sum(dep)) |>
    dplyr::ungroup()
  gsib <- holder_mkt |>
    dplyr::filter(holder %in% GSIB_HOLDERS) |>
    dplyr::mutate(gsib = names(GSIB_HOLDERS)[match(holder, GSIB_HOLDERS)]) |>
    dplyr::select(version, year, market_id, gsib, share) |>
    tidyr::pivot_wider(names_from = gsib, values_from = share, names_prefix = "share_", values_fill = 0)
  markets <- sod |>
    dplyr::group_by(version, year, market_id, market_name, metro) |>
    dplyr::summarise(market_deposits = sum(deposits), n_branches = dplyr::n(),
                     n_banks = dplyr::n_distinct(idrssd), .groups = "drop") |>
    dplyr::left_join(
      holder_mkt |> dplyr::group_by(version, year, market_id) |>
        dplyr::summarise(hhi = sum(share^2), n_holders = dplyr::n(), .groups = "drop"),
      by = c("version", "year", "market_id")) |>
    dplyr::left_join(gsib, by = c("version", "year", "market_id"))
  for (g in paste0("share_", names(GSIB_HOLDERS))) {
    if (!g %in% names(markets)) markets[[g]] <- 0
    markets[[g]] <- dplyr::coalesce(markets[[g]], 0)
  }
  markets |> dplyr::mutate(share_gsib4 = share_JPM + share_BAC + share_WFC + share_C)
}

build_sod_shares <- function() {
  raw <- read_sod()
  caps <- raw |> dplyr::filter(deposits > 0) |> dplyr::group_by(year) |>
    dplyr::summarise(cap = stats::quantile(deposits, 0.99), .groups = "drop")
  # Retail branch = at or below the cap. Ranking a firm's markets by retail-branch
  # deposits shows where its branch franchise is, rather than where it books deposits.
  raw <- raw |> dplyr::left_join(caps, by = "year") |> dplyr::mutate(retail = deposits <= cap) |> dplyr::select(-cap)
  sod <- dplyr::bind_rows(
    dplyr::mutate(raw, version = "all_branches"),
    raw |> dplyr::left_join(caps, by = "year") |>
      dplyr::mutate(deposits = pmin(deposits, cap), version = "capped") |> dplyr::select(-cap)
  )
  markets <- market_tables(sod)
  attr_tbl <- attribution()

  firm_market <- sod |>
    dplyr::inner_join(attr_tbl, by = "idrssd", relationship = "many-to-many") |>
    dplyr::group_by(version, basis, ticker, peer_group, year, market_id) |>
    dplyr::summarise(firm_deposits = sum(deposits), firm_branches = dplyr::n(),
                     firm_deposits_retail = sum(deposits[retail]), .groups = "drop") |>
    dplyr::left_join(markets, by = c("version", "year", "market_id")) |>
    dplyr::mutate(share = 100 * firm_deposits / market_deposits,
                  # other-GSIB presence: for a GSIB subject, exclude its own share
                  gsib_other_share = share_gsib4 - dplyr::case_when(
                    ticker == "JPM" ~ share_JPM, ticker == "BAC" ~ share_BAC,
                    ticker == "WFC" ~ share_WFC, ticker == "C" ~ share_C, TRUE ~ 0),
                  jpm_share_other = dplyr::if_else(ticker == "JPM", NA_real_, share_JPM))

  firm_year <- firm_market |>
    dplyr::group_by(version, basis, ticker, peer_group, year) |>
    dplyr::summarise(
      sod_deposits = sum(firm_deposits),
      n_branches = sum(firm_branches),
      n_markets = dplyr::n(),
      metro_pct = 100 * sum(firm_deposits[metro]) / sum(firm_deposits),
      dw_market_share = sum(firm_deposits * share) / sum(firm_deposits),
      dw_market_hhi = sum(firm_deposits * hhi) / sum(firm_deposits),
      dw_gsib_other_share = sum(firm_deposits * gsib_other_share) / sum(firm_deposits),
      dw_jpm_share = sum(firm_deposits * jpm_share_other) / sum(firm_deposits),
      top5_market_pct = 100 * sum(sort(firm_deposits, decreasing = TRUE)[1:min(5, dplyr::n())]) / sum(firm_deposits),
      .groups = "drop"
    )
  list(markets = markets, firm_market = firm_market, firm_year = firm_year)
}

if (sys.nframe() == 0) {
  out <- build_sod_shares()
  arrow::write_parquet(out$markets, "data/analysis/sod_markets.parquet")
  arrow::write_parquet(out$firm_market, "data/analysis/sod_firm_market.parquet")
  arrow::write_parquet(out$firm_year, "data/analysis/sod_firm_year.parquet")
  message("markets: ", nrow(out$markets), "  firm-market: ", nrow(out$firm_market),
          "  firm-year: ", nrow(out$firm_year), "  firms: ", dplyr::n_distinct(out$firm_year$ticker))
}
