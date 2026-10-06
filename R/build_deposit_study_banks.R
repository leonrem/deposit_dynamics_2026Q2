# Bank list for the deposit-competition study: current subsidiaries of the 26 firms
# (from the stress-test universe) tagged with a peer group, plus matched smaller
# regional peers (one per focus regional, once approved).
#
# Usage: Rscript R/build_deposit_study_banks.R -> data/reference/deposit_study_banks.csv

library(dplyr)

PEER_GROUPS <- tibble::tribble(
  ~ticker, ~peer_group,
  "CFG",   "Focus regional", "FITB", "Focus regional", "FCNCA", "Focus regional",
  "HBAN",  "Focus regional", "KEY",  "Focus regional", "MTB",   "Focus regional",
  "PNC",   "Focus regional", "RF",   "Focus regional", "TFC",   "Focus regional",
  "USB",   "Focus regional",
  "JPM",   "GSIB universal", "BAC",  "GSIB universal", "C",     "GSIB universal",
  "WFC",   "GSIB universal",
  "BK",    "Custody / investment bank", "STT", "Custody / investment bank",
  "NTRS",  "Custody / investment bank", "GS",  "Custody / investment bank",
  "MS",    "Custody / investment bank",
  "COF",   "Direct / card / brokerage", "AXP", "Direct / card / brokerage",
  "SYF",   "Direct / card / brokerage", "ALLY", "Direct / card / brokerage",
  "SCHW",  "Direct / card / brokerage",
  "ZION",  "Smaller regional", "PNFP", "Smaller regional"
)

# Matched smaller regionals, one per focus regional (approved 2026-10-01). Each
# holding company owns a single bank (checked against FDIC RSSDHCR). Webster was
# acquired by Santander on 2026-08-20; its Call data runs through 2026Q2.
MATCHED_PEERS <- tibble::tribble(
  ~ticker, ~idrssd,  ~bank_name,                               ~matched_to,
  "WBS",   761806L,  "Webster Bank, National Association",     "CFG",
  "CBSH",  601050L,  "Commerce Bank",                          "FITB",
  "WAL",   3138146L, "Western Alliance Bank",                  "FCNCA",
  "ONB",   208244L,  "Old National Bank",                      "HBAN",
  "COLB",  143662L,  "Columbia Bank",                          "KEY",
  "VLY",   229801L,  "Valley National Bank",                   "MTB",
  "FNB",   379920L,  "First National Bank of Pennsylvania",    "PNC",
  "FHN",   485559L,  "First Horizon Bank",                     "RF",
  "SSB",   1929247L, "SouthState Bank, National Association",  "TFC",
  "ASB",   917742L,  "Associated Bank, National Association",  "USB"
)

# Added 2026-10-04: East West (about $84B in bank-level assets, 2026Q2), a third regional near the $100B line that is
# not matched to a focus bank. Single-bank holding company.
ADDED_REGIONALS <- tibble::tribble(
  ~ticker, ~idrssd, ~bank_name,        ~matched_to,
  "EWBC",  197478L, "East West Bank",  NA_character_
)

universe <- readr::read_csv("data/reference/stress_test_bank_universe.csv", show_col_types = FALSE)
current <- universe |>
  dplyr::filter(relationship %in% c("current subsidiary", "current subsidiary (uninsured)"),
                n_quarters > 0) |>
  dplyr::select(ticker, idrssd, bank_name) |>
  dplyr::inner_join(PEER_GROUPS, by = "ticker") |>
  dplyr::mutate(matched_to = NA_character_)

study <- dplyr::bind_rows(
  current,
  dplyr::mutate(MATCHED_PEERS, peer_group = "Smaller regional (matched)"),
  dplyr::mutate(ADDED_REGIONALS, peer_group = "Smaller regional")
)
stopifnot(!anyDuplicated(study$idrssd), all(PEER_GROUPS$ticker %in% study$ticker))
readr::write_csv(study, "data/reference/deposit_study_banks.csv")
print(dplyr::count(study, peer_group))
