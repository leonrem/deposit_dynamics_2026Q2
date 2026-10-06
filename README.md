# deposit_dynamics_2026Q2

Bank-level Call Report and UBPR data from the FFIEC bulk download site, stored as long Parquet tables, plus a memo on deposit dynamics at 15 large regional banks built on top of it. Everything uses public sources. The repository holds the code and the memo; the data are not stored here and are rebuilt by one command.

**Deliverable:** `reports/2026_Q2_deposit_pressure_memo.pdf`, rendered from `reports/2026_Q2_deposit_pressure_memo.Rmd` by `R/run_all.R` (2019Q1-2026Q2 Call Reports and UBPR).

## Quick start

1. Install R (4.3 or later), pandoc and a LaTeX engine that includes `xelatex` (RStudio bundles pandoc; TinyTeX or TeX Live provide LaTeX). The memo uses the TeX Gyre Heros and Latin Modern fonts and the `arydshln`, `sectsty`, `etoolbox` and `caption` packages; TeX Live includes them, and TinyTeX needs `tinytex::tlmgr_install(c("tex-gyre", "lm", "arydshln", "sectsty", "etoolbox", "caption"))`.
2. From the project root, install the R packages: `Rscript R/install_packages.R`
3. Build everything and render the memo: `Rscript R/run_all.R` (about 1 GB of public downloads, then every table, then `reports/2026_Q2_deposit_pressure_memo.pdf`). Once the data exist on your machine, `Rscript R/run_all.R --memo-only` re-renders the memo without downloading or rebuilding.
4. The SEC cross-check (a holding-company check on deposit interest) needs `SEC_CONTACT` set to a name and email (the SEC fair-access policy requires one), for example `SEC_CONTACT="Your Name you@example.com" Rscript R/run_all.R`; add `--no-sec` to skip it.

Everything under `data/` is created by the build and is not committed, except `data/reference/mgmt_guidance_summary_2026Q2.csv`: it is coded by hand from public earnings-call transcripts (management tone and guidance for the 15 banks), so it is an input, not a pipeline output. Run every command from the project folder (the one that contains `R/run_all.R`); the scripts and the memo find it themselves and use no machine-specific paths.

## Sources

- **FFIEC CDR bulk download** (https://cdr.ffiec.gov/public/PWS/DownloadBulkData.aspx): Call Reports, single period, one zip per quarter-end (tab-delimited, all schedules plus the Panel of Reporters); UBPR Ratio, four periods, one zip per calendar year. UBPR Stats and Rank are not used.
- **FDIC BankFind API:** bank-to-holding-company links, merger history, and Summary of Deposits (branch-level deposits by year).
- **NY Fed Markets API and Treasury:** effective fed funds and the 3-month bill (FRED blocks scripted downloads).
- **SEC EDGAR XBRL companyfacts:** holding-company deposit interest and deposits, used only as a cross-check.
- **Public earnings-call transcripts**, 2026Q1 and Q2, for the 15 banks.

## Structure

```
R/install_packages.R, R/run_all.R   setup and end-to-end rebuild (see Quick start)
R/fetch_ffiec_bulk.R, R/parse_ffiec_bulk.R, R/build_ffiec_store.R   download and parse the FFIEC files into data/parquet/
R/ffiec_data.R          analysis helpers: ffiec_open(), ffiec_find_items(), ffiec_panel(), call_combine_offices()
R/build_bank_universe.R firm -> bank (IDRSSD) mapping from FDIC BankFind -> data/reference/stress_test_bank_universe.csv
R/build_subset.R        cut a bank subset (all items) -> data/subsets/<name>/
R/fetch_rates.R         EFFR, target range, 3-month bill -> data/analysis/rates_*.parquet
R/build_deposit_study_banks.R  study bank list with peer groups -> data/reference/deposit_study_banks.csv
R/build_predecessors.R  banks merged into the study banks since 2019 -> data/reference/deposit_study_predecessors.csv
R/build_deposit_panel.R deposit items -> bank and firm quarterly panels (current and pro forma) -> data/analysis/deposit_panel_*.parquet; also builds savings/MMDA/NOW from its line items and stops if the three deposit buckets do not sum to total deposits (every firm-quarter since 2021Q4)
R/fetch_sec_xbrl.R      SEC cross-check -> data/analysis/sec_deposit_facts.parquet
R/fetch_sod.R, R/build_sod_shares.R   FDIC Summary of Deposits and market shares -> data/analysis/sod_*.parquet
R/build_geography_profile.R   branch-deposit footprint by Census region; region market context
R/build_availability_series.R deposit growth by source, loan-deposit gap, wholesale growth since the first cut
R/build_franchise_profile.R   bank-by-bank franchise table; price-leader gap by peer group
R/build_gap_leads_cost.R      back-test: does a bank's funding gap lead its cost relative to peers (feeds the page-1 bullet)
R/build_gap_financing.R        how the funding gap was financed: wholesale, equity (retained earnings, AOCI, paid-in capital), securities, cash and other (memo Appendix B)
R/build_runoff_capacity.R     runoff pool, CDs due, FHLB, cash, unpledged securities, securities loss, coverage (memo Table 2)
R/study_metrics.R       shared metrics (betas, mix, positions, sensitivity) sourced by the memo and several scripts
reports/                the memo (Rmd + PDF)
data/raw/{call,ubpr,rates,sec,sod}/   source downloads, never modified
data/reference/         bank universe, study banks, predecessors (built) and the hand-coded management guidance summary
data/analysis/          analysis-ready tables read by the memo
data/parquet/<table>/   one Parquet file per period per table
data/provenance_*.csv   download log (bytes, md5, time) and build log (created by the build)
```

## Using the store

Run everything from the project root.

```r
source("R/ffiec_data.R")
ffiec_find_items("leverage", "ubpr")                 # search codes + descriptions
ubpr <- ffiec_panel(c("UBPRE013", "UBPRD486"), "ubpr", from = "2021-03-31")
call <- ffiec_panel(c("RCFD2170", "RCON2170"), "call") |> call_combine_offices("2170")
```

`ffiec_open("call_values")` returns a lazy Arrow dataset; `dplyr` verbs run in Arrow until `collect()`.

## The deposit-pressure memo

`reports/2026_Q2_deposit_pressure_memo.pdf` studies the deposit dynamics of 15 large regionals (the ten focus regionals plus Zions, Pinnacle, Western Alliance, First Horizon and East West) in the last cutting cycle: which strategies they ran (core-funded or paying to grow), who has to gather funding, who is already paying more, what could run, and who may change strategy as rates turn.

**Layout.** Page 1: bullets only (objective, strategies in the last cycle, what drives cost and rate sensitivity, who may change strategy, starting this cycle at a higher cost and what could run, combinations to watch, AI, limitations). Page 2: Table 1a, Table 1b and Chart 1a (funding gap against pricing score). Page 3: Chart 1b (franchise: rate-shopper funding against cost), Table 2 (deposit mix 2026Q2 and migration since the first cut) and Table 3 (runoff pool, costly categories, capacity). Appendices: A supporting charts (growth and starting cost against the change in cost; the rate-shopper share at 2021Q4 against 2026Q2 and against up-cycle betas), B how the funding gap was financed (the accounting), C calculation reference. The comparison-bank and +25bp sensitivity appendices were dropped from the memo. Bullets link to the tables and charts they cite. Names shaded in the tables are firms in the Fed's 2026 stress test (`data/reference/stress_test_bank_universe.csv`); an asterisk marks a major merger since 2021.

- **Table 1a, who needs to gather (no score):** banks are ordered by the **funding gap**, the change in loans less the change in deposits from the first cut (2024Q3) to 2026Q2 in percentage points of starting deposits, pro forma for deals. Five columns show how it was financed and add up to it (Appendix B): Equity Δ, − Goodwill Δ, Wholesale funding Δ, − Securities Δ, − Cash and other Δ (actual balance-sheet changes; subtract the columns headed with a minus). Loan growth, deposit growth and loans / deposits are shaded context. Back-test (`R/build_gap_leads_cost.R`): a bank whose loans outran deposits saw its interest-bearing cost rise relative to peers over the next year (correlation 0.50 and 0.23 across two sets of non-overlapping year windows).
- **Table 1b, who is already paying more (pricing score 0-100):** the pricing score is the average percentile rank among the 15 banks of two snapshots, interest-bearing cost (annualized 2026Q2 interest expense over average interest-bearing deposits, against the peer median) and CDs plus brokered as a share of interest-bearing deposits. Up- and down-cycle betas, noninterest-bearing share, uninsured share and wholesale funding growth are shaded context. Management tone is shown unshaded.
- **Betas:** change in the cost of total deposits divided by the change in the average effective fed funds rate; up-cycle 2021Q4-2024Q3, down-cycle 2024Q3-2026Q2.
- **M&A:** levels use each bank's current structure; growth and the financing breakdown are pro forma (banks merged in since 2019 are added back in every quarter).
- **Earlier scoring (kept in the Rmd for reference only):** a single 0-100 score averaging a deposit-base pillar (betas, NIB, time, uninsured, brokered) and a funding-need pillar was replaced by the two tables above; the old code still supplies the beta, NIB and uninsured fields and one correlation quoted on page 1.

## Deposit cost basis: balance-sheet averages, not RC-K

Deposit costs and betas in this project use the average of beginning- and end-of-quarter balance-sheet deposits, not the quarterly averages in Call Report Schedule RC-K that UBPR uses.

MDRM items: deposit interest expense is RIAD4508 + RIAD0093 + RIADHK03 + RIADHK04 + RIAD4172 (year-to-date, differenced to quarterly). Balance-sheet deposits are RCON2200 + RCFN2200 (total), RCON6631 + RCFN6631 (noninterest-bearing) and RCON6636 + RCFN6636 (interest-bearing). The RC-K interest-bearing averages are RCON3485 + RCONB563 + RCONHK16 + RCONHK17 + RCFN3404.

```
IE_q        = quarterly deposit interest expense (YTD items differenced)
ann         = 365 / days in quarter
RC-K basis  : cost_IB = ann * IE_q / (RC-K interest-bearing average)
              cost_total = ann * IE_q / (RC-K interest-bearing average + 0.5 * (NIB_t + NIB_t-1))
Used here   : cost_total = ann * IE_q / (0.5 * (D_t + D_t-1)),   D  = RCON2200 + RCFN2200
              cost_IB    = ann * IE_q / (0.5 * (IB_t + IB_t-1)), IB = RCON6636 + RCFN6636
beta        = (cost_end - cost_start) / (EFFR_end - EFFR_start)
```

Why: banks classify zero-rate savings and money market balances inconsistently between RC-K (interest-bearing) and the balance sheet (noninterest-bearing). Adding RC-K interest-bearing averages to balance-sheet noninterest-bearing balances counts those balances twice, so the denominator is too large and the cost and beta are too low. If the double-counted balances are a fraction of the true average, cost_RC-K = cost_true x D / (D + overlap). M&T's RC-K averages ran about 28% above its balance-sheet interest-bearing deposits until a reclassification in 2025Q1, which understated its cost by about a fifth (a true 56% beta looked like about 44%); JPMorgan and Bank of America showed the overlap in every quarter. It was found by reconciling the measured beta to M&T's stated 56%. The balance-sheet basis matches management-reported costs and stated betas (M&T 56%, Regions 37%).

The RC-K versions are kept as `*_rck` columns; the interest-bearing one reproduces UBPRE701 exactly when annualized by 4, so use it only when replicating UBPR. Trade-off: a two-point average is a rougher estimate of the true average balance, so single-quarter cost changes are noisy (Truist 2026Q2: +9bp measured against +1bp reported); use cumulative windows or check against management figures. Do not revert to RC-K without redoing this check.

## Data conventions

- **Stored long, used wide.** Storage is long because the full item set (~3,800 Call, ~3,100 UBPR) is mostly blank per bank, item codes change across quarters, and some numeric items carry `CONF` text. `ffiec_panel(..., wide = TRUE)` pivots the items an analysis actually uses into a bank x quarter table.
- Long format: `idrssd, report_date, item, value, value_text`. Call dollar amounts in $000s; UBPR ratios in percent.
- Blank cells = not reported, dropped from the long tables.
- Non-numeric cells (`CONF` = confidential, `true`/`false`, free text) are in `value_text` with `value = NA`.
- Call ratio items reported as `"9.2956%"` (Schedule RC-R) are stored as `9.2956`.
- Line breaks inside Call free-text fields are preserved in `value_text`.
- **RCFD vs RCON is not merged in storage.** RCFD = consolidated incl. foreign offices (031 filers); RCON = domestic offices. `call_combine_offices()` merges them on request, preferring RCFD.
- If the same item appears in more than one schedule/section file, identical values are kept once; conflicting values stop the build.

## Subset: domestic stress-test banks + Pinnacle + Zions

`data/subsets/stress_domestic24_pnfp_zion/` -- 26 firms, 68 banks, 1,584 bank-quarters (2019Q1-2026Q2).

- **Firms:** the 24 domestic holding companies in the Fed's 2026 stress test (32 participants minus the 8 foreign-owned IHCs: Barclays, BMO, DB, RBC, Santander, TD, UBS, HSBC), plus Pinnacle Financial Partners and Zions (no holding company; the bank is the top entity).
- **Banks:** every Call filer each firm owns today (incl. 3 uninsured trust banks), plus banks merged into them since 2019 (SunTrust, BBVA USA, MUFG Union, Discover, Comerica, Cadence, Synovus, ...) and failed banks acquired (First Republic, SVB). Their *pre-acquisition* quarters are included and tagged via `relationship` + `event_date` -- filter on those for as-owned vs pro forma views.
- **Source of the mapping:** FDIC BankFind API (`RSSDHCR` parent link + structure-change history, whole-bank merger codes only), pulled 2026-10-01. Limitation: a bank *sold* out of one of these firms since 2019 would carry its new parent and be missed.
- **Files:** `call_values.parquet` / `ubpr_values.parquet` (long, every item incl. text), `call_wide.rds` (1,584 x 3,597) / `ubpr_wide.rds` (1,584 x 2,816) with numeric items only, `banks.csv` (the mapping).
- **Current structure only (decision 2026-10-01):** the data files hold the 40 banks each firm owns today, all 30 quarters each -- a balanced panel with no entries/exits. Merged predecessors, failed-bank acquisitions and former subsidiaries are listed in `data/reference/stress_test_bank_universe.csv` but excluded.
- **Firm-level sums will show step changes** from (a) acquisitions: COF 2025Q2 (Discover), FITB 2026Q1 (Comerica), HBAN 2021Q2/2026Q1 (TCF/Cadence), PNFP 2026Q1 (Synovus), TFC 2019Q4 (SunTrust), MTB/CFG 2022Q2, PNC 2021Q4, USB 2023Q2, FCNCA 2022Q1/2023Q1; and (b) internal consolidation of excluded affiliates already owned by the firm: COF 2022Q4 (Capital One Bank (USA), ~$117B), MS 2022Q1 (E*TRADE banks, ~$60B), JPM 2019Q2 (Chase Bank USA). Immaterial: Citi's Department Stores NB (~$0.5B), Wells Fargo Ltd (~$0.8B).
- 5 universe entries have no Call filings 2019-2026 (SVB bridge bank, Recontrust, Universal Financial, Wilmington Trust Co, Citizens Bank of PA) and are excluded from the data files.

## Deposit competition panel

`data/analysis/deposit_panel_firm.parquet` -- 36 firms x 30 quarters; `deposit_panel_bank.parquet` -- 50 banks. Peer groups: Focus regional (CFG, FITB, FCNCA, HBAN, KEY, MTB, PNC, RF, TFC, USB), GSIB universal (JPM, BAC, C, WFC), Custody / investment bank (BK, STT, NTRS, GS, MS), Direct / card / brokerage (COF, AXP, SYF, ALLY, SCHW), Smaller regional (ZION, PNFP), Smaller regional (matched) -- one per focus regional, `matched_to` in `data/reference/deposit_study_banks.csv`: WBS-CFG, CBSH-FITB, WAL-FCNCA, ONB-HBAN, COLB-KEY, VLY-MTB, FNB-PNC, FHN-RF, SSB-TFC, ASB-USB (all single-bank HCs; Webster acquired by Santander 2026-08-20, data through 2026Q2).

- **Dollars summed to firm, ratios recomputed** from the sums (never averaged across banks).
- **Deposit interest expense** = RIAD4508 + 0093 + HK03 + HK04 + 4172, de-accumulated from YTD to quarterly.
- **Cost of deposits (changed 2026-10-02):** annualized quarterly deposit interest / average of beginning- and end-of-quarter balance-sheet deposits (total deposits for `cost_total_deposits`, interest-bearing for `cost_ib_deposits`). Annualization = 365 / days in quarter.
- **Why not RC-K (the daily-average schedule UBPR uses):** banks classify zero-rate savings/MMDA inconsistently between RC-K (interest-bearing) and the balance sheet (noninterest-bearing). Combining RC-K interest-bearing averages with balance-sheet NIB double-counted those balances and understated costs and betas: JPM and BAC in every quarter, M&T until a 2025Q1 reclassification (its RC-K averages were ~28% above balance-sheet interest-bearing deposits). Found by reconciling to M&T's stated 56% beta. RC-K versions are kept as `*_rck` columns (they reproduce UBPRE701 exactly with x4 annualization).
- **Validation of the balance-sheet basis:** matches management-reported costs (TFC 1.55%, FCNCA 2.07%, MTB 1.95% IB, RF 1.69% IB in 2026) and stated cumulative IB betas for M&T (56%) and Regions (37%); KeyCorp, Citizens and Truist state 6-9 points more than measured. Single-quarter changes of a few bp are NOT reliable on two-point averages (e.g. TFC 2026Q2 +9bp measured vs +1bp reported) -- use management figures for those.
- **(Loans + securities) / deposits** (`loans_sec_to_deposits`, added 2026-10-04) = (RCFD2122 + RCFD1754 held-to-maturity at amortized cost + RCFD1773 available-for-sale at fair value) / total deposits, summed to firm. Banking-book securities only (no trading assets). Used for the memo's funding-need pillar in place of loans / deposits.
- **Merger quarters** are flagged in `merger_in_quarter`; `deposit_panel_firm_proforma.parquet` adds banks merged in since 2019 to today's firm in every quarter and is used for changes spanning a deal (YTD loans/deposits, growth).
- **Management commentary:** `data/reference/mgmt_guidance_summary_2026Q2.csv` (committed input) codes each bank's 2026Q1 and Q2 earnings-call tone on deposit competition and its net-interest-income guidance, from public transcripts.
- **Uninsured share** (RC-O 5597 / domestic deposits) is computed over banks that report it (small trust banks don't); `uninsured_coverage` >= 96.5% everywhere. STT exceeds 100% in its own filing (RC-O definitional scope).
- **Rates:** `rates_quarterly.parquet` -- EFFR quarterly average (calendar-day weighted) and quarter-end, target range, 3m T-bill coupon-equivalent. Source: NY Fed (EFFR administrator) and Treasury; FRED blocks scripted downloads. The Fed hiked 25bp in 2026Q3 (upper bound 3.75 -> 4.00).
- **SEC cross-check (HC vs Call-derived):** focus regionals' deposit interest within 2% in 146/179 quarters since 2022. Gaps are explained by (a) acquired banks not yet merged into the lead bank (PNC 2021Q2-Q3 BBVA, USB 2022Q4-2023Q1 MUFG Union, FITB 2019Q1 MB Financial), (b) intercompany deposits eliminated at HC (bank deposits 1-4% above HC, MTB up to 10% in 2021), (c) tiny values in 2020-21. M&T deposit interest is derived (total - LTD - ST borrowings) because its savings/checking interest is dimension-tagged; Commerce's is the sum of its three deposit-product concepts. Matched peers (single-bank) match SEC within 2% in 17-18 of 18 quarters since 2022. SEC requests send the contact in the SEC_CONTACT environment variable as the required User-Agent.

## Deposit market shares (FDIC Summary of Deposits)

`data/analysis/sod_markets.parquet` (market-year: deposits, HHI, GSIB shares), `sod_firm_market.parquet` (firm-market-year: deposits, share), `sod_firm_year.parquet` (firm-year: deposit-weighted share, HHI, other-GSIB and JPM share in footprint, metro %, top-5 market concentration). Annual, June 30, 2019-2026.

- **SOD reconciles to Call:** each bank's branch deposits = Call RCON2200 (June) exactly in 91% of 37,928 bank-years, within 1% in 99.9%; aggregate 1.001. Citibank runs 2-4% above.
- **Markets:** metropolitan statistical areas (MSABR); branches outside an MSA (incl. micropolitan, ~7% of deposits) grouped as one non-metro market per state. Focus regionals are 90-98% metro.
- **Subjects:** 26 branch-based firms. Custody (BK, STT, NTRS, GS, MS) and direct/card/brokerage (COF, AXP, SCHW, ALLY, SYF) excluded as subjects; their branches remain in market totals.
- **HHI** on point-in-time top holders (SOD RSSDHCR that year), 0-10,000.
- **Bases:** `pro_forma` (today's banks + banks merged into them since 2019, from FDIC history; failed-bank acquisitions First Republic/SVB excluded) and `current` (today's banks only, matches the quarterly panel). Identical in 2026 by construction (verified).
- **Versions:** `all_branches` and `capped` -- every branch capped at that year's 99th-percentile branch size (~$1.3B in 2026) in firm and market totals. Booking branches distort raw shares: 484 branches over $2B held 50.5% of US deposits in 2026 (WFC Sioux Falls $462B, JPM New York $760B, PNC Pittsburgh $70B). A main-office exclusion was tried and rejected: booking branches are often not the legal main office, and dropping main offices removed genuine home-market deposits (M&T Buffalo, KEY Cleveland). Caveat: the cap also understates home-market dominance where retail deposits are booked to an HQ branch.

## Coverage

Call Reports 2019-03-31 to 2026-06-30 (30 quarters, 4,297-5,411 banks per quarter); UBPR Ratio 2019-2026 (same 30 quarters, same bank set every quarter). Built 2026-10-01: 676 MB raw zips, 1.3 GB Parquet. A full-range wide pull of a few items for all banks takes ~1.5 s.

## Known data behavior

- **Seasonal reporting.** Q1/Q3 Call files have ~20-25% fewer rows than Q2/Q4: FFIEC 051 filers report RC-R Part II (risk-weighted assets) and some RC-C/RC-N/RC-T items only semiannually or annually. A blank Q1 value for those items is "not required", not zero.
- **Pre-2021Q2 rounding.** ~350 banks per quarter (2019Q1-2021Q1) have total assets (2170) differing from total liabilities + capital (3300) by exactly +/-$1K. Zero cases from 2021Q2 on.
- **Call is newer than UBPR after amendments.** UBPR year files are not regenerated when a bank amends a past Call Report. 11 bank-quarters (2022-2025) differ on total assets; all were Call Reports amended Jul-Sep 2026. ~20 of 144k leverage-ratio pairs differ for the same reason.
- **Embedded tabs.** NARR 2022-12-31 has one row with TAB characters in the free text; the parser rejoins these when a file has exactly one TEXT column, and stops otherwise.

## Validation (all 30 quarters, run 2026-10-01)

- Call and UBPR contain the identical bank set every quarter.
- Total assets = total liabilities + capital (3300) except the +/-$1K rounding above; 3300 = 2948 + G105 with no exceptions.
- UBPR2200 = RCON2200 + RCFN2200 for every bank-quarter. UBPR2170 = Call total assets except the 11 amended filings above.
- UBPRD486 (Tier 1 leverage) = reported RCOA/RCFA7204 within UBPR rounding for 144,301 of 144,321 bank-quarters.
