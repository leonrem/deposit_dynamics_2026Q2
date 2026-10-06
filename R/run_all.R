# Rebuild the project end to end from public sources, then render the memo.
#
# Usage (from the project root):
#   Rscript R/run_all.R              # full rebuild: downloads about 1 GB of public data, rebuilds every table, renders the memo
#   Rscript R/run_all.R --memo-only  # skip downloads and rebuilds; render the memo from the tables already built in data/analysis and data/reference
#   Rscript R/run_all.R --no-sec     # full rebuild without the SEC cross-check (which needs the SEC_CONTACT environment variable)
#
# No data is stored in the repository: run the full rebuild once to create data/ on your machine, then --memo-only re-renders the memo.
# data/reference/mgmt_guidance_summary_2026Q2.csv is hand-coded from public earnings-call transcripts and is committed as an input,
# not an output of this pipeline. Requires R, the packages in R/install_packages.R, pandoc and a LaTeX engine with xelatex.

args <- commandArgs(trailingOnly = TRUE)
memo_only <- "--memo-only" %in% args; skip_sec <- "--no-sec" %in% args
setwd(if (file.exists("R/run_all.R")) "." else "..")
run <- function(script, ...) { message("\n== ", script, " =="); status <- system2("Rscript", c(script, ...)); if (status != 0) stop("Step failed: ", script) }

if (!memo_only) {
  first_year <- "2019"; last_year <- format(Sys.Date(), "%Y")
  run("R/build_ffiec_store.R", first_year, last_year)       # Call Reports + UBPR -> data/parquet/ (long tables)
  run("R/fetch_rates.R")                                    # effective fed funds (NY Fed) and 3-month bill (Treasury)
  run("R/fetch_sod.R")                                      # FDIC Summary of Deposits, all branches
  run("R/build_bank_universe.R")                            # firm -> bank (IDRSSD) mapping for the Fed stress-test firms (FDIC BankFind)
  run("R/build_deposit_study_banks.R")                      # bank list for the 15 + comparison banks
  run("R/build_predecessors.R")                             # banks merged into those banks since 2019 (FDIC history)
  run("R/build_deposit_panel.R")                            # firm and bank deposit panels, current and pro forma
  if (!skip_sec) run("R/fetch_sec_xbrl.R")                  # holding-company cross-check (needs SEC_CONTACT)
  run("R/build_sod_shares.R")                               # market shares, HHI, GSIB presence
  run("R/build_geography_profile.R")                        # footprint by Census region + region market context
  run("R/build_availability_series.R")                      # deposit growth by source, loan-deposit gap, wholesale growth
  run("R/build_franchise_profile.R")                        # bank-by-bank franchise table (an input to the geography profile)
  run("R/build_gap_leads_cost.R")                           # back-test: does the funding gap lead cost (page-1 bullet)
  run("R/build_gap_financing.R")                            # how the funding gap was financed (memo Appendix B)
  run("R/build_runoff_capacity.R")                          # runoff pool, CDs, FHLB and capacity (memo Table 3)
}

# Render the memo (pandoc from RStudio if present, TeX Live on the usual macOS path)
rstudio_pandoc <- "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools"
if (!nzchar(Sys.getenv("RSTUDIO_PANDOC")) && dir.exists(rstudio_pandoc)) Sys.setenv(RSTUDIO_PANDOC = rstudio_pandoc)
if (dir.exists("/Library/TeX/texbin")) Sys.setenv(PATH = paste("/Library/TeX/texbin", Sys.getenv("PATH"), sep = ":"))
message("\n== render memo ==")
rmarkdown::render("reports/2026_Q2_deposit_pressure_memo.Rmd", output_dir = "reports", quiet = TRUE)
message("Wrote reports/2026_Q2_deposit_pressure_memo.pdf")
