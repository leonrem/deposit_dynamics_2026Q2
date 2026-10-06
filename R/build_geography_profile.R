# Where each Table 1 bank's branch deposits sit, by U.S. Census region and division (FDIC Summary of Deposits, June 2026, branches capped at the
# 99th-percentile branch size to neutralize booking branches, as in R/build_sod_shares.R). Used to group the memo's main table by geography.
#
# Caveat: SOD shows where deposits are booked at branches. For banks whose deposits are largely outside branch markets (Western Alliance, First
# Citizens, Pinnacle's wholesale funding) the footprint describes only the branch-based part; `branch_share_of_call` reports how much that is.
#
# Usage: Rscript R/build_geography_profile.R -> data/analysis/geography_profile_2026.csv

suppressMessages({library(dplyr); library(tidyr)})
setwd(if (file.exists("R/ffiec_data.R")) "." else "..")

TABLE1 <- c("PNFP", "EWBC", "FCNCA", "USB", "FHN", "TFC", "HBAN", "CFG", "PNC", "WAL", "KEY", "MTB", "ZION", "FITB", "RF")

DIVISION <- c(  # state -> Census division
  CT = "New England", ME = "New England", MA = "New England", NH = "New England", RI = "New England", VT = "New England",
  NJ = "Mid-Atlantic", NY = "Mid-Atlantic", PA = "Mid-Atlantic",
  IL = "East North Central", IN = "East North Central", MI = "East North Central", OH = "East North Central", WI = "East North Central",
  IA = "West North Central", KS = "West North Central", MN = "West North Central", MO = "West North Central", NE = "West North Central",
  ND = "West North Central", SD = "West North Central",
  DE = "South Atlantic", DC = "South Atlantic", FL = "South Atlantic", GA = "South Atlantic", MD = "South Atlantic", NC = "South Atlantic",
  SC = "South Atlantic", VA = "South Atlantic", WV = "South Atlantic",
  AL = "East South Central", KY = "East South Central", MS = "East South Central", TN = "East South Central",
  AR = "West South Central", LA = "West South Central", OK = "West South Central", TX = "West South Central",
  AZ = "Mountain", CO = "Mountain", ID = "Mountain", MT = "Mountain", NV = "Mountain", NM = "Mountain", UT = "Mountain", WY = "Mountain",
  AK = "Pacific", CA = "Pacific", HI = "Pacific", OR = "Pacific", WA = "Pacific")
REGION <- c("New England" = "Northeast", "Mid-Atlantic" = "Northeast", "East North Central" = "Midwest", "West North Central" = "Midwest",
            "South Atlantic" = "South", "East South Central" = "South", "West South Central" = "South", "Mountain" = "West", "Pacific" = "West")

banks <- readr::read_csv("data/reference/deposit_study_banks.csv", show_col_types = FALSE) |> filter(ticker %in% TABLE1) |> distinct(idrssd, ticker)
sod <- readr::read_csv("data/raw/sod/sod_2026.csv", show_col_types = FALSE, col_types = readr::cols(.default = "c")) |>
  transmute(idrssd = as.numeric(RSSDID), state = STALPBR, dep = as.numeric(DEPSUMBR))
cap <- quantile(sod$dep, 0.99, na.rm = TRUE)
d <- sod |> inner_join(banks, by = "idrssd") |> mutate(dep = pmin(dep, cap), division = unname(DIVISION[state]), region = unname(REGION[division]))
stopifnot(!anyNA(d$division))

share <- function(v) d |> group_by(ticker, grp = .data[[v]]) |> summarise(dep = sum(dep), .groups = "drop") |> group_by(ticker) |> mutate(pct = 100 * dep / sum(dep)) |> ungroup()
reg <- share("region") |> select(ticker, region = grp, pct) |> pivot_wider(names_from = region, values_from = pct, values_fill = 0)
top_state <- d |> group_by(ticker, state) |> summarise(dep = sum(dep), .groups = "drop") |> group_by(ticker) |> mutate(pct = 100 * dep / sum(dep)) |>
  arrange(desc(pct), .by_group = TRUE) |> summarise(top_states = paste(sprintf("%s %.0f%%", head(state, 3), head(pct, 3)), collapse = ", "), .groups = "drop")
div <- share("division") |> group_by(ticker) |> arrange(desc(pct), .by_group = TRUE) |> summarise(top_division = first(grp), top_division_pct = first(pct), .groups = "drop")
prof <- reg |> left_join(div, by = "ticker") |> left_join(top_state, by = "ticker") |>
  mutate(primary_region = c("Northeast", "Midwest", "South", "West")[max.col(across(c(Northeast, Midwest, South, West)))],
         primary_pct = pmax(Northeast, Midwest, South, West)) |>
  left_join(readr::read_csv("data/analysis/franchise_profile_2026Q2.csv", show_col_types = FALSE) |> select(ticker, sod_capped_pct_of_call), by = "ticker") |>
  arrange(match(ticker, TABLE1))
readr::write_csv(prof |> mutate(across(where(is.numeric), ~ round(.x, 1))), "data/analysis/geography_profile_2026.csv")
print(as.data.frame(prof |> mutate(across(where(is.numeric), ~ round(.x, 0)))), row.names = FALSE)

# ---- Region-level market context: branch deposits and GSIB presence by Census region, SOD 2019, 2024, 2026 (capped branches, same rule) ----
GSIB_HC <- c(JPM = 1039502, BAC = 1073757, WFC = 1120754, C = 1951350)   # RSSD ids of the four GSIB holding companies
year_tbl <- function(y) {
  s <- readr::read_csv(sprintf("data/raw/sod/sod_%d.csv", y), show_col_types = FALSE, col_types = readr::cols(.default = "c")) |>
    transmute(state = STALPBR, hc = as.numeric(RSSDHCR), dep = as.numeric(DEPSUMBR))
  s <- s |> mutate(dep = pmin(dep, quantile(dep, 0.99, na.rm = TRUE)), region = unname(REGION[unname(DIVISION[state])])) |> filter(!is.na(region))
  s |> group_by(region) |> summarise(dep_B = sum(dep) / 1e6, gsib_share = 100 * sum(dep[hc %in% GSIB_HC]) / sum(dep), branches = dplyr::n(), .groups = "drop") |> mutate(year = y)
}
ctx <- bind_rows(lapply(c(2019, 2024, 2026), year_tbl)) |>
  pivot_wider(names_from = year, values_from = c(dep_B, gsib_share, branches)) |>
  transmute(region, dep_B_2026 = dep_B_2026, growth_2019_2026 = 100 * (dep_B_2026 / dep_B_2019 - 1), growth_2024_2026 = 100 * (dep_B_2026 / dep_B_2024 - 1),
            gsib_share_2019, gsib_share_2024, gsib_share_2026, branches_2026) |> arrange(match(region, c("South", "West", "Midwest", "Northeast")))
readr::write_csv(ctx |> mutate(across(where(is.numeric), ~ round(.x, 1))), "data/analysis/region_market_context.csv")
print(as.data.frame(ctx |> mutate(across(where(is.numeric), ~ round(.x, 1)))), row.names = FALSE)
