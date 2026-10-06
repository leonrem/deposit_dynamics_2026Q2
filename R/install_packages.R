# Install the R packages the project needs (run once). Needs R >= 4.3.
#
# The memo PDF also needs pandoc and a LaTeX engine with xelatex (RStudio bundles pandoc; TinyTeX or TeX Live provide LaTeX).
# Usage: Rscript R/install_packages.R

pkgs <- c("dplyr", "tidyr", "tibble", "purrr", "readr", "lubridate", "data.table", "jsonlite", "xml2", "httr2", "arrow",
          "ggplot2", "ggrepel", "patchwork", "knitr", "kableExtra", "rmarkdown", "pdftools")
missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org") else message("All packages already installed.")
