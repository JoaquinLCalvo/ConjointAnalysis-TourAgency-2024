# List of required packages
required_packages <- c(
  "mlogit", "dfidx", "lubridate", "forcats", "stringr", "dplyr", "purrr",
  "readr", "tidyr", "tibble", "ggplot2", "tidyverse", "gtable", "jsonlite",
  "compiler", "tidyselect", "scales", "statmod", "lattice", "R6", "generics",
  "lmtest", "Formula", "MASS", "rbibutils", "munsell", "pillar", "tzdb",
  "rlang", "utf8", "stringi", "timechange", "cli", "withr", "magrittr",
  "Rdpack", "grid", "hms", "lifecycle", "vctrs", "glue", "zoo", "fansi",
  "colorspace", "tools", "pkgconfig", "evaluate", "knitr", "htmltools",
  "rmarkdown", "fastmap", "evaluate", "xfun", "rstudioapi"
)

# Function to install missing packages
install_missing_packages <- function(packages) {
  for (package in packages) {
    if (!(package %in% installed.packages()[, "Package"])) {
      install.packages(package)
    }
  }
}

# Install the required packages
install_missing_packages(required_packages)

# Clean environment
rm(required_packages, install_missing_packages)
