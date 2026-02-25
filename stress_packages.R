stress_packages <- c(
    # Base R (15 packages; datasets + translations have no R code)
    "base", "compiler", "graphics", "grDevices", "grid",
    "methods", "parallel", "splines", "stats", "stats4",
    "tcltk", "tools", "utils",
    # Recommended (15 packages)
    "boot", "class", "cluster", "codetools", "foreign",
    "KernSmooth", "lattice", "MASS", "Matrix", "mgcv",
    "nlme", "nnet", "rpart", "spatial", "survival",
    # Core / Infrastructure
    "Rcpp", "rlang", "vctrs", "glue", "cli",
    "withr", "lifecycle", "magrittr", "pillar", "crayon",
    # Data Manipulation
    "dplyr", "tidyr", "tibble", "readr", "stringr",
    "forcats", "lubridate", "purrr", "data.table", "janitor",
    # Visualization
    "ggplot2", "scales", "patchwork", "cowplot", "plotly",
    "leaflet", "ggridges", "viridis", "DT",
    # Shiny / Web
    "shiny", "shinydashboard", "bslib", "htmltools", "httpuv",
    "httr", "httr2", "plumber", "rvest", "xml2",
    # Modeling / Stats
    "caret", "recipes", "parsnip", "workflows", "yardstick",
    "glmnet", "lme4", "brms", "xgboost",
    # Devtools / Packaging
    "testthat", "tinytest", "devtools", "usethis", "roxygen2",
    "pkgdown", "covr", "remotes", "pak", "renv",
    # Data Import / Storage
    "readxl", "writexl", "DBI", "RSQLite", "duckdb",
    "arrow", "sparklyr", "pins", "qs2", "fst",
    # Time Series / Specialized
    "forecast", "zoo", "xts", "tsibble", "fable",
    "igraph", "sf", "terra", "sp", "rmarkdown",
    # String / Parsing / Lang
    "stringi", "jsonlite", "yaml", "digest", "R6",
    "xmlparsedata", "evaluate", "callr", "processx", "here",
    # Performance / Parallel
    "future", "furrr", "parallelly", "RcppParallel", "bench",
    "profvis", "memoise", "progress", "curl", "openssl"
)
