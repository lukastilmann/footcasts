library(renv)
# Initialize renv for your project
renv::init()
# Install packages
renv::install(c(
  "worldfootballR",
  "dplyr",
  "tidyr",
  "purrr",
  "readr",
  "ggplot2",
  "goalmodel",
  "fs",
  "lubridate",
  "stringdist",
  "clue",
  "shiny"
))
