packages <- c(
  "googledrive",
  "googlesheets4",
  "readxl",
  "metafor",
  "dplyr",
  "meta",
  "ggplot2",
  "ggpubr",
  "ggforestplot",
  "ggplotify",
  "ggplot2",
  "tidyverse",
  "devtools",
  "svglite",
  "here"
)

for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE))
    install.packages(package)
}

devtools::install_github("NightingaleHealth/ggforestplot")
lapply(packages, library, character.only = TRUE)