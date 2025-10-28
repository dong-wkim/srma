library(here)
scripts_path <- here("scripts")
files <- list.files(scripts_path, pattern = "\\.R$", full.names = TRUE)
sapply(files, source)