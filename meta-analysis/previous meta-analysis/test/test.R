
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("scripts/load_scripts.R")

data <- read.csv("data/ikdc_subjective.csv")
forest.mean("ikdc_subjective")
meta.mean(data)
