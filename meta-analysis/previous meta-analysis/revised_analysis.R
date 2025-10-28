# The analysis script used to perform analyses - update.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory
#source("scripts/load_scripts.R") # load custom scripts automatically
#source("config.R")

source("scripts/packages.R") # install packages and load libraries

# Load datasets

h_lsi_isokinetic_180 <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/continuous/h_lsi_isokinetic_180.csv")
h_lsi_isokinetic_60 <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/continuous/h_lsi_isokinetic_60.csv")
h_lsi_isometric <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/continuous/h_lsi_isometric.csv")
ikdc_subjective <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/continuous/ikdc_subjective.csv")
instrumental_laxity <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/continuous/instrumental_laxity.csv")
koos_adl <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/continuous/koos_adl.csv")
koos_pain <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/continuous/koos_pain.csv")
koos_qol <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/continuous/koos_qol.csv")
koos_sport <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/continuous/koos_sport.csv")
koos_symptoms <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/continuous/koos_symptoms.csv")
lysholm <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/continuous/lysholm.csv")
marx <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/continuous/marx.csv")
q_lsi_isokinetic_180 <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/continuous/q_lsi_isokinetic_180.csv")
q_lsi_isokinetic_60 <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/continuous/q_lsi_isokinetic_60.csv")
q_lsi_isometric <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/continuous/q_lsi_isometric.csv")
slht <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/continuous/slht.csv")
tegner <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/continuous/tegner.csv")
vas <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/continuous/vas.csv")

arthrofibrosis <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/dichotomous/arthrofibrosis.csv")
donor_site_morbidity <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/dichotomous/donor_site_morbidity.csv")
graft_rupture <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/dichotomous/graft_rupture.csv")
ikdc_objective <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/dichotomous/ikdc_objective.csv")
lachman <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/dichotomous/lachman.csv")
lachman_0_1 <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/dichotomous/lachman_0_1.csv")
patellar_fracture <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/dichotomous/patellar_fracture.csv")
pivot_shift <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/dichotomous/pivot_shift.csv")
pivot_shift_0_1 <- read.csv("G:/My Drive/.github/meta-analysis/data/csv/dichotomous/pivot_shift_0_1.csv")

# Load them into global environment as variables

continuous <- list(h_lsi_isokinetic_180, h_lsi_isokinetic_60, h_lsi_isometric, 
                   ikdc_subjective, 
                   instrumental_laxity, 
                   koos_adl, koos_pain, koos_qol, koos_sport, koos_symptoms, 
                   lysholm, 
                   marx, 
                   q_lsi_isokinetic_180, q_lsi_isokinetic_60, q_lsi_isometric, 
                   slht, 
                   tegner, 
                   vas)

# META-ANALYSIS OF MEANS -----------------------------------------------------#

# Method 1: using meta for means
source("scripts/meta_mean.R")
meta.mean(ikdc_subjective, "graft_type")

# Method 2: using metafor for means
source("scripts/res_mean.R")
res.mean(df)
res.sqt <- res.mean.subgroup(df, "S-QT")
res.bqt <- res.mean.subgroup(df, "B-QT")

# META-ANALYSIS OF PROPORTIONS -----------------------------------------------#

# Method 1: using meta for proportions
source("scripts/meta_prop.R")
meta.prop(ikdc_objective, "graft_type")


# FOREST PLOTS ---------------------------------------------------------------#

continuous <- c(
  "h_lsi_isokinetic_60",
  "h_lsi_isokinetic_180",
  "h_lsi_isometric",
  "ikdc_subjective",
  "instrumental_laxity",
  "koos_adl",
  "koos_pain",
  "koos_qol",
  "koos_sport",
  "koos_symptoms",
  "lysholm",
  "marx",
  "slht",
  "q_lsi_isokinetic_60",
  "q_lsi_isokinetic_180",
  "q_lsi_isometric",
  "tegner",
  "vas"
)

dichotomous <- c(
  "arthrofibrosis",
  "donor_site_morbidity",
  "graft_rupture",
  "ikdc_objective",
  "lachman",
  "lachman_0_1",
  "patellar_fracture",
  "patellar_fracture_null",
  "pivot_shift",
  "pivot_shift_0_1"
)

source("scripts/forest.R")
lapply(continuous, forest.mean)
lapply(dichotomous, forest.prop)
forest.prop("patellar_fracture_null")

