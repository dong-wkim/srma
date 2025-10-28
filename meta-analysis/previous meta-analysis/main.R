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

## Code
data <- ikdc_subjective
data.sqt <- data[data$graft_type == "S-QT", ]
data.bqt <- data[data$graft_type == "B-QT", ]

meta.mean <- function(x) {
  k <- nrow(x)
  result <- metamean(ni, mi, sdi, study, data=x, sm = "MRAW",
                   common = FALSE, 
                   random = TRUE)
  
  return(c(result$TE.random, result$lower.random, result$upper.random))
}

x <- list(data, data.sqt, data.bqt)

lapply(x,meta.mean)

# , 
# method.tau = "REML", 
# tau.common = ifelse(k<10,TRUE,FALSE), 
# method.random.ci = "HK")

# Method 2: using metafor for means using custom function
source("scripts/res_mean.R")
res.mean(df)
res.sqt <- res.mean.subgroup(df, "S-QT")
res.bqt <- res.mean.subgroup(df, "B-QT")

## Code

data <- ikdc_subjective
res.sqt <- rma(yi, vi, data = data, subset = graft_type == "S-QT")
res.bqt <- rma(yi, vi, data = data, subset = graft_type == "B-QT")
res.sqt$beta
res.sqt$ci.lb
res.sqt$ci.ub

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
source("scripts/2025-08-13_forest_plot.R")
source("scripts/dongwkim.meta.R")
lapply(continuous,dongwkim.meta)
lapply(continuous, forest.mean)
lapply(dichotomous, forest.prop)


meta.mean <- function(x) {
  data <- x
  m.mean <- metamean(ni, 
                     mi, 
                     sdi, 
                     study, 
                     data = data, 
                     sm = "MRAW",
                     common = FALSE, 
                     random = TRUE)
  
  data.sqt <- data[data$graft_type == "S-QT", ]
  m.mean.sqt <- metamean(ni, 
                     mi, 
                     sdi, 
                     study, 
                     data = data.sqt, 
                     sm = "MRAW",
                     common = FALSE, 
                     random = TRUE)
  
  data.bqt <- data[data$graft_type == "B-QT", ]
  m.mean.bqt <- metamean(ni, 
                         mi, 
                         sdi, 
                         study, 
                         data = data.bqt, 
                         sm = "MRAW",
                         common = FALSE, 
                         random = TRUE, 
                         method.tau = "REML", 
                         tau.common = FALSE)
  
  result <- rbind(
      c(m.mean.sqt$TE.random, m.mean.sqt$lower.random, m.mean.sqt$upper.random), 
      c(m.mean.bqt$TE.random, m.mean.bqt$lower.random, m.mean.bqt$upper.random),
      c(m.mean$TE.random, m.mean$lower.random, m.mean$upper.random)
    )
    
    rownames(result) <- c("S-QT","B-QT","QT")
    colnames(result) <- c("estimate","ci.lower","ci.upper")
  
  results_path <- paste0("analyses/meta-analysis/meta/",deparse(substitute(x)),".csv")
  write.csv(result,file=results_path,row.names = TRUE)
  return(as.data.frame(result))
}

meta.prop <- function(x) {
  data <- x
  m.prop <- metaprop(event = xi,
                     n = ni,
                     study,
                     data, 
                     method = "GLMM", 
                     sm = "PLOGIT", 
                     common = FALSE, 
                     random = TRUE,method.random.ci = "HK", tau.common = FALSE)
  
  
  data.sqt <- data[data$graft_type == "S-QT", ]
  m.prop.sqt <- metaprop(event = xi,
                     n = ni,
                     study,
                     data.sqt, 
                     method = "GLMM", 
                     sm = "PLOGIT", 
                     common = FALSE, 
                     random = TRUE,method.random.ci = "HK", tau.common = FALSE)
  
  data.bqt <- data[data$graft_type == "B-QT", ]
  m.prop.bqt <- metaprop(event = xi,
                     n = ni,
                     study,
                     data.bqt, 
                     method = "GLMM", 
                     sm = "PLOGIT", 
                     common = FALSE, 
                     random = TRUE,method.random.ci = "HK", tau.common = FALSE)
  
  result <- rbind(
    c(m.prop.sqt$TE.random, m.prop.sqt$lower.random, m.prop.sqt$upper.random), 
    c(m.prop.bqt$TE.random, m.prop.bqt$lower.random, m.prop.bqt$upper.random),
    c(m.prop$TE.random, m.prop$lower.random, m.prop$upper.random)
  )
  
  rownames(result) <- c("S-QT","B-QT","QT")
  colnames(result) <- c("estimate","ci.lower","ci.upper")
  
  results_path <- paste0("analyses/meta-analysis/meta/",deparse(substitute(x)),".csv")
  write.csv(result,file=results_path,row.names = TRUE)
  return(as.data.frame(result))
}

a <- meta.mean(ikdc_subjective)
b <- meta.mean(lysholm)
c <- meta.mean(tegner)
d <- meta.mean(marx)
e <- meta.mean(koos_adl)
f <- meta.mean(koos_pain)
g <- meta.mean(koos_sport)
h <- meta.mean(koos_symptoms)
i <- meta.mean(koos_qol)
j <- meta.mean(q_lsi_isometric)
k <- meta.mean(q_lsi_isokinetic_60)
l <- meta.mean(q_lsi_isokinetic_180)
m <- meta.mean(h_lsi_isometric)
n <- meta.mean(h_lsi_isokinetic_60)
o <- meta.mean(h_lsi_isokinetic_180)
p <- meta.mean(slht)
q <- meta.mean(instrumental_laxity)
r <- meta.prop(ikdc_objective)
s <- meta.prop(lachman)
s2 <- meta.prop(lachman_0_1)
t <- meta.prop(pivot_shift)
t2 <- meta.prop(pivot_shift_0_1)
u <- meta.mean(vas)
v <- meta.prop(graft_rupture)
w <- meta.prop(donor_site_morbidity)
x <- meta.prop(arthrofibrosis)
y <- meta.prop(patellar_fracture)
y
patellar_fracture
results <- rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,s2,t,t2,u,v,w,x,y)
write.csv(results, file = "analyses/meta-analysis/meta/results.csv", row.names = TRUE)
