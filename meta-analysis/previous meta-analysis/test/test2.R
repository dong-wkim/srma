# meta-analysis using 'metafor' package
library(metafor)

df <- read.csv("data/ikdc_subjective.csv")
View(df)

res.mean <- function(x) {
  res <- rma(
    yi,
    vi,
    data = x
  )
  
  return(summary(res))
}

res.mean(df)

# x = data frame
# y = column name of subgroup (e.g., "gender", "graft_type")
# z = subgroup of interest (e.g., "female", "S-QT")

res.mean.subgroup <- function(x,y) {
  res <- rma(
    yi,
    vi,
    data = x,
    subset = graft_type == paste0(y)
  )
  
  return(summary(res))
}


df <- read.csv("data/ikdc_subjective.csv")

res.mean.subgroup(df,"S-QT")
res.mean.subgroup(df,"B-QT")



reg <- rma(yi, vi, data = data, mods = ~ graft_type)