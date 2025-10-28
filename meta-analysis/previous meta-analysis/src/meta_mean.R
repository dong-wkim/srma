meta.mean <- function(x) {
    result <- metamean(
      n = ni, 
      mean = mi, 
      sd = sdi, 
      studlab = study, 
      data = x,
      sm = "MRAW",
      fixed = FALSE, 
      random = TRUE, 
      tau.common = TRUE,
      method.tau = "REML", 
      method.random.ci = "HK")
    
    return(summary(result))
  }

# There are two analyses (without and with subgroup analyses), each with two-three methods of doing them.
# 1. meta-analysis of means
#2

# meta-analysis of means using 'meta' package (without subgroup analysis)
m.mean <- metamean(ni, mi, sdi, study, data, sm = "MRAW",
                     fixed = FALSE, random = TRUE, method.tau = "REML", tau.common = ifelse(k<10,TRUE,FALSE), 
                     hakn = TRUE, method.random.ci = "HK")


res.sqt <- rma(yi, vi, data = data, subset = graft_type == "S-QT")
  res.bqt <- rma(yi, vi, data = data, subset = graft_type == "B-QT")
  reg <- rma(yi, vi, data = data, mods = ~ graft_type)