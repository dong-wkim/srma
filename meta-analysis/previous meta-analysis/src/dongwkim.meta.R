dongwkim.meta <- function(x) {
  results_path <- paste0("analyses/meta-analysis/meta",deparse(substitute(x)),".meta.csv")
  data <- x
  data.sqt <- x[x$graft_type == "S-QT", ]
  data.bqt <- x[x$graft_type == "B-QT", ]
  
  m.mean <- metamean(ni, mi, sdi, study, data = x, sm = "MRAW",
                     fixed = FALSE, random = TRUE, method.tau = "REML", tau.common = ifelse(k<10,TRUE,FALSE), method.random.ci = "HK")
  
  m.mean.sqt <- update(m.mean, data = data.sqt)
  m.mean.bqt <- update(m.mean, data = data.bqt)
  
  result <- rbind(
    c(m.mean.sqt$TE.random, m.mean.sqt$lower.random, m.mean.sqt$upper.random), 
    c(m.mean.bqt$TE.random, m.mean.bqt$lower.random, m.mean.bqt$upper.random),
    c(m.mean$TE.random, m.mean$lower.random, m.mean$upper.random)
  )
  
  rownames(result) <- c("S-QT","B-QT","QT")
  colnames(result) <- c("estimate","ci.lower","ci.upper")
  
  var <- paste0(deparse(substitute(x)),".result")
  assign(var,result,envir=.GlobalEnv)
  write.csv(result, file = results_path, row.names = TRUE)
}