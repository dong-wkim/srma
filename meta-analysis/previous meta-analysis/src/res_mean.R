# meta-analysis using 'metafor' package

res.mean <- function(x) {
  res <- rma(
    yi,
    vi,
    data = x
  )
  
  return(summary(res))
}

res.mean.subgroup <- function(x,y) {
  res <- rma(
    yi,
    vi,
    data = x,
    subset = graft_type == paste0(y)
  )
  
  return(summary(res))
}