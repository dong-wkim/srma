meta.prop <- function(x, y = null) {
  if (!is.null(y)) {
    y <- NULL
    subgroup <- NULL
  } else {
    subgroup <- if (is.character(y))
      y
    else
      deparse(substitute(y))
    y <- x[[subgroup]]
  }
  
  meta.x <- metaprop(event = xi, 
                     n = ni, 
                     studlab = study, 
                     data = data, 
                     method = "GLMM", 
                     sm = "PLOGIT",
                     common = FALSE, 
                     random = TRUE, 
                     method.random.ci = "HK", 
                     tau.common = FALSE,
                   subgroup = y,
                   subgroup.name = subgroup)
  
  return(summary(meta.x))
}