variables <- function(x,y) {
  list2env(setNames(lapply(x, function(col) na.omit(y[[col]])),x), envir= .GlobalEnv)
}

# where:
# x = vector list of column names from dataset
# y = dataframe

# Example:
# df <- read.csv("./week_4.csv")
# columns <- c("critic_score", "total_sales", "na_sales", "jp_sales", "pal_sales", "other_sales")
# d.vectorize(columns,df)