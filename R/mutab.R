library(Rcpp)
library(haven)
library(labelled)
library(dplyr)
library(purrr)

sourceCpp("count_mrtab.cpp")

#dat <- read_sav('./data-raw/data_sveta.sav')

mutab_labelled <- function(dfLabelled, countValue, extractLabelPattern = NULL) {
  
  input_labels <- var_label(dfLabelled)
  
  if (is.null(extractLabelPattern)==F) {
    input_labels <- input_labels %>%
      gsub(extractLabelPattern, "\\1", .)
  }
  
  df <- dfLabelled %>%
    map(\(x) {
      numcol <- as.numeric(x)
    }) %>%
    bind_cols()
  
  freq <- create_mr_table(df, countValue)
  
  tab_out <- freq %>%
    mutate(labels = input_labels) %>%
    relocate(labels, .after = 2) 
  
  return(tab_out)
  
}

freq2 <- mutab_labelled(select(dat, starts_with('Q4a')), 1,
                        "^(.*):.*")

dat$status

x <- dat[as.numeric(dat$status)==1,]


