library(dplyr)

data_subsample <- function(dat, len = 500) {
  dat_row <- rownames(dat)
  rowID <- seq(1, nrow(dat), length=len)
  
  dat_row <- dat_row[rowID]
  print(dat_row)
  dat_sub <- dplyr::slice(dat, rowID)
  rownames(dat_sub) <- dat_row
  
  dat_sub
}