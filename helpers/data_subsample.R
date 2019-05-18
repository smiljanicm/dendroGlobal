library(dplyr)

data_subsample <- function(dat, len = 500) {
  dat_row <- rownames(dat)
  if(len < length(dat_row)) {
    rowID <- seq(1, nrow(dat), length=len)
  } else {
    rowID <- 1:nrow(dat)
  }
  
  dat_row <- dat_row[rowID]
  dat_sub <- dplyr::slice(dat, rowID)
  rownames(dat_sub) <- dat_row
  
  dat_sub
}