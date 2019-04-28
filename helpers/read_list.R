read_list <- function(filename = 'lists/countries.csv') {
  list <- read.csv(filename)
  
  lst <- unlist(list)
  
  names(lst) <- NULL
  as.list(lst)
}
