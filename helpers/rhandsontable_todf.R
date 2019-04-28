rhandsontable_todf <- function(table) {
  colH <- gsub('\\.+', '\\.', make.names(table$params$colHeaders))
  df <- t(stri_list2matrix(table$data))
  colnames(df) <- colH
  df[which(df == "NULL")] <- NA
  df[which(df == "")] <- NA
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  
  df
}
