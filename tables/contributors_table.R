source('./helpers/rhandsontable_todf.R')
contributors_table <- function(r_o = FALSE) {
  
  df <- data.frame(First_Name = NA,
                   Last_Name = NA,
                   Affiliation = NA,
                   Address = NA,
                   Email = NA,
                   Owner = TRUE,
                   Manager = TRUE)
  colH <- c('First Name', 'Last Name', 
            'Affiliation', 'Address',
            'Email', 'Owner',
            'Manager')
  return(renderRHandsontable({
    rhandsontable(df, 
                  rowHeaders = NULL, colHeaders = colH, width='100%', overflow='visible', useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) %>%
      hot_cols(colWidths = c(rep(150,5), rep(100,3))) %>%
      hot_col('First Name', renderer = text_renderer) %>%
      hot_col('Last Name', renderer = text_renderer) %>%
      hot_col('Affiliation', renderer = text_renderer) %>%
      hot_col('Email', renderer = text_renderer) %>%
      hot_col('Owner', type='checkbox', halign = 'htCenter') %>%
      hot_col('Manager', type='checkbox', halign = 'htCenter') %>%
      hot_rows(rowHeights = 35)
  }))
}
