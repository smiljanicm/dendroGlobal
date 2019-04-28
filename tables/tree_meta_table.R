source('./helpers/rhandsontable_todf.R')
tree_meta_table <- function(dm_meta, r_o = FALSE) {
  dm_meta <- rhandsontable_todf(dm_meta)
  all_trees <- dm_meta$TreeID
  
  if(sum(is.na(all_trees)) == 0) {
    df <- data.frame(TreeID = unique(all_trees), 
                     Species = NA,
                     Social_status = NA,
                     DBH_cm = NA,
                     Tree_Height_m = NA,
                     Crown_Length_m = NA,
                     Notes = NA)
    colH <- c('TreeID', 'Species', 
              'Social\n status', 'DBH [cm]', 
              'Tree\n Height [m]','Crown\n Length [m]', 
              'Notes')
    return(renderRHandsontable({
      rhandsontable(df, 
      rowHeaders = NULL, colHeaders = colH, width='100%', overflow='visible', useTypes = FALSE) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_cols(colWidths = c(200,200,100,100,100,100,100,100,250)) %>%
        hot_rows(rowHeights = 35) %>%
        hot_col(colH[[4]], 'numeric', allowInvalid = FALSE) %>%
        hot_col(colH[[5]], 'numeric', allowInvalid = FALSE)   %>%
        hot_col(colH[[6]], 'numeric', allowInvalid = FALSE)   %>%
        hot_col(colH[[3]], 'dropdown', source = c('Dominant', 'Co-dominant', 'Dominated', 'Suppressed')) %>%
        hot_col(colH[[1]], readOnly = TRUE, renderer = text_renderer) %>%
        hot_col(colH[[2]], renderer = dropdown_renderer, 'dropdown', source = sort(unlist(read_list('lists/species.csv'))))
    }))
  }
}
