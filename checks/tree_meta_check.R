tree_meta_check <- function(input, output) {
  if(is.null(input$tree_meta)) {
    return(renderUI({
      showModal(modalDialog(
        title = "Tree metadata missing",
        "You need to enter tree metadata as well", br(),
        "Please go back to the Trees tab."
      ))
    }))
  }
  
  source("./helpers/rhandsontable_todf.R")
  tree_meta <- rhandsontable_todf(input$tree_meta)
  
  if(any(is.na(tree_meta$Species)) || 
     any(tree_meta$Species == '')) {
    return(renderUI({
      showModal(modalDialog(
        title = "Tree metadata incomplete",
        "Species was not entered for all trees", br(),
        "Please go back to the Trees tab and enter missing metadata."
      ))
    }))
  }
  NULL
}
