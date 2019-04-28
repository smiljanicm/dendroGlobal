contributor_check <- function(input, output) {
  if(is.null(input$contributors)) {
    return(renderUI({
      showModal(modalDialog(
        title = "Contributor data missing",
        "Please provide your information for author list ", br(),
        "and further contact if we need further information about your sites."
      ))
    }))
  }
  
  if(length(input$contributors$params$data) == 0) {
    return(renderUI({
      showModal(modalDialog(
        title = "All contributor rows deleted",
        "We need at least one contributor for each site.", br(), 
        "Please provide your information at the Contributors tab.", br(),
        "You can Undo the Remove row, from the context menu (right click)."
      ))
    }))
  }
  
  source('./helpers/rhandsontable_todf.R')
  contributors <- rhandsontable_todf(input$contributors)
  contributors$Manager[is.na(contributors$Manager)] <- 'FALSE'
  contributors$Owner[is.na(contributors$Owner)] <- 'FALSE'
  
  
  if(any(is.na(contributors$First.Name)) ||
     any(contributors$First.Name == '')) {
    return(renderUI({
      showModal(modalDialog(
        title = "Contributor metadata incomplete",
        "First Name was not entered for all contributors", br(),
        "Please go back to the Contributors tab and enter missing metadata."
      ))
    }))
  }
  
  if(any(is.na(contributors$Last.Name)) ||
     any(contributors$Last.Name == '')) {
    return(renderUI({
      showModal(modalDialog(
        title = "Contributor metadata incomplete",
        "Last Name was not entered for all contributors", br(),
        "Please go back to the Contributors tab and enter missing metadata."
      ))
    }))
  }
  
  if(any(is.na(contributors$Affiliation)) ||
     any(contributors$Affiliation == '')) {
    return(renderUI({
      showModal(modalDialog(
        title = "Contributor metadata incomplete",
        "Affiliation was not entered for all contributors", br(),
        "Please go back to the Contributors tab and enter missing metadata."
      ))
    }))
  }
  
  if(any(is.na(contributors$Email)) ||
     any(contributors$Email == '')) {
    return(renderUI({
      showModal(modalDialog(
        title = "Contributor metadata incomplete",
        "Email was not entered for all contributors", br(),
        "Please go back to the Contributors tab and enter missing metadata."
      ))
    }))
  } else if(any(!grepl('@', contributors$Email))) {
    return(renderUI({
      showModal(modalDialog(
        title = "Contributor metadata incomplete",
        "At least one email entered for contributors was not valid", br(),
        "Please go back to the Contributors tab and enter missing metadata."
      ))
    }))
  }
  
  

  if(sum(as.logical(contributors$Manager)) < 1) {
    return(renderUI({
      showModal(modalDialog(
        title = "Contributor metadata incomplete",
        "We need at least one person of contact (Data Manager) in case we need help with your data", br(),
        "Please go back to the Contributors tab and enter missing metadata."
      ))
    }))
  }
  
  if(sum(as.logical(contributors$Owner)) < 1) {
    return(renderUI({
      showModal(modalDialog(
        title = "Contributor metadata incomplete",
        "Data owner is missing. Who holds rights to the data (most likely group lead)?", br(),
        "Please go back to the Contributors tab and enter missing metadata."
      ))
    }))
  }
  NULL
}
