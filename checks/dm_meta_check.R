source('./helpers/rhandsontable_todf.R')
dm_meta_check <- function(input, output) {
  if(is.null(input$dm_meta)) {
    return(renderUI({
      showModal(modalDialog(
        title = "Dendrometer metadata missing",
        "You need to enter dendrometer metadata as well", br(),
        "Please go back to the Dendrometers tab."
      ))
    }))
  }
  dm_meta <- rhandsontable_todf(input$dm_meta)
  if(any(is.na(dm_meta$TreeID)) ||
     any(dm_meta$TreeID == '')) {
    return(renderUI({
      showModal(modalDialog(
        title = "Dendrometer metadata incomplete",
        "Tree ID was not entered for all dendrometers", br(),
        "Please go back to Dendrometers tab and enter missing metadata."
      ))
    }))

  }
  
  if(any(is.na(dm_meta$Time.Zone)) ||
     any(dm_meta$Time.Zone == '')) {
    return(renderUI({
      showModal(modalDialog(
        title = "Dendrometer metadata incomplete",
        "Timezone offset was not entered for all dendrometers", br(),
        "Please go back to Dendrometers tab and enter missing metadata."
      ))
    }))
  }
  
  if(length(unique(dm_meta$Time.Zone)) != 1) {
    return(renderUI({
      showModal(modalDialog(
        title = "Dendrometer metadata incomplete",
        "More than one Time Zone offsets in your data files. It should be uniform for all dendrometer sensors on one site.", br(),
        "Please either go back to the Dendrometers tab and reupload the corrected datafile or correct Time Zone offsets."
      ))
    }))
    
  }
  
  if(any(is.na(dm_meta$Type)) ||
     any(dm_meta$Type == '')) {
    return(renderUI({
      showModal(modalDialog(
        title = "Dendrometer metadata incomplete",
        "Dendrometer type was not entered for all dendrometers", br(),
        "Please go back to Dendrometers tab and enter missing metadata."
      ))
    }))
  }

  if(any(is.na(dm_meta$Unit)) ||
     any(dm_meta$Unit == '')) {
    return(renderUI({
      showModal(modalDialog(
        title = "Dendrometer metadata incomplete",
        "Measurement unit was not entered for all dendrometers", br(),
        "Please go back to Dendrometers tab and enter missing metadata."
      ))
    }))
  }

  if(any(is.na(dm_meta$Monitoring.ongoing)) ||
     any(dm_meta$Monitoring_ongoing == '')) {
    return(renderUI({
      showModal(modalDialog(
        title = "Dendrometer metadata incomplete",
        "Is the monitoring active (ongoing) was not provided for all dendrometers", br(),
        "Please go back to Dendrometers tab and enter missing metadata."
      ))
    }))
  }
  
  if(any(is.na(dm_meta$Monitored.organ)) ||
     any(dm_meta$Monitored_organ == '')) {
    return(renderUI({
      showModal(modalDialog(
        title = "Dendrometer metadata incomplete",
        "Monitored organ was not entered for all dendrometers", br(),
        "Please go back to Dendrometers tab and enter missing metadata."
      ))
    }))
  }
  NULL
}
