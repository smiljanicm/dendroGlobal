dm_check <- function(input, output) {
  source('./helpers/forms.R')
  if(!is.null(input$data)) {
    sensors <- input$data$datapath %>%
      scan(nlines=1, what=character(), sep=",")
    sensors <- sensors[-1]
    nsensors <- length(sensors)

    if(length(unique(sensors)) != nsensors) {
      return(renderUI({
        showModal(modalDialog(
          title = "Dendrometer SensorIDs not unique",
          "All of the dendrometer SensorIDs should be unique", br(),
          "Please modify the column headers accordingly."
        ))
      }))
    }
    
    dm_data <- formData(input)
  } else {
    return(renderUI({
      showModal(modalDialog(
        title = "Dendrometer data missing",
        "Dendrometer data is missing for some reason", br(),
        "Please try again..."
      ))
    }))
  }
  if(is.null(dm_data)) {
    return(renderUI({
      showModal(modalDialog(
        title = "Dendrometer data format invalid",
        "We are expecting specific .csv file format.", br(),
        "Please consult Instructions for the details."
      ))
    }))
  }
  if(!dendrometeR::is.dendro(dm_data)) {
    return(renderUI({
      showModal(modalDialog(
        title = "Invalid dendrometer data",
        "Dendrometer data were unable to pass dendrometeR::is.dendro test", br(),
        "Please try again with appropriate data format as described in instructions."
      ))
    }))
  }
  NULL
}
