source('./helpers/forms.R')
clim_check <- function(input, output) {
  if(!is.null(input$clim_data)) {
    sensors <- input$clim_data$datapath %>%
      scan(nlines=1, what=character(), sep=",")
    sensors <- sensors[-1]
    nsensors <- length(sensors)
    if(length(unique(sensors)) != nsensors) {
      return(renderUI({
        showModal(modalDialog(
          title = "Environment SensorIDs not unique",
          "All of the environment SensorIDs should be unique", br(),
          "Please modify the column headers accordingly."
        ))
      }))
    }
    clim_data <- formClimData(input)
  } else {
    return(renderUI({
      showModal(modalDialog(
        title = "Environmental data missing",
        "For the drought 2018 study we also need some weather/environemental variables", br(),
        "We need at least temperature and one hydrological variable (local precipitation or relative humidity)", br(),
        "Please go to the Environmental tab and provide the approiate series"
      ))
    }))
  }
  if(is.null(clim_data)) {
    return(renderUI({
      showModal(modalDialog(
        title = "Environmental data format invalid",
        "We are expecting specific .csv file format.", br(),
        "Please consult Instructions for the details."
      ))
    }))
  }
  if(!dendrometeR::is.dendro(clim_data)) {
    return(renderUI({
      showModal(modalDialog(
        title = "Environmental data timestamp invalid",
        "We are expecting specific timestamp format in accordance to the ISO8601.", br(),
        "Please consult Instructions for the details."
      ))
    }))
  }
}
