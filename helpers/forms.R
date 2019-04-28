formData <- function(input) {
  if(is.null(input$data)) {
    return(NULL)
  }
  
  sensors <- input$data$datapath %>%
    scan(nlines=1, what=character(), sep=",")
  sensors <- sensors[-1]
  nsensors <- length(sensors)
  if(nsensors == 0) return(NULL)
  try({dm_data <- read_csv(input$data$datapath, col_types = paste0(c('T', rep('d', nsensors)), collapse=''))
  dm_data <- as.data.frame(dm_data)
  rownames(dm_data) <- dm_data[[1]]
  dm_data[1] <- NULL
  return(dm_data) })
  return(NULL)
}

formClimData <- function(input) {
  if(is.null(input$clim_data)) {
    return(NULL)
  }
  sensors <- input$clim_data$datapath %>%
    scan(nlines=1, what=character(), sep=",")
  sensors <- sensors[-1]
  nsensors <- length(sensors)
  if(nsensors == 0) return(NULL)
  try({clim_data <- read_csv(input$clim_data$datapath, col_types = paste0(c('T', rep('d', nsensors)), collapse=''))
  clim_data <- as.data.frame(clim_data)
  rownames(clim_data) <- clim_data[[1]]
  clim_data[1] <- NULL
  return(clim_data) })
  return(NULL)
}
