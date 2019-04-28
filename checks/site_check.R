site_check <- function(input, output) {
  siteID <- input$siteID
  if(siteID == "") {
    return(renderUI({
      showModal(modalDialog(
        title = "Site Name missing",
        "Please enter valid Site Name on the Site metadata tab!"
      ))
    }))
  }
  
  long <- input$long
  lat <- input$lat
  coord <- c(long, lat)
  coord_decimal <- vapply(coord,
                          function(x) {
                            if(grepl('\\.', x)) {
                              x <- strsplit(x, '.', fixed=TRUE)[[1]][[2]];
                              return(nchar(x))
                            } else {
                              return(0) 
                            } 
                          },
                          FUN.VALUE = numeric(1))
  lats_num <- as.numeric(lat)
  logs_num <- as.numeric(long)
  if(any(coord_decimal < 4) || any(c(lats_num < -90, lats_num>90, logs_num < -180, logs_num > 180))) {
    return(renderUI({
      showModal(modalDialog(
        title = "Longitude or Latitude invalid",
        "Please enter valid Longitude and Latitude coordinates", br(),
        "with minimal precision of 4 decimal places on the Site metadata tab."
      ))
    }))
  }
  NULL
}
