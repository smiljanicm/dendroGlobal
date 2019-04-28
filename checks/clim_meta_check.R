source('./helpers/forms.R')
clim_meta_check <- function(input, output) {
  if(is.null(input$clim_meta)) {
    return(renderUI({
      showModal(modalDialog(
        title = "Environmental metadata missing",
        "You should also enter Environmental metadata similar to dendrometer metadata", br(),
        "Please go back to the Environmental tab."
      ))
    }))
  } else {
    source('./helpers/rhandsontable_todf.R')
    clim_meta <- rhandsontable_todf(input$clim_meta)
    
    if(any(is.na(clim_meta$Parameter)) ||
       any(clim_meta$Parameter == '')) {
      return(renderUI({
        showModal(modalDialog(
          title = "Environmental metadata incomplete",
          "Parameter type was not provided for all sensors", br(),
          "Please go back to Environmental tab and enter missing metadata."
        ))
      }))
    }
    
    if(any(is.na(clim_meta$Time.Zone)) ||
       any(clim_meta$Time.Zone == '')) {
      return(renderUI({
        showModal(modalDialog(
          title = "Environmental metadata incomplete",
          "Time Zone offset was not provided for all sensors", br(),
          "Please go back to Environmental tab and enter missing metadata."
        ))
      }))
    }
    
    if(any(is.na(clim_meta$Site.Name)) ||
       any(clim_meta$Site.Name == '')) {
      return(renderUI({
        showModal(modalDialog(
          title = "Environmental metadata incomplete",
          "Site Name was not provided for all sensors", br(),
          "Please go back to Environmental tab and enter missing metadata."
        ))
      }))
    }
    
    if(any(is.na(clim_meta$Unit)) ||
       any(clim_meta$Unit == '')) {
      return(renderUI({
        showModal(modalDialog(
          title = "Environmental metadata incomplete",
          "Unit was not provided for all sensors", br(),
          "Please go back to Environmental tab and enter missing metadata."
        ))
      }))
    }
    
    ##### drought 2018
    if(!('Air Temperature' %in% clim_meta$Parameter)) {
      return(renderUI({
        showModal(modalDialog(
          title = "Environmental data missing",
          "For the drought 2018 study we also need some Environemental variables", br(),
          "We need at least temperature and one hydrological variable (local precipitation or relative humidity)", br(), 
          "Please go to the Environmental tab and provide the appropriate series"
        ))
      }))
    }
    
    if(!any(('Relative Humidity' %in% clim_meta$Parameter),
            ('Precipitation' %in% clim_meta$Parameter))) {
      
      return(renderUI({
        showModal(modalDialog(
          title = "Environmental data missing",
          "For the drought 2018 study we also need some Environemental variables", br(),
          "We need at least temperature and one hydrological variable (local precipitation or relative humidity)", br(), 
          "Please go to the Environmental tab and provide the appropriate series"
        ))
      }))
    }
  }
  NULL
}
