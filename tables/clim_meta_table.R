source('./helpers/read_list.R')
source('./helpers/renderers.R')
clim_meta_table <- function(sensors, r_o=FALSE, siteID=input$siteID) {
  df <- data.frame(SensorID = if(r_o) 'Sens01' else if(length(sensors)==0) NA else sensors,
                   Parameter = ifelse(r_o, 'Air Temperature', NA),
                   Time_Zone = ifelse(r_o, 'UTC + 2', NA),
                   Site_Name = ifelse(r_o, 'TestSite', siteID),
                   Unit = ifelse(r_o, 'degC', NA),
                   Sensor_Height_m = ifelse(r_o, 2, NA),
                   Manufacturer = ifelse(r_o, 'Campbell', NA),
                   Model = ifelse(r_o, '', NA),
                   Datalogger = ifelse(r_o, 'CR1000', NA),
                   Notes = ifelse(r_o, 'Further comments', NA))
  colH = c('SensorID', 'Parameter',
           'Time Zone', 'Site Name',
           'Unit', 'Sensor\n Height [m]',
           'Manufacturer', 'Model',
           'Datalogger', 'Notes')
  if(is.null(sensors)) {
    df_names <- names(df)
    df <- as.data.frame(matrix(nrow=0, ncol=ncol(df)))
    colnames(df) <- df_names
  }
  renderRHandsontable({
    rhandsontable(df, 
                  rowHeaders = NULL, colHeaders = colH, width='100%', 
                  hightlightCol=TRUE, highlightRow = TRUE, overflow='visible', 
                  contextMenu = FALSE, useTypes = FALSE) %>%
      hot_cols(colWidths = c(200,200,100,100,100,100,100,100,125,100,100,100,100,250)) %>%
      hot_rows(rowHeights = 35) %>%
      hot_col(colH[[1]], readOnly = TRUE, renderer = text_renderer) %>%
      hot_col(colH[[2]], 'dropdown', renderer=dropdown_renderer,
              source = c('Air Temperature', 
                         'Precipitation',
                         'Relative Humidity',
                         'Barometric pressure',
                         'PAR',
                         'Quantum radiation',
                         'NDVI',
                         'Soil Volumetric Water Content',
                         'Soil Moisture',
                         'Soil Temperature',
                         'Wind Speed',
                         'Wind direction',
                         'Other')) %>%
      hot_col(colH[[3]], 'dropdown', renderer=dropdown_renderer, 
              source = unlist(read_list('lists/utc.csv')), allowInvalid = FALSE) %>%
      hot_col(colH[[4]], renderer = text_renderer) %>%
      hot_col(colH[[5]], renderer = text_renderer) %>%
      hot_col(colH[[6]], 'numeric', allowInvalid = FALSE)
  })
}
