source('./helpers/read_list.R')
source('./helpers/renderers.R')
dm_meta_table <- function(sensors, r_o=FALSE) {
  df <- data.frame(SensorID = if(r_o) 'DM01' else if(length(sensors)==0) NA else sensors, 
                     TreeID = ifelse(r_o, 'Tree01', NA),
                     Time_Zone = ifelse(r_o, 'UTC + 2', NA),
                     Type = ifelse(r_o, 'Radius/Point', NA),
                     Unit = ifelse(r_o, 'micrometer [um]', NA),
                     Measurements_ongoing = ifelse(r_o, 'Yes', NA),
                     Monitored_organ = ifelse(r_o, 'Stem', NA),
                     Cardinal_position = ifelse(r_o, 'N', NA),
                     Sensor_Height_m = ifelse(r_o, 1.3, NA),
                     Dead_bark_removal = FALSE,
                     Sensor_on_xylem = FALSE,
                     Manufacturer = ifelse(r_o, 'Ecomatik', NA),
                     Model = ifelse(r_o, 'DR', NA),
                     Datalogger = ifelse(r_o, 'CR1000', NA),
                     Notes = ifelse(r_o, 'Further comments', NA))
  print(df)
  if(is.null(sensors)) {
    df_names <- names(df)
    df <- as.data.frame(matrix(nrow=0, ncol=ncol(df)))
    colnames(df) <- df_names
  }
  df_cols = c('SensorID', 'TreeID',
               'Time Zone', 'Type',
               'Unit', 'Measurements\n ongoing',
               'Monitored\n organ', 'Cardinal\n position',
               'Sensor\n Height [m]', 'Dead bark\n removal',
               'Sensor\n on xylem', 'Manufacturer',
               'Model', 'Datalogger', 'Notes')
  renderRHandsontable({
    rhandsontable(df, 
                  rowHeaders = NULL, width='100%', useTypes = FALSE, 
                  hightlightCol=TRUE, highlightRow = TRUE, overflow='visible', 
                  contextMenu = FALSE, colHeaders = df_cols) %>%
      hot_cols(colWidths = 125) %>%
      hot_rows(rowHeights = 35) %>%
      hot_col("SensorID", readOnly = TRUE, renderer = text_renderer) %>%
      hot_col("TreeID", renderer = text_renderer) %>%
      hot_col("Time Zone", 'dropdown', renderer = dropdown_renderer, source = (unlist(read_list('lists/utc.csv'))), allowInvalid = FALSE) %>%
      hot_col("Type", 'dropdown', renderer = dropdown_renderer, source = c('Radius/Point', 'Circumference', 'Diameter', 'Circumference converted to radius', 'Circumference converted to diameter'), allowInvalid = FALSE) %>%
      hot_col("Unit", 'dropdown', renderer = dropdown_renderer, source = c('micrometer [um]', 'milimeter [mm]'), allowInvalid = FALSE) %>%
      hot_col("Measurements\n ongoing", renderer = dropdown_renderer, 'dropdown', source = c('Yes', 'No'), allowInvalid = FALSE) %>%
      hot_col("Monitored\n organ", renderer = dropdown_renderer, 'dropdown', source = c('Stem', 'Branch', 'Root'), allowInvalid = FALSE) %>%
      hot_col("Cardinal\n position", 'dropdown', source = c('North', 'West', 'East', 'South'), allowInvalid = FALSE) %>%
      hot_col("Sensor\n Height [m]", 'numeric', allowInvalid = FALSE)  %>%
      hot_col("Dead bark\n removal", 'checkbox') %>%
      hot_col("Sensor\n on xylem", 'checkbox') 
  })
}
