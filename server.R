library(shiny)
options(shiny.maxRequestSize=500*1024^2)
library(readr)
library(stringi)
library(rhandsontable)
library(leaflet)
library(mapview)
library(dendrometeR)
library(xts)
library(knitr)

source('./helpers/rhandsontable_todf.R')
source('./helpers/read_list.R')
source('./helpers/renderers.R')

shinyServer(function(input, output, session) {
  observeEvent(input$site_next, {
    source('./checks/site_check.R')
    site_check_result <- site_check(input,output)
    if(!is.null(site_check_result)) {
      output$Site_check <- site_check_result
      return()
    } else {
      updateTabsetPanel(session, "Submition",
                        selected = "Dendrometers")
    }
  })
  
  observeEvent(input$dm_next, {
    source('./checks/dm_check.R')
    withProgress(({
      dm_check_result <- dm_check(input,output)
    }), value=0.5, message="Checking dendrometer data")
    if(!is.null(dm_check_result)) {
      output$Dm_check <- dm_check_result
      return()
    }
    source('./checks/dm_meta_check.R')
    withProgress(({
      dm_meta_check_result <- dm_meta_check(input,output)
    }), value=0.5, message="Checking dendrometer data")
    if(!is.null(dm_meta_check_result)) {
      output$Dm_check <- dm_meta_check_result
      return()
    } else {
      updateTabsetPanel(session, "dendroGlobal",
                       selected = "Trees")
    }
  })
  
  observeEvent(input$tree_next, {
    if(is.null(input$tree_meta)) {
      output$Tree_check <- renderUI({
        showModal(modalDialog(
          title = "Tree metadata missing",
          "You need to enter tree metadata as well", br(),
          "Please go back to the Trees tab."
        ))
      })
      return()
    }
    
    source('./checks/tree_meta_check.R')
    tree_meta_check_result <- tree_meta_check(input,output)
    if(!is.null(tree_meta_check_result)) {
      output$Tree_check <- tree_meta_check_result
      return()
    } else {
      updateTabsetPanel(session, "dendroGlobal",
                       selected = "Environment")
    }
  })
  
  observeEvent(input$clim_next, {
    source('./checks/clim_check.R')
    clim_check_result <- clim_check(input, output)
    if(!is.null(clim_check_result)) {
      output$Clim_check <- clim_check_result
      return()
    }
    source('./checks/clim_meta_check.R')
    withProgress(({
      clim_meta_check_result <- clim_meta_check(input,output)
    }), value=0.5, message = "Checking environmental data")
    if(!is.null(clim_meta_check_result)) {
      output$Clim_check <- clim_meta_check_result
      return()
    } else {
      updateTabsetPanel(session, "dendroGlobal",
                       selected = "Contributors")
    }
  })
  
  observeEvent(input$cont_next, {
    source('./checks/contributor_check.R')
    contributor_check_result <- contributor_check(input,output)
    if(!is.null(contributor_check_result)) {
      output$Cont_check <- contributor_check_result
      return()
    } else {
      updateTabsetPanel(session, "dendroGlobal",
                       selected = "Submit")
    }
  })
  
  output$data <- renderUI({
    fileInput("data", label='Upload dendrometer data:')
  })
  
  output$clim_data <- renderUI({
    fileInput("clim_data", label='Upload environmental data:')
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = matrix(as.numeric(c(input$long, input$lat)), ncol=2)) %>%
      setView(10, 50, zoom = 3) %>%
      addMouseCoordinates()
  })
  
  ##### Dendrometers #####
  source("./tables/dm_meta_table.R")
  output$dm_meta <- dm_meta_table(NA, r_o = TRUE)
  
  observeEvent(input$data, {
    source('./checks/dm_check.R')
    withProgress(({
      dm_check_result <- dm_check(input,output)
    }), value=0.5, message="Checking dendrometer data")
    if(!is.null(dm_check_result)) {
      output$Dm_check <- dm_check_result
      return()
    }
    source("./helpers/forms.R")
    dm_data <- formData(input)
    
    sensors <- input$data$datapath %>%
      scan(nlines=1, what=character(), sep=",")
    sensors <- sensors[-1]
    nsensors <- length(sensors)
    source("./tables/dm_meta_table.R")
    output$dm_meta <- dm_meta_table(sensors)
  })
  
  ##### Tree meta ####
  source('./tables/tree_meta_table.R')
  observeEvent(input$dm_meta, {
    output$tree_meta <- tree_meta_table(input$dm_meta)
  })
  
  ##### Weather data #####
  source("./tables/clim_meta_table.R")
  output$clim_meta <- clim_meta_table(NA, r_o = TRUE)
  
  observeEvent(input$clim_data, {
    source('./checks/clim_check.R')
    clim_check_result <- clim_check(input, output)
    if(!is.null(clim_check_result)) {
      output$Clim_check <- clim_check_result
      return()
    }
    
    sensors <- input$clim_data$datapath %>%
      scan(nlines=1, what=character(), sep=",")
    sensors <- sensors[-1]
    nsensors <- length(sensors)
    
    source("./tables/clim_meta_table.R")
    output$clim_meta <- clim_meta_table(sensors, siteID=input$siteID)
  })

  ##### Contributors #####
  source('./tables/contributors_table.R')
  output$contributors <- contributors_table()
  reportname <- paste0("/opt/dendroGlobal/Report_", Sys.time(), '.html')
  
  output$Report <- renderUI({
    withProgress({
      incProgress(1/10, detail="validating data...")
      source("./checks/complete_check.R")
      complete_check_result <- complete_check(input, output, session)
      if(!is.null(complete_check_result)) {
        output$Submitted <- complete_check_result
        return()
      }
      
      dm_data <- formData(input)
      clim_data <- formClimData(input)

      site_meta <- data.frame(Site_Name = input$siteID,
                              Latitude = input$lat,
                              Longitude = input$long)
      param <- list(contributors = input$contributors,
                     site_meta = site_meta,
                     tree_meta = input$tree_meta,
                     dm_data = dm_data,
                     dm_meta = input$dm_meta,
                     clim_data = clim_data,
                     clim_meta = input$clim_meta)
      incProgress(4/10, detail='summarizing...')
      rmarkdown::render('./pages/Report.Rmd', output_file = reportname,
                        params = param, intermediates_dir = '/opt/dendroGlobal')
      },
       message="Generating report")

    if(file.exists(reportname)) {
      output$Submit <- renderUI({actionButton("submit", "Submit")})
      includeHTML(reportname)
    }
  })

    
  ##### Submit #####
  observeEvent(input$submit, domain=session, {
    withProgress({
      incProgress(amount=0.1, detail = 'Validating (meta)data')
      source('./checks/complete_check.R')
      complete_check_result <- complete_check(input,output)
      if(!is.null(complete_check_result)) {
        output$Submitted <- complete_check_result
        return()
      }

      site_meta <- data.frame(ID = input$siteID,
                              Country = ifelse(is.null(input$country), 'NA', input$country),
                              Latitude = input$lat,
                              Longitude = input$long,
                              Elevation = input$elevation,
                              Species = ifelse(is.null(input$species_comp), 'NA', input$species_comp),
                              Slope = ifelse(is.null(input$slope), 'NA', input$slope),
                              Aspect = ifelse(is.null(input$aspect), 'NA', input$aspect),
                              Soil_water_holding = ifelse(is.null(input$water_holding), 'NA', input$water_holding),
                              Soil_depth = ifelse(is.null(input$soil_depth), 'NA', input$soil_depth),
                              Management = ifelse(is.null(input$management), 'NA', input$management),
                              Other_data = ifelse(is.null(input$other_data), 'NA', input$other_data),
                              stringsAsFactors = FALSE)
      site_meta$Latitude <- as.numeric(site_meta$Latitude)
      site_meta$Longitude <- as.numeric(site_meta$Longitude)
      site_meta$Elevation <- as.numeric(site_meta$Elevation)

      dm_data <- formData(input)
      dm_meta <- rhandsontable_todf(input$dm_meta)
      dm_meta$Sensor.Height.m. <- as.numeric(dm_meta$Sensor.Height.m.)
      dm_meta$Dead.bark.removal <- as.logical(dm_meta$Dead.bark.removal)
      dm_meta$Sensor.on.xylem <- as.logical(dm_meta$Sensor.on.xylem)

      tree_meta <- rhandsontable_todf(input$tree_meta)
      tree_meta$DBH.cm. <- as.numeric(tree_meta$DBH.cm.)
      tree_meta$Tree.Height.m. <- as.numeric(tree_meta$Tree.Height.m.)
      tree_meta$Crown.Length.m. <- as.numeric(tree_meta$Crown.Length.m.)

      clim_data <- formClimData(input)
      clim_meta <- rhandsontable_todf(input$clim_meta)
      clim_meta$Sensor.Height.m. <- as.numeric(clim_meta$Sensor.Height.m.)

      contributors <- rhandsontable_todf(input$contributors)
      contributors$Manager[is.na(contributors$Manager)] <- 'FALSE'
      contributors$Owner[is.na(contributors$Owner)] <- 'FALSE'
      contributors$Manager <- as.logical(contributors$Manager)
      contributors$Owner <- as.logical(contributors$Owner)

      incProgress(amount = 0.6, detail = 'Saving...')
      out <- list()
      out$site_meta <- site_meta
      out$dm_data <- dm_data
      out$dm_meta <- dm_meta
      out$tree_meta <- tree_meta
      out$clim_data <- clim_data
      out$clim_meta <- clim_meta
      out$contributors <- contributors

      contributor_names <- make.names(paste(contributors$Last.Name, collapse="."))
      site_name <- paste0(contributor_names, '_', make.names(input$siteID), '_', round(as.numeric(Sys.time()),0), '.RDS')
      file_path <- paste0('/opt/dendroGlobal/', site_name)
      saveRDS(out, file_path)

      if(file.exists(file_path)) {
        showModal(modalDialog(
          title = "Submission successful",
          "Thank you for your submission! Please click 'OK' to proceed.",
          footer = tagList(
            actionButton("ok", "OK")
          )
        ), session=session)
      } else {
        showModal(modalDialog(
          title = "Data not saved",
          "Something went wrong", br(),
          "Please try again later..."
        ))
      }
    }, message = "Submitting - please wait...")
  })
  
  observeEvent(input$ok, domain = session, {
    session$reload()
    # Check that data object exists and is data frame.
    removeModal(session)
  })
})
