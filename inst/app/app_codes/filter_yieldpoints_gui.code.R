
# # Sample data for visualization TODO
samplesize <- 1E4 # FIXME make dynamic
# observeEvent(input$sampling_data, {
#   req(rv$pt_yielddata)
# })

# Define UI objects dinamically for filtering
output$indata_rangev <- renderUI({
  req(rv$pt_yielddata, rv$vectfields)
  sliderInput(inputId="rangev",
              label="Velocità della macchina:",
              min = trunc(min(rv$pt_yielddata@datatable$speed)),
              max = ceiling(max(rv$pt_yielddata@datatable$speed)),
              value = quantile(rv$pt_yielddata@datatable$speed,c(0,1)),
              post = " km/h",
              step = 0.1)
})
output$indata_rangey <- renderUI({
  req(rv$pt_yielddata, rv$vectfields)
  sliderInput(inputId="rangey",
              label="Resa, valori consentiti:",
              min = 0, #trunc(min(rv$pt_yielddata@datatable$yield)),
              max = ceiling(max(rv$pt_yielddata@datatable$yield)),
              value = quantile(rv$pt_yielddata@datatable$yield,c(0.02,0.98)),
              post = " t",
              step = 0.1)
})

# Define data with filters
# filtdata <- reactive({
#   filter.yieldpoints(rv$pt_yielddata,"rangev",input$rangev,samplesize=NA)
#   filter.yieldpoints(rv$pt_yielddata,"mins",input$mins/100,samplesize=NA)
#   filter.yieldpoints(rv$pt_yielddata,"rangey",input$rangey,samplesize=NA)
#   filter.yieldpoints(rv$pt_yielddata,"stdy",input$stdy,samplesize=NA)
#   filter.yieldpoints(rv$pt_yielddata,"rangeq",input$rangeq/100,samplesize=NA)
#   filter.yieldpoints(rv$pt_yielddata,"pos",input$pos,inlayer=rv$vectfields,samplesize=NA)
# })
# observe({
#   rv$pt_yielddata <- filter.yieldpoints(rv$pt_yielddata,"rangev",input$rangev,samplesize=NA)
# })
# observe({
#   rv$pt_yielddata <- filter.yieldpoints(rv$pt_yielddata,"mins",input$mins/100,samplesize=NA)
# })
# observe({
#   rv$pt_yielddata <- filter.yieldpoints(rv$pt_yielddata,"rangey",input$rangey,samplesize=NA)
# })
# observe({
#   rv$pt_yielddata <- filter.yieldpoints(rv$pt_yielddata,"stdy",input$stdy,samplesize=NA)
# })
# observe({
#   rv$pt_yielddata <- filter.yieldpoints(rv$pt_yielddata,"rangeq",input$rangeq/100,samplesize=NA)
# })
# observe({
#   rv$pt_yielddata <- filter.yieldpoints(rv$pt_yielddata,"pos",input$pos,inlayer=rv$vectfields,samplesize=NA)
# })

# Filters
observe({
  req(rv$pt_yielddata, rv$vectfields)
  observeEvent(c(input$check_rangev, input$rangev), {
    if (input$check_rangev) {
      rv$pt_yielddata <- filter.yieldpoints(
        rv$pt_yielddata, "rangev", input$rangev,
        inlayer = as(rv$vectfields, "Spatial"),
        id_fieldname="id_geom", # TODO add GUI input for it
        byfield = TRUE, samplesize = NA
      )
    } else {
      filter.yieldpoints.reset(rv$pt_yielddata, "rangev")
    }
  })
  observeEvent(c(input$check_mins, input$mins), {
    if (input$check_mins) {
      rv$pt_yielddata <- filter.yieldpoints(
        rv$pt_yielddata, "mins", input$mins/100,
        inlayer = as(rv$vectfields, "Spatial"),
        id_fieldname="id_geom", # TODO add GUI input for it
        byfield = TRUE, samplesize = NA
      )
    } else {
      filter.yieldpoints.reset(rv$pt_yielddata, "mins")
    }
  })
  observeEvent(c(input$check_rangey, input$rangey), {
    if (input$check_rangey) {
      rv$pt_yielddata <- filter.yieldpoints(
        rv$pt_yielddata, "rangey", input$rangey,
        inlayer = as(rv$vectfields, "Spatial"),
        id_fieldname="id_geom", # TODO add GUI input for it
        byfield = TRUE, samplesize = NA
      )
    } else {
      filter.yieldpoints.reset(rv$pt_yielddata, "rangey")
    }
  })
  observeEvent(c(input$check_stdy, input$stdy), {
    if (input$check_stdy) {
      rv$pt_yielddata <- filter.yieldpoints(
        rv$pt_yielddata, "stdy", input$stdy,
        inlayer = as(rv$vectfields, "Spatial"),
        id_fieldname="id_geom", # TODO add GUI input for it
        byfield = TRUE, samplesize = NA
      )
    } else {
      filter.yieldpoints.reset(rv$pt_yielddata, "stdy")
    }
  })
  observeEvent(c(input$check_rangeq, input$rangeq), ignoreNULL = FALSE, ignoreInit = FALSE, {
    if (input$check_rangeq) {
      rv$pt_yielddata <- filter.yieldpoints(
        rv$pt_yielddata, "rangeq", input$rangeq/100,
        inlayer = as(rv$vectfields, "Spatial"),
        id_fieldname="id_geom", # TODO add GUI input for it
        byfield = TRUE, samplesize = NA
      )
    } else {
      filter.yieldpoints.reset(rv$pt_yielddata, "rangeq")
    }
  })
  observeEvent(c(input$check_pos, input$pos), ignoreNULL = FALSE, ignoreInit = FALSE, {
    if (input$check_pos) {
      rv$pt_yielddata <- filter.yieldpoints(
        rv$pt_yielddata, "pos", input$pos,
        inlayer = as(rv$vectfields, "Spatial"),
        id_fieldname="id_geom", # TODO add GUI input for it
        byfield = TRUE, samplesize = NA
      )
    } else {
      filter.yieldpoints.reset(rv$pt_yielddata, "pos")
    }
  })
}) # end of observe on filters


# Histogram of yield
observeEvent(c(
  input$check_rangev, input$rangev,
  input$check_mins, input$mins,
  input$check_rangey, input$rangey,
  input$check_stdy, input$stdy,
  input$check_rangeq, input$rangeq,
  input$check_pos, input$pos
), {
  req(rv$pt_yielddata, rv$vectfields)
  rv$yield_hist_range <- quantile(rv$pt_yielddata@datatable$yield, c(.005,.995), type=1)
  rv$yield_hist_breaks <- hist(plot = FALSE, rv$pt_yielddata@datatable[sid<=samplesize & yield>=rv$yield_hist_range[1] & yield<=rv$yield_hist_range[2],]$yield, breaks=50)$breaks
  rv$yield_hist_ylim <- c(0,max(hist(plot = FALSE, rv$pt_yielddata@datatable[sid<=samplesize & yield>=rv$yield_hist_range[1] & yield<=rv$yield_hist_range[2],]$yield, breaks=50)$counts))
  output$yield_hist <- renderPlot({
    # req(rv$pt_yielddata, rv$vectfields)
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # hist(rv$pt_yielddata@datatable$yield, breaks = bins, col = 'darkgray', border = 'white')
    # ggplot(rv$pt_yielddata@datatable[yield>quantile(yield,input$rangeq[1]/100)&yield<quantile(yield,input$rangeq[2]/100)], aes(x=yield)) +
    #   geom_histogram(colour="white", breaks=yield_hist_breaks)
    p <- ggplot(rv$pt_yielddata@datatable, aes(x=yield)) +
      geom_histogram(data=rv$pt_yielddata@datatable[sid<=samplesize&filter==FALSE,], colour="white", fill="darkgreen", breaks=rv$yield_hist_breaks) +
      geom_histogram(data=rv$pt_yielddata@datatable[sid<=samplesize&filter==TRUE,], colour="white", fill="red", breaks=rv$yield_hist_breaks) +
      ylim(rv$yield_hist_ylim) +
      xlab("Yield (t)") + ylab("Count")
    if (!is.null(input$rangey)) {
      p <- p + geom_vline(xintercept = input$rangey, colour="red")
    }
    if (!is.null(input$rangeq)) {
      p <- p + geom_vline(xintercept = quantile(rv$pt_yielddata@datatable$yield,input$rangeq/100), colour="red", linetype="dashed")
    }
    p
  })
})

# Secondary histogram
observeEvent(c(
  input$check_rangev, input$rangev,
  input$check_mins, input$mins,
  input$check_rangey, input$rangey,
  input$check_stdy, input$stdy,
  input$check_rangeq, input$rangeq,
  input$check_pos, input$pos
), {
  req(rv$pt_yielddata, rv$vectfields)
  rv$hist_breaks <- hist(plot = FALSE, rv$pt_yielddata@datatable[sid<=samplesize,][[input$hist_selvariable]], breaks=30)$breaks
  rv$hist_ylim <- c(0,max(hist(plot = FALSE, rv$pt_yielddata@datatable[sid<=samplesize,][[input$hist_selvariable]], breaks=30)$counts))
  output$sec_hist <- renderPlot({
    # req(rv$pt_yielddata, rv$vectfields)
    p <- ggplot(rv$pt_yielddata@datatable, aes(x=get(input$hist_selvariable))) +
      geom_histogram(data=rv$pt_yielddata@datatable[sid<=samplesize&filter==FALSE,], colour="white", fill="darkcyan", breaks=rv$hist_breaks) +
      geom_histogram(data=rv$pt_yielddata@datatable[sid<=samplesize&filter==TRUE,], colour="white", fill="red", breaks=rv$hist_breaks) +
      ylim(rv$hist_ylim) +
      xlab(input$hist_selvariable) + ylab("Count")
    if (!is.null(input$rangev)) {
      p <- p + geom_vline(
        xintercept = if (input$hist_selvariable=="speed") {
          input$rangev
        } else if (input$hist_selvariable=="rel_width") {
          input$mins/100
        } else {
          NA
        },
        colour="red"
      )
      p
    }
  })
})


# Create the map
output$map_map <- renderLeaflet({
  req(rv$pt_yielddata, rv$vectfields)
  leaflet() %>%
    addProviderTiles("Esri.WorldImagery",
                     options = providerTileOptions(noWrap=TRUE, maxZoom=22)
    ) %>%
    addPolygons(data=st_transform(rv$vectfields,4326), color="blue", fill=NA, weight=2.5 #, dashArray="8,6"
    ) %>%
    # setView(lng = 11.96, lat = 44.86, zoom = 15)
    fitBounds(lng1 = min(rv$pt_yielddata@datatable$lon),
              lat1 = min(rv$pt_yielddata@datatable$lat),
              lng2 = max(rv$pt_yielddata@datatable$lon),
              lat2 = max(rv$pt_yielddata@datatable$lat))
})

# Update the map
observeEvent(c(
  input$check_rangev, input$rangev,
  input$check_mins, input$mins,
  input$check_rangey, input$rangey,
  input$check_stdy, input$stdy,
  input$check_rangeq, input$rangeq,
  input$check_pos, input$pos
), {
  req(rv$pt_yielddata, rv$vectfields)
  colorData <- rv$pt_yielddata@datatable[sid<samplesize&filter==FALSE,]
  pal <- colorBin("RdYlGn", range(colorData[[input$map_selvariable]],na.rm=TRUE), bins=5, pretty=TRUE)
  reac_map <- leafletProxy("map_map") %>%
    clearShapes() %>%
    addPolygons(data=st_transform(rv$vectfields,4326), color="blue", fill=NA, weight=2.5) %>%
    addCircles(~lon, ~lat, data=rv$pt_yielddata@datatable[sid<samplesize&filter==TRUE,],
               radius=1.5, stroke=FALSE, fillOpacity=0.4, fillColor="gray") %>%
    addCircles(~lon, ~lat, data=colorData,
               radius=1.5, stroke=FALSE, fillOpacity=0.65, fillColor=~pal(colorData[[input$map_selvariable]])) %>%
    # addCircles(~lon, ~lat, data=rv$pt_yielddata@datatable[sid<samplesize&filter==FALSE,],
    #            radius=1.5, stroke=FALSE, fillOpacity=0.4, fillColor=~colorBin("RdYlGn", yield)(yield)) %>%
    addLegend("bottomleft", pal=pal, values=colorData, #title=input$map_selvariable,
              layerId="colorLegend")
  if (input$check_pos) {
    reac_map <- reac_map %>%
      addPolygons(data=st_transform(st_buffer(rv$vectfields, -input$pos),4326), color="darkcyan", fill=NA, weight=2, dashArray="8,6")
  }
  reac_map
})

# Export filtering parameters
output$downloadFilters <- downloadHandler(
  filename = "filters.json",
  content = function(file) {
    filters <- toJSON(list("rangev"=input$rangev,
                           "mins"=input$mins/100,
                           "rangey"=input$rangey,
                           "stdy"=input$stdy,
                           "rangeq"=input$rangeq/100,
                           "pos"=input$pos,
                           "check_rangev"=input$check_rangev,
                           "check_mins"=input$check_mins,
                           "check_rangey"=input$check_rangey,
                           "check_stdy"=input$check_stdy,
                           "check_rangeq"=input$check_rangeq,
                           "check_pos"=input$check_pos))
    write(filters, file)
  }
)

# Import filters
observe({
  if(!is.null(input$importFilters)) {
    imported_filters <- fromJSON(file(input$importFilters$datapath,"r"))
    updateSliderInput(session, "rangev", value=imported_filters[["rangev"]])
    updateSliderInput(session, "mins", value=imported_filters[["mins"]]*100)
    updateSliderInput(session, "rangey", value=imported_filters[["rangey"]])
    updateSliderInput(session, "stdy", value=imported_filters[["stdy"]])
    updateSliderInput(session, "rangeq", value=imported_filters[["rangeq"]]*100)
    updateSliderInput(session, "pos", value=imported_filters[["pos"]])
    updateCheckboxInput(session, "check_rangev", value=imported_filters[["check_rangev"]])
    updateCheckboxInput(session, "check_mins", value=imported_filters[["check_mins"]])
    updateCheckboxInput(session, "check_rangey", value=imported_filters[["check_rangey"]])
    updateCheckboxInput(session, "check_stdy", value=imported_filters[["check_stdy"]])
    updateCheckboxInput(session, "check_rangeq", value=imported_filters[["check_rangeq"]])
    updateCheckboxInput(session, "check_pos", value=imported_filters[["check_pos"]])
  }
})


# Save filtered data
observeEvent(input$save_filtered, {
  updateTabItems(session, "tabs", "tab_input")
  # stopApp(rv$pt_yielddata)
})
