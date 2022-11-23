if(!require(leaflet)){
  install.packages("leaflet")
  library(leaflet)
}
if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}
if(!require(RColorBrewer)){
  install.packages("RColorBrewer")
  library(RColorBrewer)
}
if(!require(devtools)){
  install.packages("devtools")
  library(devtools)
}
if(!require(arcgis)){
  #devtools::install_github("EsriPS/aRcgis",
  #                         ref="main",
  #                         auth_token = "10569ef60ad1ea5bfc37bb1a6ff4d5a9169b3d14")
  library(arcgis)
}
if(!require(httr)){
  install.packages("httr")
  library(httr)
}
if(!require(geojsonio)){
  install.packages("geojsonio")
  library(geojsonio)
}
if(!require(jsonlite)){
  install.packages("jsonlite")
  library(jsonlite)
}


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                textInput("searchTerm", "Search for Feature Layers", value = "NJ_Demographics",
                          width = NULL, placeholder = NULL
                ),
                actionButton("searchButton", "Search"),
                checkboxInput("owner", "Your Content?", TRUE)
  )
)

server <- function(input, output, session) {

  v <- reactiveValues(data = NULL)

  gis <- new("GIS",
             url="https://learngis.maps.arcgis.com/",
             username = "gbrunner_LearnGIS",
             password = "Pirates##2020")

  #gis <- new("GIS",
  #           url="https://slustl.maps.arcgis.com/",
  #           username = "gregbrunner_slugis",
  #           password = "100%BlueGatorade")

  gis <- login(gis)

  observeEvent(input$searchButton, {
    v$col <- 1
    ind <- 1
    if (input$owner==TRUE) {
      search_term <- paste("owner:gbrunner_LearnGIS AND ", input$searchTerm," AND type:feature service")
    }
    else{
      search_term <- paste(input$searchTerm," AND type:feature service")
    }
    print(input$searchTerm)
    items <- search_gis(gis, search_term)
    print(items$results)
    #service <- get_service(gis,items$results[1,])
    #library(print(service)
    v$spdf <- get_sdf(gis, items$results[[1]], 0)
    ind <- get_column_to_render(v$spdf) #v$spdf$TOTPOP_CY #
    v$col <- v$spdf@data[,ind]
    v$colname <- colnames(v$spdf@data)[ind]
    print(v$colname)
    #v$colname <- as.name(colnames(v$col)[1])
    #print(v$col)
    #print(pal(log10(v$col)))
    #print(v$colname)
  }, ignoreNULL = FALSE)

  pal <- colorNumeric("viridis", NULL)

  # https://rstudio.github.io/leaflet/markers.html

  output$map <- renderLeaflet({
    if (is.null(v$spdf)){
      leaflet() %>%
        addProviderTiles(providers$Esri.WorldStreetMap)
    }
    else{
      if (class(v$spdf) == "SpatialPolygonsDataFrame") {
        m <- add_polygons_layer(v$spdf, v$colname)
      }
      else if (class(v$spdf) == "SpatialPointsDataFrame") {
        m <- add_points_layer(v$spdf, v$colname)
      }
      else if (class(v$spdf) == "SpatialLinesDataFrame") {
        m <- add_polylines_layer(v$spdf, v$colname)
      }
      else if (class(v$spdf) == "SpatialMultiPointsDataFrame") {
        m <- add_multipoints_layer(v$spdf, v$colname)
      }
      else {
        leaflet() %>%
          addProviderTiles(providers$Esri.WorldStreetMap) %>%
          addMarkers()
      }
    }
  })


}

shinyApp(ui, server)
