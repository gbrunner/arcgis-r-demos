#' GIS class
#'
#' This clas contains your ArcGIS Online credentials and session token
#' @param url The url to your ArcGIS Online organization
#' @param username Your ArcGIS Online username
#' @param password Your ArcGIS Online password
#' @keywords GIS, ArcGIS
#' @export
#' @examples
#' list(url, username, password)
GIS <- setClass(
  # Set the name for the class
  "GIS",

  # Define the ArcGIS Online structure (slots)
  slots = c(url="character",
            username = "character",
            password = "character",
            token = "character"
  ),

  # Set the default values for the ArcGIS Online structure(slots).
  prototype=list(
    url="https://www.arcgis.com/",
    username = "",
    password = "",
    token = ""
  )
)



#' GIS class
#'
#' This function logs you into ArcGIS ONline
#' @param gis The connection to your ArcGIS Online account
#' @keywords GIS, ArcGIS, login
#' @export
#' @examples
#' login(gis)
setGeneric(name="login",
           def=function(object)
           {
             standardGeneric("login")
           }
)

# A method to log into ArcGIS Online
setMethod(f="login",
          signature="GIS",
          definition=function(object){
            resp <- POST(
              url = paste(object@url,"/sharing/rest/generateToken?", sep=""),
              body = list(username = object@username,
                           password = object@password,
                           referer = "https://www.arcgis.com",
                           f = "json")
            )
            #print(resp)
            cont<-(content(resp))
            object@token <- cont$token
            print(object@token)
            return(object)
          }
)


#' search_gis
#'
#' This function is a nethod to search ArcGIS Online
#' @param object The connection to your ArcGIS Online account
#' @param q The query you are submitting to ArcGIS Online
#' @keywords ArcGIS, query, search
#' @export
#' @examples
#' search_gis(gis, query)
setGeneric(name="search_gis",
           def=function(object, q)
           {
             standardGeneric("search_gis")
           }
)

# A method to search through ArcGIS Online Content
setMethod(f="search_gis",
          signature="GIS",
          definition=function(object, q){
            resp <- POST(
              url = paste(object@url,"/sharing/rest/search?", sep=""),
              body = list(num=10,
                           start=0,
                           sortField="title",
                           sortOrder="asc",
                           type = "feature service",
                           q = q,
                           f = "json",
                           token=object@token)

            )
            #obj_list <-fromJSON(content(resp))
            obj_list <- content(resp)
            return(obj_list)
          }
)

#' get_service
#'
#' This function is  a method to get the service properties
#'
#' @param object The connection to your ArcGIS Online account
#' @param item The ArcGIS Online item that you are interested in
#' @keywords ArcGIS, service, feature service
#' @export
#' @examples
#' get_service(gis, item)
setGeneric(name="get_service",
           def=function(object, item)
           {
             standardGeneric("get_service")
           }
)

# Method to get get service properties
setMethod(f="get_service",
          signature="GIS",
          definition=function(object, item){
            #print(paste(item$url,"?", sep=""))
            r <- GET(
              url = paste(item$url,"?", sep=""),
              handle = list(f = "json",
                           token=object@token)
            )
            service <- content(r)
            return(service)
          }
)


#' get_sdf
#'
#' This function is a method to get a spatial data frame from an item
#'
#' @param object The connection to your ArcGIS Online account
#' @param service The service that you want to query into a dataframe
#' @param layer_idx The index of the layer that you are interested in. Defaults
#' to 0.
#' @keywords  ArcGIS, dataframe
#' @export
#' @examples
#' get_sdf(gis, item)
# Method to get a spatial dataframe from an item
setGeneric(name="get_sdf",
           def=function(object, service, layer_idx=0)
           {
             standardGeneric("get_sdf")
           }
)

# Method to get a spatial dataframe from an item
setMethod(f="get_sdf",
          signature="GIS",
          definition=function(object, service, layer_idx=0){
            #print(paste(service$url,"/", toString(layer_idx), "/query?", sep=""))
            ur<-paste(service$url,"/", toString(layer_idx), "/query?where=1=1&", sep="")
            payload<-paste("outFields=*&f=geojson&token=",object@token,sep="")
            request<-paste(ur, payload, sep="")
            print(request)
            r <- GET(
              url = request
            )
            dl<-content(r)
            ListJSON=toJSON(dl,pretty=TRUE,auto_unbox=TRUE)
            mydata<-geojson_sf(ListJSON)
            new_spdf<-sf::as_Spatial(mydata)
            return(new_spdf)
          }
)


#' get_imagery_sdf
#'
#' This function is a method to get a spatial data frame from an image service
#'
#' @param service The service that you want to query into a dataframe
#' @keywords ArcGIS, dataframe
#' @export
#' @examples
#' get_imagery_sdf(service)
# Method to get a spatial dataframe from an item
setGeneric(name="get_imagery_sdf",
           def=function(gis, service)
           {
             standardGeneric("get_imagery_sdf")
           }
)

setMethod(f="get_imagery_sdf",
          signature="GIS",
          definition=function(gis, service) {
            print(paste(service$url, "/query?", sep=""))
            r <- GET(
              url = paste(service$url, "/query?", sep=""),
              query = list(outFields="*",
                           f = "json",
                           token=gis@token,
                           where="1=1")
            )
            #imagery_sdf <- readOGR(content(r))
            return(content(r))
          }
)




#' add_item
#'
#' This function is a method to add an item to ArcGIS Online
#'
#' @param object The GIS Object that representst the connection to ArcgIS
#' Online
#' @param filename The ZIP file to upload into ArcGIS Online
#' @param type Content type, i.e. Shapeile, GDB, etc.
#' @param keywords Keywords with which to tag the content
#' @param text Text with which to describe the content
#' @keywords Add ArcGIS Item,
#' @examples
#'
#' add_item(object, file_name, type, keywords, title, text)
#' Method to add an item to ArcGIS Online
#'
#' @export add_item
add_item <- function(object,
                     filename,
                     type,
                     keywords,
                     title,
                     text) {
  resp <- POST(
    url = paste(object@url
                ,"/sharing/rest/content/users/",
                object@username,
                '/addItem?',
                sep=""),
    query = list(token = object@token,
                 referer = "https://www.arcgis.com",
                 f = "json",
                 file = "true",
                 type = type,
                 typeKeywords = keywords,
                 title = title,
                 text = text),
    body = list(filename = upload_file(system.file(filename)))
  )
  return(resp)

}


#' get_item_types
#'
#' This function is a method get a list of AGOL content types and typeKeywords
#'
#' @param item The item list as a dataframe
#' @keywords ArcGIS item, type,
#' @examples
#'
#' get_item_types(items)
#' Method to get a list of AGOL content types and typeKeywords
#'
#' @export get_item_types
get_item_types <- function(items) {
  type_info <- list("type" = items$results$type,
                    "typeKeywords" = items$results$typeKeywords)
  return(type_info)
}


#' get_item_type
#'
#' This function is a method get content types and typeKeywords from an item on
#' AGOL
#'
#' @param item The item that you are interested in
#' @keywords ArcGIS item, type,
#' @examples
#'
#' get_item_type(item)
#' Method to get content types and typeKeywords from an item i AGOL
#'
#' @export get_item_type
get_item_type <- function(item) {
  type_info <- list("type" = item$type,
                    "typeKeywords" = item$typeKeywords)
  return(type_info)
}

#' is_hosted_feature_service
#'
#' This function is determines whether the item you are interested in
#'
#' is a hosted feature service
#' @param item The item that you are interested in
#' @keywords item, type, ArcGIS, hosted, feature, service
#' @export
#' @examples
#' is_hosted_feature_service(item)
#' Method to determine whether the item you are interested in
#' is a hosted feature service
is_hosted_feature_service <- function(item) {
  item_type <- get_item_type(item)
  print(item_type)
  if (item_type$type == "Feature Service" &
      is.element("Hosted Service", item_type$typeKeywords[[1]])){
    is_hfs <- TRUE
  }
  else{
    is_hfs <- FALSE
  }
  return(is_hfs)
}



#' get_first_feature_service
#'
#' This finds the first feature service in the dataframe of items returned by
#' the search function
#'
#' @param items The dataframe of items returned by the search funtion
#' @keywords ArcGIS, feature, hosted, item, service type,
#' @examples
#'
#' get_first_feature_service(items)
#' This finds the first feature service in the dataframe of items
#' returned by the search function,
#' try this get_first_feature_service(items)$url
#'
#' @export get_first_feature_service
get_first_feature_service <- function(items){
  return(items$results[match("Feature Service", items$results$type),])
}


#' get_column_to_render
#'
#' This function finds the second numberic column in a feature layers
#' attributes. It is meant as a kind of 'poor man's smart mapping'
#'
#' @param attrs The feature attribute dataframe
#' @keywords attributes, mapping smart
#' @export get_column_to_render
get_column_to_render <- function(attrs) {
  col_class_list <-  sapply(attrs@data, class)
  #print(col_class_list)
  count <- 0
  index <- 0
  ind = 1
  for (val in col_class_list) {
    if (val == "integer"){
      count <- count+1
      print(count)
    }
    index <- index+1
    print(index)
    if (count == 2){
      ind = index
      break
    }
  }
  # of use colnames(attrs[ind])
  return(ind)
}


#' show_map
#'
#' This function displays a leaflet map. In the future I hope to extent this
#' function to do more
#'
#' @param basemap An optional basemap parameter can be used to set the basemap
#' @keywords map, leaflet, esri
#' @export
#' @examples
#' show_map(basemap)
#' This function displays a leaflet map. In the future I hope to extent this
#' function to do more
show_map <- function(basemap = providers$Esri.WorldStreetMap){
  m <- leaflet() %>%
    addProviderTiles(basemap)
  return(m)
}



#' add_points_layer
#'
#' This function displays a map using geojson points as input.
#'
#' @param spdf The spatial points dataframe to be displayed
#' @param colname The column in the features to render as the symbology
#' @param basemap An optional basemap parameter can be used to set the basemap
#' @keywords dataframe esri, leaflet, map, points points, spatial
#' @examples
#'
#' add_points_layer(spdf, colname, basemap)
#' This function displays a map using geojson points as input.
#'
#' @export add_points_layer
add_points_layer <- function(spdf,
                             colname,
                             basemap = providers$Esri.WorldStreetMap){

  col <- spdf@data[[colname]]

  m <- leaflet(spdf) %>%
    addProviderTiles(basemap) %>%
    addMarkers(clusterOptions = markerClusterOptions(),
              popup = ~as.character(col),
              label = ~paste0(colname, ": ",as.character(col)))
  return(m)
}


#' add_multipoints_layer
#'
#' This function displays a map using geojson multipoints as input.
#'
#'
#' @param spdf The spatial multipoints dataframe to be displayed
#' @param colname The column in the features to render as the symbology
#' @param basemap An optional basemap parameter can be used to set the basemap
#' @keywords dataframe esri, leaflet, map, multipoints points, spatial
#' @examples
#'
#' add_multipoints_layer(spdf, colname, basemap)
#' This function displays a map using geojson multipoints as input.
#'
#' @export add_multipoints_layer
add_multipoints_layer <- function(spdf,
                             colname,
                             basemap = providers$Esri.WorldStreetMap){

  col <- spdf@data[[colname]]

  m <- leaflet(spdf) %>%
    addProviderTiles(basemap) %>%
    addMarkers(clusterOptions = markerClusterOptions(),
               popup = ~as.character(col),
               label = ~paste0(colname, ": ",as.character(col)))
  return(m)
}



#' add_polylines_layer
#'
#' This function displays a map using geojson polylines as input.
#'
#' @param spdf The spatial polylines dataframe to be displayed
#' @param colname The column in the features to render as the symbology
#' @param basemap An optional basemap parameter can be used to set the basemap
#' @keywords dataframe esri, leaflet, map, points, polylines spatial
#' @examples
#'
#' add_polylines_layer(spdf, colname, basemap)
#' This function displays a map using geojson polylines as input.
#'
#' @export add_polylines_layer
add_polylines_layer <- function(spdf,
                             colname,
                             basemap = providers$Esri.WorldStreetMap){

  col <- spdf@data[[colname]]

  m <-leaflet(spdf) %>%
    addProviderTiles(basemap)  %>%
    addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1)#,
  return(m)
}



#' add_polygons_layer
#'
#' This function displays a map using geojson polygons as input.
#'
#' @param spdf The spatial polygons dataframe to be displayed
#' @param colname The column in the features to render as the symbology
#' @param basemap An optional basemap parameter can be used to set the basemap
#' @keywords dataframe esri, leaflet, map, points, polygons spatial
#' @examples
#'
#' add_polygons_layer(spdf, colname, basemap)
#' This function displays a map using geojson polygons as input.
#'
#' @export add_polygons_layer
add_polygons_layer <- function(spdf,
                               colname,
                               basemap = providers$Esri.WorldStreetMap,
                               colorname = "viridis"){

  pal <- colorNumeric(colorname, NULL)

  col <- spdf@data[[colname]]

  m <-leaflet(spdf) %>%
    addProviderTiles(basemap) %>%
    addPolygons(stroke = FALSE,
                smoothFactor = 0.3,
                fillOpacity = 1,
                fillColor = ~pal(col),
                label = ~paste0(colname, ": ",
                                formatC(col, big.mark = ",")))
  return(m)
}



#' add_legend_to_map
#'
#' This function adds a legend to the map.
#'
#' @param m The map to add the legend to
#' @param spdf The spatial dataframe to be used to drive the legend symbology
#' @param colname The column in the features to render as the symbology
#' @param colorname The colormap to use for symbology
#' @keywords esri, leaflet, legend map,
#' @examples
#'
#' add_legend_to_map(m, spdf, colname, colorname)
#' This function adds a legend to the map.
#'
#' @export add_legend_to_map
add_legend_to_map <- function(m, spdf, colname, colorname = "viridis"){

  pal <- colorNumeric(colorname, NULL)

  m %>% addLegend("bottomright",
            pal = pal,
            values = spdf@data[[colname]],
            title = colname)
}

#' list_esri_basemaps
#'
#' This function lists the available esri basemaps.
#'
#' @keywords map, leaflet, esri, basemaps
#' @export
#' @examples
#' list_esri_basemaps()
#' This function lists the available esri basemaps.
list_esri_basemaps <- function(){

  esri <- grep("^Esri", providers, value = TRUE)

  return(esri)

}

#' list_stamen_basemaps
#'
#' This function lists the available stamen basemaps.
#'
#' @keywords map, leaflet, stamen, basemaps
#' @export
#' @examples
#' list_stamen_basemaps()
#' This function lists the available stamen basemaps.
list_stamen_basemaps <- function(){

  stamen <- grep("^Stamen", providers, value = TRUE)

  return(stamen)
}

