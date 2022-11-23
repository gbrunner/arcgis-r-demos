

#' get_imagery_sdf
#' 
#' This function is a method to get a spatial data frame from an image service
#' 
#' 
#' @param service The service that you want to query into a dataframe
#' @keywords ArcGIS, dataframe
#' @examples
#' 
#' get_imagery_sdf(service)
#' 
NULL





#' get_sdf
#' 
#' This function is a method to get a spatial data frame from an item
#' 
#' 
#' @param object The connection to your ArcGIS Online account
#' @param service The service that you want to query into a dataframe
#' @param layer_idx The index of the layer that you are interested in. Defaults
#' to 0.
#' @keywords ArcGIS, dataframe
#' @examples
#' 
#' get_sdf(gis, item)
#' 
NULL





#' get_service
#' 
#' This function is a method to get the service properties
#' 
#' 
#' @param object The connection to your ArcGIS Online account
#' @param item The ArcGIS Online item that you are interested in
#' @keywords ArcGIS, feature service service,
#' @examples
#' 
#' get_service(gis, item)
#' 
NULL





#' GIS class
#' 
#' This clas contains your ArcGIS Online credentials and session token
#' 
#' 
#' @name GIS-class
#' @aliases GIS-class GIS
#' @docType class
#' @param url The url to your ArcGIS Online organization
#' @param username Your ArcGIS Online username
#' @param password Your ArcGIS Online password
#' @keywords ArcGIS GIS,
#' @examples
#' 
#' list(url, username, password)
#' 
NULL





#' GIS class
#' 
#' This function logs you into ArcGIS ONline
#' 
#' 
#' @param gis The connection to your ArcGIS Online account
#' @keywords ArcGIS, GIS, login
#' @examples
#' 
#' login(gis)
#' 
NULL





#' search_gis
#' 
#' This function is a nethod to search ArcGIS Online
#' 
#' 
#' @param object The connection to your ArcGIS Online account
#' @param q The query you are submitting to ArcGIS Online
#' @keywords ArcGIS, query, search
#' @examples
#' 
#' search_gis(gis, query)
#' 
NULL



