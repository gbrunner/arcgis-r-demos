# aRcgis
The making of an R API to interface with ArcGIS Online and ArcGIS Enterprise.

# Why?
1. Why not? We should have some sort of API for every serious data science language. That includes R.
2. DataFrames. In R, everything is a dataframe. It makes data exploration very intuitive.
3. There's already a ton there.

# Getting started
## Install the package
1. Download and install the package from GitHub: ```devtools::install_github("EsriPS/aRcgis",ref="main", auth_token = "yourpersonalauthtoken")```
2. Import package package:  ```library(arcgis)```
3. Import the R ```sp``` package: ```library(sp)```

## Create a connection to ArcGIS Online
1. Create a GIS Onbject from your portal URL, username, and password. This object will keep your credentials and session token.
```
gis <- new("GIS",
           url="https://learngis.maps.arcgis.com/",
           username = "gbrunner_LearnGIS",
           password = "Password")
```
2. Log into ArcGIS Online
```
gis <- login(gis)
print(gis)
```

## Search for content
1. Define a serch term, for example: ```search_term <- "owner:gbrunner_LearnGIS AND NJ AND type:feature service"```
2. Use the ```search_gis``` function to search for items: ```items <- search_gis(gis, search_term)```
3. Print out the results: ```print(head(items$results))```

## Get service properties
1. From the ```items``` dataframe, pass the first result to the ```get_service``` function: ```service <- get_service(gis,items$results[1,])```
2. Print out the results: ```print(service)```

## Feature service as a dataframe
1. Use ```get_sdf``` to turn the feature layer into a spatial dataframes: ```pdf <- get_sdf(gis, items$results[1,], 0)```
2. Print out the results: ```print(head(spdf))```

## Plot the results
1. Plot the polygons using ```plot(spdf)```.
2. Plot the polygons symbolized by the "TOTPOP_CY" attribute using ```spplot(spdf, 'TOTPOP_CY')```
![](https://github.com/EsriPS/aRcgis/blob/main/nj.png)

## Add results to Leaflet map in RStudio
1. Import the **leaflet** library: ```library(leaflet)```.
2. Create a map and add the layer:
```
pal <- colorNumeric("viridis", NULL)
leaflet(spdf) %>%
     addProviderTiles(providers$Esri.WorldStreetMap) %>%
     addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                 fillColor = ~pal(log10(TOTPOP_CY)),
                 label = ~paste0(TOTPOP_CY, ": ", formatC(TOTPOP_CY, big.mark = ",")))
```
![](https://github.com/EsriPS/aRcgis/blob/main/nj_leaflet.png)

