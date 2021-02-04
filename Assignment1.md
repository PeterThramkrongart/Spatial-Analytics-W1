W1 - Make me an interactive map
================
Peter Thramkrongart

#### Describe a problem or question in your field for which spatial analysis could be applicable.

Creating emergency evacuation plans for large scale terror attacks or
hot-spot city catastrophes such as large explosions.

#### List 5 data layers that you think are necessary to answer your question/solve your problem. Find on the internet github.and then describe examples of two or three of your listed layers.

###### five layers could be:

1:A vector map of roads.

2:A Vector map of buildings.

3:An overlay heat map of population density.

4:Points of suspected terror objectives or vulnerable catastrophe spots.

5: A Map of building heights.

A wealth of city maps of roads, buildings, elevations and so on is
freely available through Leaflet.

``` r
pacman::p_load(pacman,tidyverse, leaflet,htmlwidgets)#load packages
places <- read_csv("RCFeature.csv")#read data
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   uuid = col_double(),
    ##   FeatureID = col_double(),
    ##   FeatureType = col_character(),
    ##   FeatureTimestamp = col_datetime(format = ""),
    ##   Latitude = col_double(),
    ##   Longitude = col_double(),
    ##   Northing = col_double(),
    ##   Easting = col_double(),
    ##   Accuracy = col_double(),
    ##   Description = col_character(),
    ##   Material = col_character(),
    ##   geospatialcolumn = col_character()
    ## )

``` r
places %>% glimpse()#look at data
```

    ## Rows: 257
    ## Columns: 12
    ## $ uuid             <dbl> 1.00011e+18, 1.00004e+18, 1.00005e+18, 1.00004e+18...
    ## $ FeatureID        <dbl> 2000, 4000, 6000, 4001, 5000, 4002, 6001, 6002, 50...
    ## $ FeatureType      <chr> "Other", "Artefact scatter", "Masonry", "Isolated ...
    ## $ FeatureTimestamp <dttm> 2018-04-21 21:44:43, 2018-04-22 00:43:47, 2018-04...
    ## $ Latitude         <dbl> -33.71817, -33.75828, -33.75846, -33.75825, -33.75...
    ## $ Longitude        <dbl> 150.3107, 150.2924, 150.2915, 150.2922, 150.2922, ...
    ## $ Northing         <dbl> 6265844, 6261351, 6261328, 6261354, 6261351, 62613...
    ## $ Easting          <dbl> 250796.0, 249212.8, 249135.6, 249201.8, 249201.4, ...
    ## $ Accuracy         <dbl> 3.0, 0.9, 0.8, 0.8, 0.9, 0.9, 0.9, 3.0, 0.8, 0.7, ...
    ## $ Description      <chr> "YHA startpoint", "two pieces of blue green bottle...
    ## $ Material         <chr> NA, "Glass", "Ceramic - Glass - {Brick} - Other", ...
    ## $ geospatialcolumn <chr> "POINT(250796.002961 6265844.465518)", "POINT(2492...

``` r
#create a funtion for making labels
labs <- lapply(seq(nrow(places)), function(i) {
  paste0( "<b>","FeatureID: ", places[i, "FeatureID"],'</b><br/>', 
          "FeatureType: ", places[i, "FeatureType"], '<br/>', 
          "Description: ",places[i, "Description"],"<br/>")
})


  #create function for creating a color pallette for coloring markers by the FeatureType

colors <- colorFactor(palette = 'RdYlGn', levels(as.factor(places$FeatureType)))

map <- leaflet() %>% # make leaflet map
  addTiles() %>%
  addMiniMap() %>%
   addMeasure() %>% #add measuring tool
  #add circle markers
  addCircleMarkers(
    data = places,
    lat = ~ places$Latitude,
    lng = ~ places$Longitude,
    color = ~ colors(as.factor(places$FeatureType)),
    popup = places$Description,
    label = lapply(labs, htmltools::HTML),#adding labels as HTML
    clusterOptions = markerClusterOptions(),#cluster close markers
    opacity = 0.95
  ) %>% 
  #add legend for colors
   addLegend('bottomleft', pal = colors, values = as.factor(places$FeatureType),
            title = 'Feature Type',
            opacity = 1)
```

    ## Warning in validateCoords(lng, lat, funcName): Data contains 1 rows with either
    ## missing or invalid lat/lon values and will be ignored

``` r
#save map
saveWidget(map, "map.html", selfcontained = TRUE)
```
