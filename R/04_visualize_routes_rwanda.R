library(ggmap)
library(leaflet)
library(magrittr)
library(maps)
library(maptools)
library(mapdata)
library(raster)
library(rgeos)
library(sp)
library(sf)
library(tidyverse)

data(world2HiresMapEnv)
country   <- 'rwanda'
zoomLevel <- 6

rwa.map <- 
  world.cities %>% 
  dplyr::filter(country.etc == "Rwanda") %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  sf::as_Spatial()

# Visualize
l.ita.map <- leaflet( ita.spdf ) %>% 
  setView(lng = ita.center$lon, lat = ita.center$lat, zoom = zoomLevel ) %>%
  addTiles() %>%
  addPolygons( data = ita.spdf, weight = 1, fillColor = "blue", fillOpacity = 0.5 )




# Extract the names from ita.map.
# e.g. "Trapani:I. Le Egadi:I. Marettimo" -> "Trapani"
# note: any other solution is fine, because we don't really need them, but they
# can be useful later
rwa.map.ids <-
  sapply(strsplit(rwa.map$name, ':' ), function(x) x[1] )


# Convert our map object to SpatialPolygons
rwa.sp <-
  map2SpatialPolygons(rwa.map, IDs=rwa.map.ids,
                       proj4string=CRS("+proj=longlat +datum=WGS84"))

# Note: if you only need a unified polygon, it can be achieved by fortify
# ita.sp.df <- fortify( ita.sp )

# Finally convert our SpatialPolygons to SpatialPolygonsDataFrame
tmp.id.df <- data.frame( ID = names(ita.sp) )
rownames( tmp.id.df ) <- names( ita.sp )
ita.spdf <- SpatialPolygonsDataFrame( ita.sp, tmp.id.df )

# Visualize
l.ita.map <- leaflet( ita.spdf ) %>% 
  setView(lng = ita.center$lon, lat = ita.center$lat, zoom = zoomLevel ) %>%
  addTiles() %>%
  addPolygons( data = ita.spdf, weight = 1, fillColor = "blue", fillOpacity = 0.5 )

l.ita.map