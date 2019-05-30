# Code written by Daniel Cullen
# Using code from the following sources:
# https://padpadpadpad.github.io/post/animate-your-strava-activities-using-rstrava-and-gganimate/
# https://medium.com/@annthurium/getting-started-with-the-strava-api-a-tutorial-f3909496cd2d
# https://www.r-bloggers.com/where-do-you-run-to-map-your-strava-activities-on-static-and-leaflet-maps/
# http://www.databrew.cc/posts/strava.html
# https://github.com/fawda123/rStrava
library(rStrava) # devtools::install_github('fawda123/rStrava')
library(dplyr)
library(tidyr)
library(purrr)
library(sp)
library(raster)
library(leaflet)
library(htmlwidgets)

setwd("~/Google Drive/ECON/dancullen/dcullen.github.io")

## Necessary info from Strava api https://www.strava.com/settings/api
app_name       <- 'dancullen' # chosen by user
app_client_id  <- '35805'     # an integer, assigned by Strava
app_secret     <- '1344b30906f6be7fa3498f1ff903c98a854bdff1' # an alphanumeric secret, assigned by Strava
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret))

my_data  <- get_activity_list(stoken)
act_data <- compile_activities(my_data) 

## keep only activity id and map line
keeps <- c('map.summary_polyline', 'upload_id')
my_acts <- dplyr::select(act_data, match(keeps, names(act_data)))

## convert map polyline to collection of lat lon coordinates
lat_lon <- my_acts %>%
  filter(!is.na(map.summary_polyline)) %>%
  nest(., -upload_id) %>%
  mutate(coords = map(data, get_latlon),
         distance = map(coords, ~get_dists(.x$lon, .x$lat))) %>%
  unnest(., data) %>%
  unnest(., coords, distance)

## Create blank map bounded by given lon and lat
lons.range <- c(-119.9, -119.6)
lats.range <- c(34.3, 34.58)
## tile options CartoDB.Positron , CartoDB.DarkMatter , Stamen.Toner  
map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles('CartoDB.Positron',
                   options = providerTileOptions(noWrap = T, minZoom=12, maxZoom=12)) %>%
  fitBounds(lng1 = min(lons.range), lat1 = max(lats.range), lng2 <- max(lons.range), lat2 = min(lats.range))

## Loop through each activity to add activity to map
loop <- unique(lat_lon$upload_id)
for (i in loop) {
  lat_lon_single <- filter(lat_lon, upload_id == i)
  
  ## reorder columns so lat lon are first
  lat_lon_single <- dplyr::select(lat_lon_single, lat, lon, everything())
  
  interp <- raster::spLines(as.matrix(lat_lon_single[,1:2])) %>%
    sp::spsample(., n = 250, type = 'regular') %>%
    data.frame() %>%
    mutate(., distance = get_dists(lon, lat),
           n = row_number())
  
  map <- addPolylines(map, lng = interp$lon, lat = interp$lat,
                      color = 'blue', opacity = 1/4, weight = 2)
}
map

saveWidget(map, file="runningmap.html")
