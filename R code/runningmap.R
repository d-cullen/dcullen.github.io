# Code written by Daniel Cullen
# Using code from the following sources:
# https://padpadpadpad.github.io/post/animate-your-strava-activities-using-rstrava-and-gganimate/
# https://medium.com/@annthurium/getting-started-with-the-strava-api-a-tutorial-f3909496cd2d
# https://www.r-bloggers.com/where-do-you-run-to-map-your-strava-activities-on-static-and-leaflet-maps/
# http://www.databrew.cc/posts/strava.html
# https://github.com/fawda123/rStrava
library(rStrava) # devtools::install_github('fawda123/rStrava')
library(gganimate) # devtools::install_github('dgrtwo/gganimate')
library(dplyr)
library(tidyr)
library(purrr)
library(sp)
library(raster)
library(leaflet)
library(htmlwidgets)

setwd('..')

## Necessary info from Strava api https://www.strava.com/settings/api
app_name       <- 'dancullen' # chosen by user
app_client_id  <- '35805' # an integer, assigned by Strava
app_secret     <- '1344b30906f6be7fa3498f1ff903c98a854bdff1' # an alphanumeric secret, assigned by Strava
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret))


## Create blank map bounded by given lon and lat
lons.range <- c(-119.9, -119.6)
lats.range <- c(34.3, 34.58)

my_data  <- get_activity_list(stoken)
act_data <- compile_activities(my_data) 

desired_columns <- c('distance', 'elapsed_time', 'moving_time', 'start_date', 'start_date_local', 'type', 'map.summary_polyline', 'location_city', 'upload_id', 'start_latitude', 'start_longitude')

# keep only desired columns
my_acts <- dplyr::select(act_data, match(desired_columns, names(act_data)))

# transformations ####
my_acts <- mutate(my_acts,
                  activity_no = seq(1,n(), 1),
                  elapsed_time = elapsed_time/60/60,
                  moving_time = moving_time/60/60, 
                  date = gsub("T.*$", '', start_date) %>%
                    as.POSIXct(., format = '%Y-%m-%d'),
                  EUdate = format(date, '%d/%m/%Y'),
                  month = format(date, "%m"),
                  day = format(date, "%d"),
                  year = format(date, "%Y")) %>%
  mutate_at(., c('month', 'day'), as.numeric)

lat_lon <- my_acts %>%
  filter(!is.na(map.summary_polyline)) %>%
  nest(., -activity_no) %>%
  mutate(coords = map(data, get_latlon),
         distance = map(coords, ~get_dists(.x$lon, .x$lat))) %>%
  unnest(., data) %>%
  unnest(., coords, distance)


## tile options CartoDB.Positron , CartoDB.DarkMatter , Stamen.Toner
map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles('CartoDB.Positron',
                   options = providerTileOptions(noWrap = T)) %>%
  fitBounds(lng1 = min(lons.range), lat1 = max(lats.range), lng2 <- max(lons.range), lat2 = min(lats.range))


loop <- unique(lat_lon$activity_no)

for (i in loop) {
  lat_lon_single <- filter(lat_lon, activity_no == i)
  
  # reorder columns so lat lon are first
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
