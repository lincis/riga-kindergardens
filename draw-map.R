library(leaflet)
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(
    lat = public.kgs$latitude %>% as.numeric()
    , lng = public.kgs$longitude %>% as.numeric()
    , popup = public.kgs$institution_name
  )
m
