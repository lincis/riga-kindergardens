library(leaflet)
library(ggplot2)



theme_set(theme_minimal())
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(
    lat = public.kgs$latitude %>% as.numeric()
    , lng = public.kgs$longitude %>% as.numeric()
    , popup = public.kgs$institution_name
  )
m

time.series.data <- all.applications %>%
  filter(institution_id < 550) %>%
  mutate(application_month = as.Date(application_registered_date) %>% format("%Y-%m")) %>%
  group_by(institution_name, institution_id, application_month) %>%
  summarise(
    applications = n()
  ) %>%
  ungroup()

ggplot(time.series.data, aes(x = application_month, y = applications, group = institution_name)) + 
  geom_point(aes(color = institution_name)) +
  geom_smooth(aes(color = institution_name)) +
  # scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()
