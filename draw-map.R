library(leaflet)
library(ggplot2)

getColor <- function(kgs) {
  sapply(kgs$language, function(language) {
    if(language == "lv") {
      "darkread"
    } else if(language == "ru") {
      "blue"
    } else {
      "purple"
    }
  }) %>% unname()
}

icons <- awesomeIcons(
  icon = 'education',
  # iconColor = 'black',
  # library = 'ion',
  markerColor = getColor(public.kgs)
)

# theme_set(theme_minimal())
leaflet(public.kgs) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addAwesomeMarkers(
    ~as.numeric(longitude)
    , ~as.numeric(latitude)
    , icon = icons
    , label = ~as.character(institution_name)
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

admissions.by.school.year <- all.admissions %>%
  dplyr::left_join(
    all.applications %>%
      dplyr::group_by(institution_id, school_year, group_language) %>%
      dplyr::summarise(applications = n()) %>%
      dplyr::ungroup()
  ) %>%
  tidyr::replace_na(list(applications = 0)) %>%
  dplyr::rename(admissions = number_of_accepted_children) %>%
  dplyr::select(institution_id, school_year, group_language, admissions, applications) %>%
  tidyr::pivot_longer(c("admissions", "applications"), names_to = "type")

getSchoolYear <- function(date) {
  y <- as.numeric(format(date, "%Y"))
  m <- as.numeric(format(date, "%m"))
  if(m <= 9)
    return(y)
  y + 1
}

admissions.by.school.year %>% dplyr::filter(institution_id == 801) %>% View()

admissions.by.school.year %>%
  dplyr::filter(institution_id == 801) %>%
  dplyr::filter(group_language == "latviešu") %>%
  ggplot(aes(x = school_year, y = value, fill = Skaits)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 1, size = 14, angle = 45)
    , axis.title = element_text(size = 16)
    , axis.text.y = element_text(size = 14)
  ) + xlab("Uzņemšanas gads") + ylab("Uzņemtie bērni")
