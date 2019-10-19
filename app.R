library(shiny)
library(leaflet)
library(ggplot2)

public.kgs <- readRDS(here::here("r-data/public.kgs.rds"))
all.admissions <- readRDS(here::here("r-data/all.admissions.rds"))
all.applications <- readRDS(here::here("r-data/all.applications.rds"))

ui <- fluidPage(
  fluidRow(
    column(
      6, titlePanel("Pieteikumi Rīgas pašvaldības bērnudārzos"), leafletOutput("kgmap", height = "600px")
    )
    , column(
      6, titlePanel(textOutput("selected.kg")), plotOutput("application.timeseries", height = "600px")
    )
  )
)

server <- function(input, output) {

  output$kgmap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addAwesomeMarkers(
        lat = public.kgs$latitude %>% as.numeric()
        , lng = public.kgs$longitude %>% as.numeric()
        , popup = paste0("<b>", public.kgs$institution_name, "</b><br>", public.kgs$address)
        , layerId = public.kgs$institution_id
      )
  })
  
  output$selected.kg <-renderText({
    req(input$kgmap_marker_click$id)
    public.kgs %>%
      dplyr::filter(institution_id == input$kgmap_marker_click$id) %>%
      dplyr::pull(institution_name)
  })

  clicked.applications <- reactive({
    req(input$kgmap_marker_click$id)
    dplyr::filter(all.applications, institution_id == input$kgmap_marker_click$id)
  })
  
  output$application.timeseries <- renderPlot({
    ggplot(
      clicked.applications() %>%
       dplyr::mutate(application_month = as.Date(application_registered_date) %>% format("%Y-%m-01") %>% as.Date()) %>%
       dplyr::group_by(institution_name, institution_id, application_month) %>%
       dplyr::summarise(applications = n()) %>%
       dplyr::ungroup()
     , aes(x = application_month, y = applications, group = institution_name)) + 
      geom_point(aes(color = institution_name), size = 3) +
      geom_smooth(aes(color = institution_name), linetype = "dotted", alpha = .15) +
      # scale_color_manual(values = c("#00AFBB", "#E7B800")) +
      theme_minimal() +
      theme(
        legend.position = "none"
        , axis.text.x = element_text(angle = 45, hjust = 1, size = 14)
        , axis.title = element_text(size = 16)
        , axis.text.y = element_text(size = 14)
      ) +
      xlab("Mēnesis") + ylab("Pieteikumu skaits") +
      scale_x_date(breaks = scales::pretty_breaks(n = 20), date_labels = "%b., %Y")
  })
}

shinyApp(ui, server)