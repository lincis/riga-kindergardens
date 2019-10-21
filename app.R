library(shiny)
library(leaflet)
library(ggplot2)
library(shinyWidgets)

public.kgs <- readRDS(here::here("r-data/public.kgs.rds"))
all.admissions <- readRDS(here::here("r-data/all.admissions.rds"))
all.applications <- readRDS(here::here("r-data/all.applications.rds"))

admissions.by.school.year <- readRDS(here::here("r-data/admissions.by.school.year.rds"))


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
  markerColor = getColor(public.kgs)
)

getLanguage <- function(language) {
  dplyr::case_when(
    language == "lv" ~ "latviešu"
    , language == "ru" ~ "mazākumtautību"
    , TRUE ~ "jaukta"
  )
}

ui <- fluidPage(
  HTML('<meta name="viewport" content="width=1024">')
  , fluidRow(
    column(
      6, titlePanel("Pieteikumi Rīgas pašvaldības bērnudārzos"), leafletOutput("kgmap", height = "400px")
    )
    , column(
      6, titlePanel(textOutput("selected.kg"))
      , tabsetPanel(
        tabPanel(
          "Pieteikumu dinamika"
          , dropdownButton(
            "Datu atlases kritēriji"
            , radioGroupButtons(
              "timeseries.type", "Datuma veids"
              , c("Vēlamais uzsākšanas datums" = "desirable_start_date", "Pieteikuma datums" = "application_registered_date")
              , selected = "desirable_start_date"
            )
            , uiOutput("language.selector")
            , circle = TRUE, status = "danger", icon = icon("gear")
          )
          , plotOutput("application.timeseries", height = "360px")
        )
        , tabPanel(
          "Pieteiktie / uzņemtie"
          , uiOutput("language.selector.admissions")
          , plotOutput("applications.vs.admissions", height = "360px")
        )
      )
    )
  )
)

server <- function(input, output) {

  output$kgmap <- renderLeaflet({
    leaflet(public.kgs) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addAwesomeMarkers(
        lat = ~as.numeric(latitude), lng = ~as.numeric(longitude)
        , popup = paste0("<b>", public.kgs$institution_name, "</b><br />", public.kgs$address, "<br />Valoda: ", getLanguage(public.kgs$language))
        , layerId = ~institution_id
        , icon = icons
        , label = ~institution_name
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
  
  output$language.selector <- renderUI({
    checkboxGroupButtons(
      "applications.language", "Mācību valoda"
      , unique(clicked.applications()$group_language)
      , selected = unique(clicked.applications()$group_language)
    )
  })
  
  output$language.selector.admissions <- renderUI({
    checkboxGroupButtons(
      "applications.language.admissions", "Mācību valoda"
      , unique(clicked.applications()$group_language)
      , selected = unique(clicked.applications()$group_language)
    )
  })
  
  output$application.timeseries <- renderPlot({
    language <- input$applications.language
    if(!isTruthy(language))
      language <- unique(clicked.applications()$group_language)
    timeseries.type <- ifelse(isTruthy(input$timeseries.type), input$timeseries.type, "desirable_start_date")
    ggplot(
      clicked.applications() %>%
        dplyr::mutate(
         application_month = clicked.applications()[, timeseries.type] %>% format("%Y-%m-01") %>% as.Date()
        ) %>%
        dplyr::filter(group_language %in% language) %>%
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
  
  admissions.data <- reactive({
    req(input$kgmap_marker_click$id)
    req(input$applications.language.admissions)
    institution.data <- admissions.by.school.year %>%
      dplyr::filter(
        institution_id == input$kgmap_marker_click$id
      )
    language <- input$applications.language.admissions
    if(!isTruthy(language))
      language <- unique(institution.data$group_language)
    institution.data %>%
      dplyr::filter(group_language %in% language) %>%
      dplyr::group_by(school_year, Skaits) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup()
  })
  
  output$applications.vs.admissions <- renderPlot({
    admissions.data() %>%
      ggplot(aes(x = school_year, y = value, fill = Skaits)) +
      geom_bar(stat="identity", color="black", position=position_dodge())+
      theme_minimal() +
      theme(
        axis.text.x = element_text(hjust = 1, size = 14, angle = 45)
        , axis.title = element_text(size = 16)
        , axis.text.y = element_text(size = 14)
        , legend.text = element_text(size = 14)
        , legend.title = element_text(size = 16)
      ) + xlab("Uzņemšanas gads") + ylab("Uzņemtie bērni") +
      scale_x_continuous(breaks = unique(admissions.data()$school_year))
  })
}

shinyApp(ui, server)