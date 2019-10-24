library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinycssloaders)
source(here::here("functions.R"))

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
  tags$head(
    tags$style(HTML("
      .shiny-input-container, .radioGroupButtons, .shiny-bound-output {
        display: inline-block !important;
      }

    "))
  )
  # , HTML('<meta name="viewport" content="width=1024">')
  , fluidRow(
    column(
      6, titlePanel("Pieteikumi Rīgas pašvaldības bērnudārzos"), withSpinner(leafletOutput("kgmap", height = "400px"))
      , actionButton(inputId = "info", label = "Par šo lapu", icon = icon("info"))
    )
    , column(
      6, titlePanel(textOutput("selected.kg"))
      , tabsetPanel(
        tabPanel(
          "Dinamika"
          , radioGroupButtons(
            "timeseries.type"
            , choices = c("Uzsākšanas datums" = "desirable_start_date", "Pieteikuma datums" = "application_registered_date")
            , selected = "desirable_start_date"
          )
          , uiOutput("language.selector")
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

server <- function(input, output, session) {

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
    if (length(input$applications.language.admissions) > 0)
      selected.values <- input$applications.language.admissions
    else
      selected.values <- unique(clicked.applications()$group_language)
    checkboxGroupButtons(
      "applications.language"
      , choices = unique(clicked.applications()$group_language)
      , selected = selected.values
    )
  })
  
  output$language.selector.admissions <- renderUI({
    if (length(input$applications.language) > 0)
      selected.values <- input$applications.language
    else
      selected.values <- unique(clicked.applications()$group_language)
    checkboxGroupButtons(
      "applications.language.admissions"
      , choices = unique(clicked.applications()$group_language)
      , selected = selected.values
    )
  })
  
  output$application.timeseries <- renderPlot({
    req(input$applications.language)
    req(input$timeseries.type)
    plotTimeSeries(clicked.applications(), input$timeseries.type, input$applications.language)
  })
  
  admissions.data <- reactive({
    req(input$kgmap_marker_click$id)
    req(input$applications.language.admissions)
    institution.data <- admissions.by.school.year %>%
      dplyr::filter(
        institution_id == input$kgmap_marker_click$id
      )
    institution.data %>%
      dplyr::filter(group_language %in% input$applications.language.admissions) %>%
      dplyr::group_by(school_year, Skaits) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup()
  })
  
  output$applications.vs.admissions <- renderPlot({
    plotAdmissions(admissions.data())
  })
  
  observeEvent(input$info, {
    sendSweetAlert(
      session = session
      , title = "Rīgas bērnudārzu atvērtie dati"
      , text = tags$div(
        tags$div("© Linards Kalvāns, 2019.")
        , tags$div(
          "Datu avots - Latvijas atvērto datu portāls"
          , tags$br()
          , tags$a(target="_blank", href="https://data.gov.lv/dati/lv/dataset/pieteikumi-uznemsanai-rigas-pasvaldibas-pirmsskolas-izglitibas-iestades", "Pieteikumi")
          , " un ", tags$a(target="_blank", href="https://data.gov.lv/dati/lv/dataset/uzaicinato-un-uznemto-bernu-skaits-rigas-pasvaldibas-pirmsskolas-izglitibas-iestades", "uzņemtie bērni")
          , " Rīgas pašvaldības pirmsskolas izglītības iestādēs."
        )
        , tags$div("Dati atjaunoti: ##last_update_date##.")
        , tags$div(tags$a(target="_blank", href="https://github.com/lincis/riga-kindergardens", "Pirmkods"))
      )
      , type = "info"
      , html = TRUE
    )
  })
}

shinyApp(ui, server)