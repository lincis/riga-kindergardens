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
    , tags$script(src = "https://www.googletagmanager.com/gtag/js?id=UA-150842285-1")
    , includeScript("google-analytics.js")
  )
  # , HTML('<meta name="viewport" content="width=1024">')
  , fluidRow(
    column(
      6, titlePanel("Pieteikumi Rīgas pašvaldības bērnudārzos"), withSpinner(leafletOutput("kgmap", height = "400px"))
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
  , fluidRow(
    column(
      12
      , actionLink(inputId = "info", label = "Par aplikāciju", icon = icon("info"), style = "float: right")
      , actionLink(inputId = "application.info", label = "Kāpēc tik maz pieteikumu", icon = icon("question-circle"), style = "float: right; padding-right: 10pt;")
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
        style = "text-align: justify; font-size: 12pt;"
        , tags$p("© Linards Kalvāns, 2019.")
        , tags$p(
          "Datu avots - Latvijas atvērto datu portāls: "
          , tags$a(target="_blank", href="https://data.gov.lv/dati/lv/dataset/pieteikumi-uznemsanai-rigas-pasvaldibas-pirmsskolas-izglitibas-iestades", "Pieteikumi")
          , " un ", tags$a(target="_blank", href="https://data.gov.lv/dati/lv/dataset/uzaicinato-un-uznemto-bernu-skaits-rigas-pasvaldibas-pirmsskolas-izglitibas-iestades", "uzņemtie bērni")
          , " Rīgas pašvaldības pirmsskolas izglītības iestādēs."
        )
        , tags$p("Dati atjaunoti: ##last_update_date##.")
        , tags$p("Pirmkods: ", tags$a(target="_blank", href="https://github.com/lincis/riga-kindergardens", "https://github.com/lincis/riga-kindergardens"))
      )
      , type = "info"
      , html = TRUE
    )
  })
  
  observeEvent(input$application.info, {
    sendSweetAlert(
      session = session
      , title = "Kāpēc pieteikumu izskatās par maz?"
      , text = tags$div(
        style = "text-align: justify; font-size: 12pt;"
        , tags$p("Pirkārt, dati ir pieejami, kopš 2011. gada pieteikumi uzņemšanai līdz pat 2017. varēja tikt un tika iesniegti pirms tam.")
        , tags$p("Otrkārt, mana hipotēze ir, ka datu kopā ir pieejami tikai elektroniski reģistrētie pieteikumi, diemžēl mēģinājums sazināties ar datu uzturētājiem caur atvērto datu portālu bija nesekmīgs")
      )
      , type = "warning"
      , html = TRUE
    )
  })
}

shinyApp(ui, server)