library(shiny)
library(tidyverse)
library(rvc)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)
library(leaflet)

myFiles <- list.files("plots/", pattern = "*.R", full.names = T)
sapply(myFiles, source)
species <- read.csv("Data/speciesList_short.csv")
drto <- readRDS("Data/drto_samll.rds")
keys <- readRDS("Data/keys_small.rds")
sefl <- readRDS("Data/sefl_small.rds")

ui <-
  navbarPage("NCRMP Atlantic Fish", collapsible = TRUE, inverse = TRUE, theme = shinytheme("spacelab"),
             # Home panel ----
             tabPanel("Home",
                      # parent container
                      tags$div(class="landing-wrapper",
                               
                               # child element 1: images
                               tags$div(class="landing-block background-content",
                                        
                                        # top left
                                        img(src="stingray.jpg"),
                                        
                                        # top right
                                        img(src="HYGE.jpg"),
                                        
                                        # bottom left
                                        img(src="Angel2.jpg"), 
                                        
                                        # bottom right
                                        
                                        img(src="Judge.jpg")
                                        
                               ),
                               
                               # child element 2: content
                               tags$div(class="landing-block foreground-content",
                                        tags$div(class="foreground-text",
                                                 tags$h1("Welcome"),
                                                 tags$p("This shiny application 
                                                        analyzes fish data from the
                                                        Gulf", actionLink("Atlantic", "Atlantic")),
                                        )
                               )
                      )
             ),
             # Analysis Panel ---------
             tabPanel("Analysis",
                      fluidPage(tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "app.css")),
                                titlePanel("Fish"),
                                sidebarLayout(
                                  sidebarPanel(
                                    sliderInput("years",
                                                label = "Year Range",
                                                min = 2000,
                                                max = 2021,
                                                value = c(2000, 2021),
                                                sep = ""
                                    ),
                                    selectInput("domain",
                                                "Domain",
                                                c("Dry Tortugas", "Florida Keys", "SE Florida"),
                                                selected = "Dry Tortugas"
                                    ),
                                    selectInput("species",
                                                "Select Species",
                                                choices = setNames(species$SPECIES_CD, species$search_name),
                                                selected = "OCY CHRY"
                                    ),
                                    actionButton("build", "Build Species Plots"),
                                    width = 2
                                  ),
                                  mainPanel(
                                    tabsetPanel(
                                      tabPanel(
                                        "Plot",
                                        fluidRow(
                                          column(
                                            12,
                                            withSpinner(
                                              plotOutput("densityplot"),
                                              type = 1,
                                              color.background = "white",
                                              hide.ui = TRUE
                                            )
                                          )
                                        ),
                                        fluidRow(
                                          column(
                                            6,
                                            withSpinner(
                                              plotOutput("occurrenceplot"),
                                              type = 3,
                                              color.background = "white",
                                              hide.ui = TRUE
                                            )
                                          ),
                                          column(
                                            6,
                                            withSpinner(
                                              plotOutput("biomassplot"),
                                              type = 3,
                                              color.background = "white",
                                              hide.ui = TRUE
                                            )
                                          )
                                        ),
                                        fluidRow(
                                          column(
                                            12,
                                            withSpinner(
                                              plotOutput("lenfreqplot"),
                                              type = 3,
                                              color.background = "white",
                                              hide.ui = TRUE
                                            )
                                          )
                                        )
                                      ),
                                      tabPanel("Map",
                                               fluidPage(
                                                 fluidRow(
                                                   column(12,
                                                          tags$div(class = "mappage",
                                                                   leafletOutput("mymap", width = "100%", height = 900)
                                                          )
                                                   )
                                                 )
                                               )
                                      ),
                                      tabPanel(
                                        "Table",
                                        fluidRow(
                                          column(
                                            2,
                                            selectInput(
                                              "whichMetric",
                                              "Choose Metric",
                                              c("Density" = "densitydata", "Occurrence" = "occurrencedata", "Biomass" = "biomassdata"),
                                              selected = "densitydata"
                                            ),
                                            downloadButton("downloadData", "Download"),
                                          ),
                                          column(
                                            10,
                                            withSpinner(
                                              tableOutput("data_table"),
                                              type = 3,
                                              color.background = "white"
                                            )
                                          )
                                        ),
                                      )
                                    )
                                  )
                                )
                      )),
             # Panel with sub panels Panel --------
             tabPanel("Panel with subpanels",
                      fluidPage(
                        tabsetPanel(
                          tabPanel("subpanel 1"),
                          tabPanel("subpanel 2"),
                          tabPanel("subpanel 3"),
                          tabPanel("subpanel 4")
                        ))),
             # outreach panel ------
             tabPanel("Outreach"),
             # about panel --------
             tabPanel("About")
  )
# Server ----
server <- function(input, output, session) {
  observeEvent(input$Atlantic, {
    updateNavbarPage(session, "Atlantic")
  })
  # dataset choice
  dataset <- eventReactive(input$build, {
    if (input$domain == "Dry Tortugas") {
      return(drto)
    } else if (input$domain == "Florida Keys") {
      return(keys)
    } else if (input$domain == "SE Florida") {
      return(sefl)
    }
  })
  
  years <- eventReactive(input$build, {
    input$years
  })
  
  spp <- eventReactive(input$build, {
    input$species  
  })
  
  domain <- eventReactive(input$build, {
    input$domain  
  })

  dt <- reactiveValues()
      
  output$mymap <- renderLeaflet({
    sites = dataset()$sample_data %>% filter(YEAR == max(dataset()$sample_data$YEAR)) %>% group_by(REGION, YEAR, PRIMARY_SAMPLE_UNIT, STATION_NR) %>% summarise(lat = mean(LAT_DEGREES), lon = mean(LON_DEGREES))
      leaflet(sites) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addMarkers(lng = ~lon, lat = ~lat,
                 popup = ~PRIMARY_SAMPLE_UNIT)
                 # popup = paste0("<img src = HYGE.jpg width=100 height=100>"))
  })

  observeEvent(input$build, {
  output$densityplot <- renderPlot({
    a <- plot_domain_den_by_year(
      dataset = dataset(),
      species = spp(),
      years = seq(years()[1], years()[2]),
      print_dataframe = T,
      title = paste(domain(), spp())
    )
    dt$densitydata <- a
  })
  })
 
  output$occurrenceplot <- renderPlot({
    a <- plot_domain_occ_by_year(
      dataset = dataset(),
      species = spp(),
      years = seq(years()[1], years()[2]),
      print_dataframe = T,
      title = paste(domain(), spp())
    )
    dt$occurrencedata <- a
  })

  output$biomassplot <- renderPlot({
    a <- plot_domain_biomass_by_year(
      dataset = dataset(),
      species = spp(),
      years = seq(years()[1], years()[2]),
      print_dataframe = T,
      title = paste(domain(), spp())
    )
    dt$biomassdata <- a
  })

  output$lenfreqplot <- renderPlot({
    plot_domain_LF_by_year(
      data = dataset(),
      species = spp(),
      bin_size = 5,
      title = paste(domain(), spp())
    )
  })

  output$data_table <- renderTable({
    observeEvent(input$whichMetric, {
      dt[[input$whichMetric]]
    })
    dt[[input$whichMetric]]
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$domain, "_",input$species,"_",input$whichMetric, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dt[[input$whichMetric]], file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
