library(shiny)
library(tidyverse)
library(rvc)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)
library(leaflet)
library(bslib)

myFiles <- list.files("plots/", pattern = "*.R", full.names = T)
sapply(myFiles, source)
species <- read.csv("Data/speciesList_short.csv")
drto <- readRDS("Data/drto_samll.rds")
keys <- readRDS("Data/keys_small.rds")
sefl <- readRDS("Data/sefl_small.rds")

source("mod-density.R")
source("mod-occurrence.R")
source("mod-biomass.R")
source("mod-lenfreq.R")



ui <-
  navbarPage("NCRMP Atlantic Fish", collapsible = TRUE, inverse = TRUE, theme = bs_theme(bootswatch = "sketchy"),
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
                                          column(12,
                                            density_ui("x")  
                                          )
                                        ),
                                        fluidRow(
                                          column(6,
                                            occurrence_ui("x")
                                          ),
                                          column(6,
                                            biomass_ui("x")
                                          )
                                        ),
                                        fluidRow(
                                          column(12,
                                            lenfreq_ui("x")
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
             tabPanel("Outreach",
                      fluidPage(
                        tags$iframe(src='https://cdn.knightlab.com/libs/timeline3/latest/embed/index.html?source=1B-iclSukiIB36Ny4M5wEBNddu7U_3XoJFHE6mQAvfFA&font=Default&lang=en&initial_zoom=2&height=650',
                                    width='100%',
                                    height='650',
                                    frameborder='0')
                      )
             ),
             # about panel --------
             tabPanel("About")
  )
# Server ----
server <- function(input, output, session) {

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
    density_server("x", dataset(), domain(), spp(), years())
    occurrence_server("x", dataset(), domain(), spp(), years())
    biomass_server("x", dataset(), domain(), spp(), years())
    lenfreq_server("x", dataset(), domain(), spp())
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
