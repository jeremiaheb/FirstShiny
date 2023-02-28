library(shiny)
library(tidyverse)
library(rvc)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)
library(leaflet)
library(bslib)
library(geojsonio)
library(shinyjs)

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

keys_spas <- geojson_read("Data/fkeys_spas.geojson", what = "sp")
keys_domain <- geojson_read("Data/keys_domain.geojson", what = "sp")

ui <-
  navbarPage("NCRMP Atlantic Fish", collapsible = TRUE, inverse = TRUE, theme = bs_theme(bootswatch = "solar"),
             # Home panel ----
             tabPanel("Home",
                    HTML('
                          <section class="section-one"> 
                            <div class="container">
                              <h1>Landing Page</h1>
                              <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit</p>
                              <a href="" class="home-button">Read More</a>
                            </div>  
                          </section> 
 
                           <section class="section-two">
                              <div class="container-two">
                                <div class="container-two-content content">
                                  <h1>Title</h1>
                                  <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.</p>
                                  <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.</p>
                                </div>
                                <div class="container-two-content content-image">
                                  <img src="HYGE.jpg">
                                </div>
                              </div>
                           </section>
                         ')
             ),
             # Analysis Panel ---------
             tabPanel("Analysis",
                      fluidPage(tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "app.css")),
                                useShinyjs(),
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
                                    radioButtons("selectedArea", "Select part of Domain", choices = c("All Domain", "Open vs Protected", "Strata")),
                                    checkboxGroupInput("selectedPlots", "Select Plots", choices = c("Density", "Occurrence", "Biomass", "lengthFreq")),
                                    actionButton("build", "Build Species Plots"),
                                    width = 2
                                  ),
                                  mainPanel(
                                    tabsetPanel(
                                      tabPanel(class = "analysisPlots",
                                        "Plot",
                                        fluidRow(class = "plotRow",
                                          column(12,
                                            density_ui("x")
                                          )
                                        ),
                                        fluidRow(class = "plotRow",
                                            column(12,
                                              occurrence_ui("x")
                                          )
                                        ),
                                        fluidRow(class = "plotRow",
                                            column(12,
                                              biomass_ui("x")
                                          )
                                        ),
                                        fluidRow(class = "plotRow",
                                          column(12,
                                            lenfreq_ui("x")
                                          )
                                        )
                                      ),
                                      tabPanel("Map",
                                               fluidPage(
                                                 fluidRow(class = "plotRow",
                                                   column(12,
                                                          tags$div(class = "mappage",
                                                                   leafletOutput("mymap", width = "100%", height = 600)
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
                        fluidRow(class = "timeln",
                          column(12,
                        tags$iframe(src='https://cdn.knightlab.com/libs/timeline3/latest/embed/index.html?source=1B-iclSukiIB36Ny4M5wEBNddu7U_3XoJFHE6mQAvfFA&font=Default&lang=en&initial_zoom=2&height=650',
                                    width='100%',
                                    height='650',
                                    frameborder='0')
                          )
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
  
  selectedInputs <- eventReactive(input$build, {
    input$selectedPlots
  })
  
  selectedArea <- eventReactive(input$build, {
    input$selectedArea
  })

  dt <- reactiveValues()
      
  observeEvent(input$build, {
    output[["x-densityplot"]] <- NULL
    output[["x-occurrenceplot"]] <- NULL
    output[["x-biomassplot"]] <- NULL
    output[["x-lenfreqplot"]] <- NULL
    addClass("x-densityplot", "noPlot")
    addClass("x-occurrenceplot", "noPlot")
    addClass("x-biomassplot", "noPlot")
    addClass("x-lenfreqplot", "noPlot")
    
    if(!is.null(selectedInputs())) {
      
      nplot<-length(selectedInputs())
      x<-selectedInputs()
      a<-selectedArea()
      for (i in 1:nplot) {
        if (x[[i]] == "Density" && a == "All Domain") {
          density_server("x", dataset(), domain(), spp(), years())
          removeClass("x-densityplot", "noPlot")
        } else if (x[[i]] == "Density" && a == "Open vs Protected") {
          density_prot_server("x", dataset(), domain(), spp(), years())
          removeClass("x-densityplot", "noPlot")
          
        } else if (x[[i]] == "Occurrence" && a == "All Domain") {
          occurrence_server("x", dataset(), domain(), spp(), years())
          removeClass("x-occurrenceplot", "noPlot")
        } else if (x[[i]] == "Occurrence" && a == "Open vs Protected") {
          occurrence_prot_server("x", dataset(), domain(), spp(), years())
          removeClass("x-occurrenceplot", "noPlot")
          
        } else if (x[[i]] == "Biomass" && a == "All Domain") {
          biomass_server("x", dataset(), domain(), spp(), years())
          removeClass("x-biomassplot", "noPlot")
        } else if (x[[i]] == "Biomass" && a == "Open vs Protected") {
          biomass_prot_server("x", dataset(), domain(), spp(), years())
          removeClass("x-biomassplot", "noPlot")
          
        } else if (x[[i]] == "lengthFreq") {
          lenfreq_server("x", dataset(), domain(), spp())
          removeClass("x-lenfreqplot", "noPlot")
        }
      }
      
    }
  })
      
  output$mymap <- renderLeaflet({
    sites = dataset()$sample_data %>% filter(YEAR == max(dataset()$sample_data$YEAR)) %>% group_by(REGION, YEAR, PRIMARY_SAMPLE_UNIT, STATION_NR) %>% summarise(lat = mean(LAT_DEGREES), lon = mean(LON_DEGREES))
    # leaflet(sites) %>%
    #   addProviderTiles(providers$Esri.WorldImagery) %>%
    #   addMarkers(lng = ~lon, lat = ~lat,
    #              popup = ~PRIMARY_SAMPLE_UNIT)
    # popup = paste0("<img src = HYGE.jpg width=100 height=100>"))
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      addPolygons(data = keys_domain, stroke = FALSE,
                  color = "black",
                  popup = paste0('<div class="modal-content">
                                   <p>Some text in the Popup</p>
                                  </div>')) %>%
      addPolygons(data = keys_spas, stroke = 0.5,
                  color = "blue",
                  popup = paste0("<img src = HYGE.jpg width=100 height=100>")) %>%
      addMarkers(data = sites, 
                 lng = ~lon, lat = ~lat, 
                 popup = ~PRIMARY_SAMPLE_UNIT)
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
