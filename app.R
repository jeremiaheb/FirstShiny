library(shiny)
library(tidyverse)
library(rvc)
library(shinycssloaders)
library(shinythemes)


myFiles <- list.files("plots/", pattern = "*.R", full.names = T)
sapply(myFiles, source)
species <- read.csv("Data/speciesList.csv")

ui <- fluidPage(theme = shinytheme("yeti"),
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
      # submitButton("Build Species Plots"),
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
                type = 3,
                color.background = "white"
              )
            )
          ),
          fluidRow(
            column(
              6,
              withSpinner(
                plotOutput("occurrenceplot"),
                type = 3,
                color.background = "white"
              )
            ),
            column(
              6,
              withSpinner(
                plotOutput("biomassplot"),
                type = 3,
                color.background = "white"
              )
            )
          ),
          fluidRow(
            column(
              12,
              withSpinner(
                plotOutput("lenfreqplot"),
                type = 3,
                color.background = "white"
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
)
server <- function(input, output) {

  # dataset choice
  dataset <- reactive({
    if (input$domain == "Dry Tortugas") {
      return(readRDS("stash/drytortugas.rds"))
    } else if (input$domain == "Florida Keys") {
      return(readRDS("stash/floridakeys.rds"))
    } else if (input$domain == "SE Florida") {
      return(readRDS("stash/seflorida.rds"))
    }
  })

  dt <- reactiveValues()

  output$densityplot <- renderPlot({
    a <- plot_domain_den_by_year(
      dataset = dataset(),
      species = input$species,
      years = seq(input$years[1], input$years[2]),
      print_dataframe = T,
      title = paste(input$domain, input$species)
    )
    dt$densitydata <- a
  })

  output$occurrenceplot <- renderPlot({
    a <- plot_domain_occ_by_year(
      dataset = dataset(),
      species = input$species,
      years = seq(input$years[1], input$years[2]),
      print_dataframe = T,
      title = paste(input$domain, input$species)
    )
    dt$occurrencedata <- a
  })

  output$biomassplot <- renderPlot({
    a <- plot_domain_biomass_by_year(
      dataset = dataset(),
      species = input$species,
      years = seq(input$years[1], input$years[2]),
      print_dataframe = T,
      title = paste(input$domain, input$species)
    )
    dt$biomassdata <- a
  })

  output$lenfreqplot <- renderPlot({
    plot_domain_LF_by_year(
      data = dataset(),
      species = input$species,
      bin_size = 5,
      title = paste(input$domain, input$species)
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
