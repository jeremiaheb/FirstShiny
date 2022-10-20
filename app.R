library(shiny)
library(tidyverse)
library(rvc)


myFiles = list.files("plots/", pattern = "*.R", full.names = T)
sapply(myFiles, source)

ui <- fluidPage(

  titlePanel("Fish"),
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput("years", 
                  label = "Year Range",
                  min = 2000, 
                  max = 2021, 
                  value = c(2000,2021),
                  sep = ""),
      selectInput("domain",
                  "Domain",
                  c("Dry Tortugas", "Florida Keys", "SE Florida"),
                  selected = "Dry Tortugas"),
      textInput("species", 
                "Species", 
                value = "MYC BONA", 
                placeholder = "MYC BONA"),
    width = 2),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", 
                 fluidRow(
                    column(12,plotOutput("densityplot"))
                    ),
                 fluidRow(
                    column(6, plotOutput("occurrenceplot")
                    ),
                    column(6, plotOutput("biomassplot"))
                    ),
                 fluidRow(
                    column(12, plotOutput("lenfreqplot"))
                    )
                 ),
        tabPanel("Table", 
                 fluidRow(
                    column(12, tableOutput("data_table"))
                 ),
                )
              )
            )
  )
)
server <- function(input, output) {
  #dataset choice
  dataset <- reactive({
    if (input$domain == 'Dry Tortugas'){
      return(readRDS("stash/drytortugas.rds"))
    } else if (input$domain == 'Florida Keys') {
      return(readRDS("stash/floridakeys.rds"))
    } else if (input$domain == 'SE Florida') {
      return(readRDS("stash/seflorida.rds"))
    }
  })
  
  dt <- reactiveVal()
  
  output$densityplot <- renderPlot({
      a <- plot_domain_den_by_year(dataset = dataset(),
                                 species = input$species,
                                 years = seq(input$years[1], input$years[2]),
                                 print_dataframe = T,
                                 title = paste(input$domain, input$species))
      dt(a)
  })
  
  output$occurrenceplot <- renderPlot({
      a <- plot_domain_occ_by_year(dataset = dataset(),
                                   species = input$species,
                                   years = seq(input$years[1], input$years[2]),
                                   print_dataframe = T,
                                   title = paste(input$domain, input$species))
      dt(a)
  })
  
  output$biomassplot <- renderPlot({
      a <- plot_domain_biomass_by_year(dataset = dataset(),
                                       species = input$species,
                                       years = seq(input$years[1], input$years[2]),
                                       print_dataframe = T,
                                       title = paste(input$domain, input$species))
      dt(a)
  })
  
  output$lenfreqplot <- renderPlot({
    plot_domain_LF_by_year(data = dataset(),
                           species = input$species, 
                           bin_size = 5,
                           title = paste(input$domain, input$species))
  })
  
  output$data_table <- renderTable({
    dt()
  })
}
shinyApp(ui, server)
