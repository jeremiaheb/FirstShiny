library(shiny)
library(tidyverse)
library(rvc)


source("timeseries_density.R")
myFiles = list.files("Data/", pattern = "*.R", full.names = T)
sapply(myFiles, source)

ui <- fluidPage(
  
  titlePanel("Tabsets"),
  
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
      selectInput("metric",
                  "Metric",
                  c("Density", "Occurrence", "Biomass"),
                  selected = "Density"),
      textInput("species", 
                "Species", 
                value = "MYC BONA", 
                placeholder = "MYC BONA"),
    width = 2),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("timeplot")),
        tabPanel("Table", tableOutput("data_table"))
      )
    )
  )
)
server <- function(input, output) {
  #dataset choice
  dataset <- reactive({
    if (input$domain == 'Dry Tortugas'){
      return(drto)
    } else if (input$domain == 'Florida Keys') {
      return(fkeys)
    } else if (input$domain == 'SE Florida') {
      return(sefcri)
    }
  })
  
  dt <- reactiveVal()
  
  output$timeplot <- renderPlot({
    
    if (input$metric == "Density"){
      # dens <- getDomainDensity(x = dataset(), species = input$species, years = seq(input$years[1], input$years[2]))
      # dt(dens)
      # ggplot(dens, aes(x = YEAR, y = density)) + geom_point() + geom_line()
      a <- plot_domain_den_by_year(dataset = dataset(), 
                                   species = input$species, 
                                   years = seq(input$years[1], input$years[2]),
                                   print_dataframe = T,
                                   title = paste(input$domain, input$species))
      dt(a)
    } else if (input$metric == "Occurrence"){
      a <- plot_domain_occ_by_year(dataset = dataset(), 
                                   species = input$species, 
                                   years = seq(input$years[1], input$years[2]), 
                                   print_dataframe = T,
                                   title = paste(input$domain, input$species))
      dt(a)
    } else if (input$metric == "Biomass"){
      a <- plot_domain_biomass_by_year(dataset = dataset(), 
                                       species = input$species, 
                                       years = seq(input$years[1], input$years[2]), 
                                       print_dataframe = T,
                                       title = paste(input$domain, input$species))
      dt(a)
    }
  })
  
  output$data_table <- renderTable({
    
    dt()
  })
}
shinyApp(ui, server)
