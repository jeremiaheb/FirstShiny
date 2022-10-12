library(shiny)
library(tidyverse)
library(rvc)


drto <- readRDS("Data/drytortugas.rds")
fkeys <- readRDS("Data/floridakeys.rds")

ui <- fluidPage(
  sliderInput(inputId = "years", 
              label = "Year Range",
              min = 2000, 
              max = 2021, 
              value = c(2000,2021),
              sep = ""),
  selectInput("domain",
              "Domain",
              c("Dry Tortugas", "Florida Keys"),
              selected = "Dry Tortugas"),
  textInput("species", 
            "Species", 
            value = "", 
            placeholder = "Abu saxa"),
  plotOutput(outputId = "timeplot"),
  tableOutput(outputId = "data_table")
)

server <- function(input, output, session) {
  
  updateSliderInput(session, "years",
                    min = 1999, max = 2021,
                    value = min, max
  )
  
  #dataset choice
  dataset <- reactive({
    if (input$domain == 'Dry Tortugas'){
      return(drto)
    } else if (input$domain == 'Florida Keys') {
      return(fkeys)
    } else {
      return(drto)
    }
  })
  
  dt <- reactiveVal()
  
  output$timeplot <- renderPlot({

    dens <- getDomainDensity(x = dataset(), species = input$species, years = seq(input$years[1], input$years[2]))
    dt(dens)
    ggplot(dens, aes(x = YEAR, y = density)) + geom_point() + geom_line()
  })
  
  output$data_table <- renderTable({

    dt()
  })

}

shinyApp(ui = ui, server = server)