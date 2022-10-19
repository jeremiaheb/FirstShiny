
drto <- readRDS("Data/dt.rds")
fkeys <- readRDS("Data/fk.rds")
sefcri <- readRDS("Data/se.rds")

timeseries_UI <- function(id) {
  tagList(
    sliderInput(NS(id,"years"), 
                label = "Year Range",
                min = 2000, 
                max = 2021, 
                value = c(2000,2021),
                sep = ""),
    selectInput(NS(id, "domain"),
                "Domain",
                c("Dry Tortugas", "Florida Keys", "SE Florida"),
                selected = "Dry Tortugas"),
    selectInput(NS(id, "metric"),
                "Metric",
                c("Density", "Occurrence", "Biomass"),
                selected = "Density"),
    textInput(NS(id, "species"), 
              "Species", 
              value = "", 
              placeholder = "Abu saxa"),
    plotOutput(NS(id, "timeplot")),
    tableOutput(NS(id, "data_table"))
  )
}

timeseries_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
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
      } else if (input$domain == 'SE Florida') {
        return(sefcri)
      }
    })
    
    dt <- reactiveVal()
    
    output$timeplot <- renderPlot({
      
      if (input$metric == "Density"){
        dens <- getDomainDensity(x = dataset(), species = input$species, years = seq(input$years[1], input$years[2]))
        dt(dens)
        ggplot(dens, aes(x = YEAR, y = density)) + geom_point() + geom_line()
      } else if (input$metric == "Occurrence"){
        occ <- getDomainOccurrence(x = dataset(), species = input$species, years = seq(input$years[1], input$years[2]))
        dt(occ)
        ggplot(occ, aes(x = YEAR, y = occurrence)) + geom_point() + geom_line()        
      } else if (input$metric == "Biomass"){
        bio <- getDomainBiomass(x = dataset(), species = input$species, years = seq(input$years[1], input$years[2]))
        dt(bio)
        ggplot(bio, aes(x = YEAR, y = biomass)) + geom_point() + geom_line()
      }
    })
    
    output$data_table <- renderTable({
      
      dt()
    })
    
  })
}