occurrence_ui <- function(id) {
  
  withSpinner(
    plotOutput(NS(id, "occurrenceplot")),
    type = 1,
    color.background = "white",
    hide.ui = TRUE
  )
}

occurrence_server <- function(id,data, domain, species, years) {
  moduleServer(id, function(input, output, session)
    output$occurrenceplot <- renderPlot({
      a <- plot_domain_occ_by_year(
        dataset = data,
        species = species,
        years = seq(years[1], years[2]),
        print_dataframe = T,
        title = paste(domain, species)
      )
    }))
  
}