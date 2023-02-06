density_ui <- function(id) {

    withSpinner(
      plotOutput(NS(id, "densityplot")),
      type = 1,
      color.background = "white",
      hide.ui = TRUE
    )
}

density_server <- function(id,data, domain, species, years) {
  moduleServer(id, function(input, output, session)
    output$densityplot <- renderPlot({
      a <- plot_domain_den_by_year(
        dataset = data,
        species = species,
        years = seq(years[1], years[2]),
        print_dataframe = T,
        title = paste(domain, species)
      )
    }))
  
}