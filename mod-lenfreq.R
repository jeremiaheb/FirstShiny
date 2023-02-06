lenfreq_ui <- function(id) {
  
  withSpinner(
    plotOutput(NS(id, "lenfreqplot")),
    type = 1,
    color.background = "white",
    hide.ui = TRUE
  )
}

lenfreq_server <- function(id,data, domain, species) {
  moduleServer(id, function(input, output, session)
    output$lenfreqplot <- renderPlot({
      plot_domain_LF_by_year(
        data = data,
        species = species,
        bin_size = 5,
        title = paste(domain, species)
      )
    })
    )
  
}