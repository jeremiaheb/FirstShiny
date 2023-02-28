biomass_ui <- function(id) {
  plotOutput(NS(id, "biomassplot"))
}

biomass_server <- function(id,data, domain, species, years) {
  moduleServer(id, function(input, output, session)
    output$biomassplot <- renderPlot({
      a <- plot_domain_biomass_by_year(
        dataset = data,
        species = species,
        years = seq(years[1], years[2]),
        print_dataframe = T,
        title = paste(domain, species)
      )
    }))
  
}

biomass_prot_server <- function(id,data, domain, species, years) {
  moduleServer(id, function(input, output, session)
    output$biomassplot <- renderPlot({
      a <- plot_domain_biomass_by_year_by_prot(
        dataset = data,
        species = species,
        years = seq(years[1], years[2]),
        print_dataframe = T,
        title = paste(domain, species)
      )
    }))
  
}