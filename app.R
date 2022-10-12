library(shiny)
library(tidyverse)
library(rvc)

source("timeseries_density.R")

ui <- fluidPage(
  timeseries_UI("timeseries1")
)

server <- function(input, output, session) {
  timeseries_Server("timeseries1")
}

shinyApp(ui = ui, server = server)