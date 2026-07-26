library(shiny)
library(tidyverse)

files <- list.files("data", pattern = "\\.csv$", full.names = TRUE)
hdi_data <- files |> map_df(read_csv)

ui <- fluidPage(
  titlePanel("My First Shiny App"),
  p("checkkkk")
)

server <- function(input, output) {
}

shinyApp(ui, server)