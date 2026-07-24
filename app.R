library(shiny)

ui <- fluidPage(
  titlePanel("My First Shiny App"),
  p("If you can see this text, it worked!")
)

server <- function(input, output) {
  # empty for now
}

shinyApp(ui, server)