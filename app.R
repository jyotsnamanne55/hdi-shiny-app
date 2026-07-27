library(shiny)
library(tidyverse)
library(DT)
library(plotly)
files <- list.files("data", pattern = "\\.csv$", full.names = TRUE)
hdi_data <- files |> map_df(read_csv)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .selectize-control.multi .selectize-input {
        position: relative;
        padding-right: 25px;
      }
      .selectize-control.multi .selectize-input::after {
        content: '▼';
        position: absolute;
        right: 10px;
        top: 50%;
        transform: translateY(-50%);
        color: #888;
        font-size: 12px;
        pointer-events: none;
      }
    ")),
  ),
  titlePanel(textOutput("dynamic_title")),
  sidebarLayout(
    sidebarPanel(fileInput(
      inputId = "uploaded_file",
      label = "Upload extra HDI data (hdro_indicators_COUNTRY.csv):",
      accept = ".csv"
     ),
    hr(),
    #country selection block
      selectizeInput(
        inputId = "selected_countries",
        label = "Select country/countries:",
        choices = unique(hdi_data$country_name),
        selected = NULL,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
    #slecting no of rows in table
    sliderInput(
      inputId = "num_rows",
      label = "Number of rows to display:",
      min = 1,
      max = nrow(hdi_data),
      value = 10
    ),
    #can select or deselect specific coloumns
    checkboxGroupInput(
      inputId = "selected_columns",
      label = "Columns to display:",
      choices = names(hdi_data),
      selected = names(hdi_data)
    )
    ,
    hr(),
    h4("Plot 1 controls"),
    #different aspects to plot
    selectInput(
      inputId = "plot1_indicator",
      label = "Select indicator:",
      choices = unique(hdi_data$indicator_name)
    ),
    sliderInput(
      inputId = "plot1_years",
      label = "Year range:",
      min = min(hdi_data$year),
      max = max(hdi_data$year),
      value = c(min(hdi_data$year), max(hdi_data$year)),
      sep = ""
    ),
    sliderInput(
      inputId = "plot1_linewidth",
      label = "Line thickness:",
      min = 0.5,
      max = 3,
      value = 1,
      step = 0.5
    ),
    hr(),
    h4("Plot 2 controls"),
    selectInput(
      inputId = "plot2_index",
      label = "Select index:",
      choices = unique(hdi_data$index_name)
    ),
    sliderInput(
      inputId = "plot2_year",
      label = "Select year:",
      min = min(hdi_data$year),
      max = max(hdi_data$year),
      value = max(hdi_data$year),
      sep = ""
    ),
    radioButtons(
      inputId = "plot2_sort",
      label = "Sort bars by:",
      choices = c("Value (High to Low)" = "value", "Alphabetical" = "alpha")
    )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table", DTOutput("data_table")),
        tabPanel("Plot 1", plotlyOutput("plot1")),
        tabPanel("Plot 2", plotlyOutput("plot2"))
      )
    )
  )
)

server <- function(input, output) {
  #trying to include dynamic file uploading as well
  full_data <- reactive({
    if (is.null(input$uploaded_file)) {
      hdi_data
    } else {
      uploaded <- read_csv(input$uploaded_file$datapath)
      bind_rows(hdi_data, uploaded)
    }
  })
  filtered_data <- reactive({
    req(input$selected_countries)
    full_data() |> filter(country_name %in% input$selected_countries)
  })
  observeEvent(input$uploaded_file, {
    updateSelectizeInput(
      inputId = "selected_countries",
      choices = unique(full_data()$country_name),
      selected = input$selected_countries
    )
  })
  #name should change when we select or deselect countries
  output$dynamic_title <- renderText({
    paste("Human Development Indicators:", paste(input$selected_countries, collapse = ", "))
  })
  output$data_table <- renderDT({
    validate(need(input$selected_countries, "Please select a country."))
    
    filtered_data() |> 
      select(all_of(input$selected_columns)) |> 
      head(input$num_rows)
  })
  #plot1
  output$plot1 <- renderPlotly({
    validate(need(input$selected_countries, "Please select a country."))
    
    plot_data <- filtered_data() |> 
      filter(
        indicator_name == input$plot1_indicator,
        year >= input$plot1_years[1],
        year <= input$plot1_years[2]
      )
    
    validate(need(nrow(plot_data) > 0, "No data available for this combination of country, indicator, and year range."))
    
    p <- ggplot(plot_data, aes(x = year, y = value, color = country_name)) +
      geom_line(linewidth = input$plot1_linewidth) +
      labs(
        title = input$plot1_indicator,
        x = "Year",
        y = "Value",
        color = "Country"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  #plot2 bar graph
  output$plot2 <- renderPlotly({
    validate(need(input$selected_countries, "Please select a country."))
    
    plot_data <- filtered_data() |> 
      filter(
        index_name == input$plot2_index,
        year == input$plot2_year
      )|> 
      group_by(country_name) |> 
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(plot_data) > 0, "No data available for this combination of country, index, and year."))
    
    if (input$plot2_sort == "value") {
      plot_data <- plot_data |> arrange(desc(value))
    } else {
      plot_data <- plot_data |> arrange(country_name)
    }
    
    plot_data$country_name <- factor(plot_data$country_name, levels = plot_data$country_name)
    
    p <- ggplot(plot_data, aes(x = country_name, y = value, fill = country_name)) +
      geom_col() +
      labs(
        title = paste(input$plot2_index, "-", input$plot2_year),
        x = "Country",
        y = "Value"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
}
#rendering the app
shinyApp(ui, server)