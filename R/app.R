#### latest working version

# Install/Load Libraries
pacman::p_load(c(
  "tidyverse",
  "readr",
  "leaflet",
  "shiny",
  "shiny.semantic",
  "data.table",
  "DT",
  "testthat",
  "shinytest"),
  character.only = TRUE,
  install = TRUE)

# Include functions' scripts
# source("scripts/functions.R")

marineApp <- function(...) {
# Load data
vessels <- fread("data/vessels.csv")

# Grid Template
grid_graphics <- grid_template(
  default = list(
    areas = rbind(c("map", "chart")),
    rows_height = c("100%"),
    cols_width = c("70%", "30%")
  )
)

# UI
ui <- semanticPage(
  title = "Marine Traffic Dashboard",
  h2(
    class = "ui header", icon("ship"),
    div(
      class = "content", "Marine Traffic Dashboard",
      div(class = "sub header", "Automatic Identification System (AIS) Data")
    )
  ),
  br(),
  main_panel(
    flow_layout(
      dropdown_input(
        "port",
        c(unique(vessels$port[order(vessels$port)])),
        value = c(unique(vessels$port[order(vessels$port)]))[[1]]
      ),
      dropdown_input("vessel_type", NULL, value = NULL),
      dropdown_input("vessel_name", NULL, value = NULL),
      min_cell_width = "30%",
      column_gap = "20px"
    ),
    br(),
    segment(
      grid(grid_graphics,
           map = leafletOutput("map"),
           chart = plotOutput("plot")
      ),
      br(), hr(),
      h3(class = "ui header", div(class = "content", "Lat/Long Measurements")),
      br(),
      dataTableOutput("table")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Initialize reactive data
  ships <- reactiveValues(data = NULL)
  
  # Load & filter reactive data
  ships$data <- reactive(load_data(input$vessel_name))
  
  # Update dropdown choices
  observeEvent(input$port, {
    ships$vessel_type <- vessels %>%
      filter(port == input$port) %>%
      select(ship_type) %>%
      unique() %>%
      pull(1)
    
    update_dropdown_input(session, "vessel_type", choices = sort(ships$vessel_type))
  })
  
  observeEvent(c(input$port, input$vessel_type), {
    ships$vessel_name <- vessels %>%
      filter(ship_type == input$vessel_type & port == input$port) %>%
      select(shipname) %>%
      unique() %>%
      pull(1)
    
    update_dropdown_input(session, "vessel_name", choices = sort(ships$vessel_name))
  })
  
  # Leaflet map
  map <-  reactive({
    ships$data() %>%
      calculate_max_distance() %>%
      create_map()
  })
  
  # Plot
  plot_theme <-
    theme(
      text = element_text(size = 15),
      axis.text = element_text(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      legend.background = element_blank(),
      legend.title = element_blank(),
      legend.position = "none",
      panel.grid = element_line(colour = NULL),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.margin = unit(
        c(1, 1, 1, 1),
        "lines"
      )
    )
  
  plot <- reactive({
    ships$data() %>%
      ggplot(aes(x = port, y = distance, color = speed)) +
      geom_boxplot() +
      geom_jitter() +
      labs(
        title = paste0("Distances sailed by the ship ", "'", input$vessel_name, "'"),
        subtitle = "Distances between Lat/Long measurements, by port",
        x = NULL,
        y = "Distance (Meters)",
        color = "Speed"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold")
      )
  })
  
  # Outputs
  output$plot <- renderPlot(plot())
  output$map <- renderLeaflet(map())
  output$table <- renderDataTable(ships$data())
}

shinyApp(ui, server)
}