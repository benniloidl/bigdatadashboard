library(shiny)
library(bslib)
library(plotly)
library(readr)
library(leaflet)
library(ggmap)
library(dplyr)

data <- read.csv("worldometer_coronavirus_daily_data.csv") %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) # Convert date to Date type

# Register your Google Maps API key
register_google(key = "")

# Load cached geocoding data if it exists, or fetch new data and save it
geocode_cache_file <- "geocode_cache.rds"

# Check if the cache file exists
if (file.exists(geocode_cache_file)) {
  # Load the cached geocode data
  geocode_cache <- readRDS(geocode_cache_file)
} else {
  # If cache doesn't exist, fetch geocoding data and cache it
  countries <- unique(data$country)
  geocode_cache <- data.frame(country = countries, lat = NA, lon = NA)
  
  for (i in 1:length(countries)) {
    country_name <- countries[i]
    coords <- geocode(country_name)
    if (!is.na(coords$lat) && !is.na(coords$lon)) {
      geocode_cache$lat[i] <- coords$lat
      geocode_cache$lon[i] <- coords$lon
    }
  }
  
  # Save the geocode data to the cache file
  saveRDS(geocode_cache, geocode_cache_file)
}

format_number <- function(x) {
  if (x >= 1e9) {
    # For numbers >= 1 billion (10^9), use "B"
    return(paste0(round(x / 1e9, 1), "B"))
  } else if (x >= 1e6) {
    # For numbers >= 1 million (10^6), use "M"
    return(paste0(round(x / 1e6, 1), "M"))
  } else if (x >= 1e3) {
    # For numbers >= 1 thousand (10^3), use "K"
    return(paste0(round(x / 1e3, 1), "K"))
  } else {
    # For numbers less than 1000, return the number itself
    return(as.character(x))
  }
}


ui <- shinyUI(fluidPage(
  includeCSS("styles.css"),  # Custom CSS for layout adjustments
  
  tags$style(HTML("
  
    .overlay-container > h2, .card {
      box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.5);
    }
  
    .selectize-input, .selectize-dropdown {
      background: #242E41 !important;
      border-radius: 16px !important;
      box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.5);
    }
    
    .selectize-dropdown {
      overflow: hidden;
    }
    
    .selectize-dropdown-content {
      padding: 0 !important;
    }
    
    .selectize-input::after {
      border-color: #ffffff transparent transparent transparent !important;
    }
    
    .right-value-box {
      background: #242E41 !important;
      pointer-events: all;
      height: 400px;
    }
    
    .right-value-box .value-box-showcase {
      max-height: none !important;
    }
    
    .right-value-box .value-box-area {
      min-height: 77px;
    }
    
    #barChart {
      overflow: hidden auto;
    }
    
    /* Ensure the plot area doesn't block scrolling */
    .plot-container .main-svg {
      pointer-events: none;
    }
    
    .plot-container .main-svg .scatterlayer {
      pointer-events: auto; /* Maintain interactivity for bars or hover layers */
    }
    
    .leaflet-popup-content-wrapper {
      background-color: #242E41 !important;
      border-radius: 16px;
      box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.5);
    }
    .leaflet-popup-content {
      font-size: 14px;
      color: #ffffff !important;
    }
    .leaflet-popup-tip {
      background-color: #242E41 !important;
    }

  ")),
  
  # Map as the full-screen background
  div(
    class = "background-map",
    leafletOutput("map", width = "100%", height = "100%")
  ),
  
  # Overlayed content
  div(
    class = "overlay-container",
    titlePanel(
      HTML(paste(
        "<span style='color: #ffffff; font-size: 0.8em;'>",
        textOutput("dynamicTitle"),
        "</span>",
        "<span style='color: #ffffff; opacity: .8; font-size: 10px; font-weight: 400; line-height: 2em;'>",
        textOutput("dynamicSubtitle"),
        "</span>"
      ))
    ),
    
    fluidRow(
      column(3,
             value_box( 
               textOutput("chart1Title"), 
               textOutput("chart1Subtitle"), 
               showcase = plotlyOutput("chart1"), 
               showcase_layout = "bottom",
               style = "margin-bottom: 1rem; background: #242E41; pointer-events: all;"
             ),
             value_box( 
               textOutput("chart2Title"), 
               textOutput("chart2Subtitle"), 
               showcase = plotlyOutput("chart2"), 
               showcase_layout = "bottom",
               style = "background: #242E41; pointer-events: all;"
             ),
             style = ""),
      column(6,
             layout_columns(
               selectInput( 
                 "select1", 
                 "Type:", 
                 list("New" = "new", "Cumulative" = "cumulative")
               ),
               selectInput( 
                 "select2", 
                 "Figures:", 
                 list("Positive Cases" = "positive_cases", "Deaths" = "deaths") 
               )
             ),
             style = "z-index: 2;"),
      column(3,
             value_box( 
               "Select a Country for more details", 
               textOutput("barChartTitle"), 
               showcase = plotlyOutput("barChart"), 
               showcase_layout = "bottom",
               class = "right-value-box"
             ),
             style = "overflow-y: auto; height: 100%; padding-bottom: 10px")
    ),
    
    style = "padding: 1rem; position: relative; z-index: 2;"
  )
))



server <- shinyServer(function(input, output, session) {
  
  # Reactive variable to store highlighted bar
  highlightedCountry <- reactiveVal(NULL)
  
  output$dynamicTitle <- renderText({
    if (!is.null(highlightedCountry())) {  # Check if a country is selected
      paste("Global COVID-19 Tracker - Selected:", highlightedCountry())
    } else {
      paste("Global COVID-19 Tracker")
    }
  })
  
  output$dynamicSubtitle <- renderText({
    # Get the earliest and latest date from the entire dataset
    earliest_date <- min(as.Date(data$date), na.rm = TRUE)
    latest_date <- max(as.Date(data$date), na.rm = TRUE)
    
    # Format the dates as "DD. MMM YYYY"
    formatted_earliest <- format(earliest_date, "%d. %b %Y")
    formatted_latest <- format(latest_date, "%d. %b %Y")
    
    # Create the date range string
    paste(formatted_earliest, "-", formatted_latest)
  })
  
  # Helper function to filter, aggregate, and get the most recent value
  get_most_recent_value <- function(selected_column) {
    filtered_data <- if (!is.null(highlightedCountry())) {
      data %>%
        filter(country == highlightedCountry()) %>%
        group_by(date) %>%
        summarise(value = sum(.data[[selected_column]], na.rm = TRUE)) %>%
        arrange(date) %>%
        slice_max(order_by = date, n = 1)  # Get the most recent (last) row
    } else {
      data %>%
        group_by(date) %>%
        summarise(value = sum(.data[[selected_column]], na.rm = TRUE)) %>%
        arrange(date) %>%
        slice_max(order_by = date, n = 1)  # Get the most recent (last) row
    }
    
    return(filtered_data$value)
  }
  
  output$chart1Title <- renderText({
    type <- ifelse(input$select1 == "new", "New", "Cumulative")
    paste(type, "Positive Cases")
  })
  
  output$chart1Subtitle <- renderText({
    # Map select1 input directly to column names
    selected_column <- switch(input$select1, "new" = "daily_new_cases", "cumulative" = "cumulative_total_cases")
    
    recent_value <- get_most_recent_value(selected_column)
    
    # Format and return the value
    paste(format_number(recent_value))
  })
  
  output$chart1 <- renderPlotly({
    # Map select1 input directly to column names
    selected_column <- switch(input$select1, "new" = "daily_new_cases", "cumulative" = "cumulative_total_cases")
    
    # Filter and aggregate data based on the highlighted country
    filtered_data <- if (!is.null(highlightedCountry())) {
      data %>% 
        filter(country == highlightedCountry()) %>% 
        group_by(date) %>% 
        summarise(value = sum(.data[[selected_column]], na.rm = TRUE)) %>%
        arrange(date)  # Sort by date
    } else {
      data %>% 
        group_by(date) %>% 
        summarise(value = sum(.data[[selected_column]], na.rm = TRUE)) %>%
        arrange(date)  # Sort by date
    }
    
    # Add custom hover text
    filtered_data <- filtered_data %>%
      mutate(
        hover_text = paste0(
          "<b>Date:</b> ", format(date, "%Y-%m-%d"), "<br>",
          "<b>Cases:</b> ", format(value, big.mark = ",")
        )
      )
    
    # Create the plot
    fig <- 
      plot_ly(filtered_data, height = 100) |> 
      add_lines( 
        x = ~date, 
        y = ~value, 
        color = I("#406EF1"), 
        fill = "tozeroy", 
        alpha = 0.2,
        text = ~hover_text,  # Add custom hover text
        hoverinfo = "text"   # Show only the custom hover text
      ) %>% 
      layout( 
        xaxis = list(visible = FALSE, showgrid = FALSE), 
        yaxis = list(visible = FALSE, showgrid = FALSE), 
        hovermode = "x", 
        margin = list(t = 0, r = 0, l = 0, b = 0), 
        paper_bgcolor = "transparent", 
        plot_bgcolor = "transparent" 
      ) 
    
    fig 
  })
  
  output$chart2Title <- renderText({
    type <- ifelse(input$select1 == "new", "New", "Cumulative")
    paste(type, "Deaths")
  })
  
  output$chart2Subtitle <- renderText({
    # Map select1 input directly to column names
    selected_column <- switch(input$select1, "new" = "daily_new_deaths", "cumulative" = "cumulative_total_deaths")
    
    recent_value <- get_most_recent_value(selected_column)
    
    # Format and return the value
    paste(format_number(recent_value))
  })
  
  output$chart2 <- renderPlotly({
    # Map select1 input directly to column names
    selected_column <- switch(input$select1, "new" = "daily_new_deaths", "cumulative" = "cumulative_total_deaths")
    
    # Filter and aggregate data based on the highlighted country
    filtered_data <- if (!is.null(highlightedCountry())) {
      data %>% 
        filter(country == highlightedCountry()) %>% 
        group_by(date) %>% 
        summarise(value = sum(.data[[selected_column]], na.rm = TRUE)) %>%
        arrange(date)  # Sort by date
    } else {
      data %>% 
        group_by(date) %>% 
        summarise(value = sum(.data[[selected_column]], na.rm = TRUE)) %>%
        arrange(date)  # Sort by date
    }
    
    # Add custom hover text
    filtered_data <- filtered_data %>%
      mutate(
        hover_text = paste0(
          "<b>Date:</b> ", format(date, "%Y-%m-%d"), "<br>",
          "<b>Cases:</b> ", format(value, big.mark = ",")
        )
      )
    
    # Create the plot
    fig <- 
      plot_ly(filtered_data, height = 100) |> 
      add_lines( 
        x = ~date, 
        y = ~value, 
        color = I("#406EF1"), 
        fill = "tozeroy", 
        alpha = 0.2,
        text = ~hover_text,  # Add custom hover text
        hoverinfo = "text"   # Show only the custom hover text
      ) %>% 
      layout( 
        xaxis = list(visible = FALSE, showgrid = FALSE), 
        yaxis = list(visible = FALSE, showgrid = FALSE), 
        hovermode = "x", 
        margin = list(t = 0, r = 0, l = 0, b = 0), 
        paper_bgcolor = "transparent", 
        plot_bgcolor = "transparent" 
      ) 
    
    fig 
  })
  
  # Render the initial map (static base map setup)
  output$map <- renderLeaflet({
    leaflet() |> 
      addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png",
               options = tileOptions(opacity = 1)) |> 
      setView(lng = 0, lat = 0, zoom = 1.5)
  })
  
  # Update the map circles dynamically based on the selected column
  observe({
    # Map select2 input directly to column names
    selected_column <- switch(input$select2, "positive_cases" = "daily_new_cases", "deaths" = "daily_new_deaths")
    
    # Summarize the data grouped by country
    bar_data <- data %>%
      group_by(country) %>%
      summarise(total_cases = sum(.data[[selected_column]], na.rm = TRUE))
    
    # Use leafletProxy to update the circles on the map
    leafletProxy("map") %>%
      clearShapes() %>%  # Clear existing circles before adding new ones
      {
        for (i in 1:nrow(bar_data)) {
          country_name <- bar_data$country[i]
          total_cases <- bar_data$total_cases[i]
          
          # Find the coordinates for the country from the cache
          coords <- geocode_cache %>% filter(country == country_name)
          
          if (nrow(coords) > 0 && !is.na(coords$lon) && !is.na(coords$lat)) {
            # Determine fill opacity based on whether the country is selected
            marker_fill_opacity <- if (!is.null(highlightedCountry()) && !is.na(highlightedCountry()) && country_name == highlightedCountry()) {
              0.8
            } else {
              0.3
            }
            
            # Add circles to the map based on total cases
            addCircles(
              .,
              lng = coords$lon, lat = coords$lat, 
              radius = total_cases / switch(input$select2, "positive_cases" = 50, "deaths" = 1),  # Adjust size based on total cases
              color = "#406EF1", fillOpacity = marker_fill_opacity, 
              stroke = FALSE,                 # Remove the border
              popup = paste(country_name, ": ", total_cases, " cases"),
              layerId = country_name  # Set the country name as layerId for easy reference
            )
          }
        }
        .
      }
  })
  
  # Capture the click event on the circles and update the highlightedCountry
  observeEvent(input$map_shape_click, {
    # Get the clicked country from the layerId
    clicked_country <- input$map_shape_click$id
    
    # Check if the clicked country is already selected
    if (!is.null(highlightedCountry()) && highlightedCountry() == clicked_country) {
      # If the clicked country is already selected, remove the selection
      highlightedCountry(NULL)
    } else {
      # Otherwise, update the highlightedCountry variable with the clicked country
      highlightedCountry(clicked_country)
    }
  })
  
  output$barChartTitle <- renderText({
    if (input$select2 == "positive_cases") paste("New Positive Cases")
    else paste("New Deaths")
  })
  
  # Render interactive bar chart
  output$barChart <- renderPlotly({
    # Map select2 input directly to column names
    selected_column <- switch(input$select2, "positive_cases" = "daily_new_cases", "deaths" = "daily_new_deaths")
    
    # Bar chart data based on the selected column
    bar_data <- data %>%
      group_by(country) %>%
      summarise(value = sum(.data[[selected_column]], na.rm = TRUE))  # Dynamically use selected column
    
    num_countries <- nrow(bar_data)  # Number of bars
    bar_height <- 36                # Minimum height per bar in pixels
    chart_height <- max(400, num_countries * bar_height)  # Ensure at least 400px height
    
    # Determine colors based on highlightedCountry
    bar_colors <- if (!is.null(highlightedCountry())) {
      ifelse(bar_data$country == highlightedCountry(), "white", "#406EF1")
    } else {
      rep("#406EF1", nrow(bar_data))  # Default to blue for all bars
    }
    text_colors <- if (!is.null(highlightedCountry())) {
      ifelse(bar_data$country == highlightedCountry(), "#406EF1", "white")
    } else {
      rep("white", nrow(bar_data))  # Default to blue for all bars
    }
    
    plot_ly(
      data = bar_data,
      x = ~value,
      y = ~country,
      type = 'bar',
      orientation = 'h',
      marker = list(color = bar_colors),  # Use dynamically determined colors
      text = ~paste0(country),  # Add labels directly on the bars
      textposition = "auto",  # Automatically position labels on the bars
      textfont = list(color = text_colors, size = 12),  # Customize text appearance
      height = chart_height,  # Set the height directly in plot_ly()
      source = "barChartSource"  # Specify the source name
    ) %>%
      layout(
        plot_bgcolor = "rgba(0, 0, 0, 0)",  # Transparent plot area
        paper_bgcolor = "rgba(0, 0, 0, 0)",  # Transparent entire canvas
        margin = list(l = 0, r = 10, t = 0, b = 0),  # Adjust margin for better spacing
        xaxis = list(
          title = "Cases",
          fixedrange = TRUE  # Disable zooming on the x-axis
        ),
        yaxis = list(
          title = "",
          showticklabels = FALSE,  # Remove labels on the left y-axis
          categoryorder = "total ascending",  # Sort the bars by total value in descending order
          fixedrange = TRUE  # Disable zooming on the y-axis
        ),
        dragmode = FALSE,  # Disable dragging (panning)
        hovermode = "closest",  # Hover over the closest point
        showlegend = FALSE  # Optionally hide the legend if not needed
      ) %>%
      config(scrollZoom = TRUE)  # Enable scrolling for large data
  })
  
  # Observe click events on the bar chart
  observeEvent(event_data("plotly_click", source = "barChartSource"), {
    click_data <- event_data("plotly_click", source = "barChartSource")
    if (!is.null(click_data)) {
      # Map select2 input directly to column names
      selected_column <- switch(input$select2, "positive_cases" = "daily_new_cases", "deaths" = "daily_new_deaths")
      
      # Bar chart data based on the selected column
      bar_data <- data %>%
        group_by(country) %>%
        summarise(value = sum(.data[[selected_column]], na.rm = TRUE))
      
      selected_country <- bar_data$country[click_data$pointNumber + 1]
      
      # Check if the clicked country is already selected
      if (!is.null(highlightedCountry()) && highlightedCountry() == selected_country) {
        # If the clicked country is already selected, remove the selection
        highlightedCountry(NULL)
      } else {
        # Otherwise, update the highlightedCountry variable with the clicked country
        highlightedCountry(selected_country)
      }
    }
  })
  
})

shinyApp(ui = ui, server = server)
