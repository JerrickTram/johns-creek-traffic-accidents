# Load libraries
library(DT)
library(geojsonsf)
library(lubridate)
library(leaflet)
library(plotly)
library(rsconnect)
library(sf)
library(shiny)
library(shinythemes)
library(tidyverse)

# Load John Creeks Accident Data
source("Scripts/load_geojson.R")
source("Scripts/clean_geojson.R")

# Define UI ----
ui <- fluidPage(theme = shinytheme("united"),
  titlePanel("Traffic Accidents in Johns Creek"),
  
  sidebarLayout(
    sidebarPanel(
      p("Create a map showing the location of accidents in the metro Atlanta area."),
      dateRangeInput("daterange", "Date Range", 
        start = min(as.Date(df$DateTimeOccurred)), 
        end = max(as.Date(df$DateTimeOccurred))
        ),
      selectInput("collisions", "Collision Type",
        choices = unique(df$MannerOfCollision),
        multiple = TRUE,
        selected = "Angle"
      ),
      selectInput("weather", "Weather",
        choices = unique(df$WeatherCondition),
        multiple = TRUE,
        selected = "Clear"
      ),
      selectInput(
        "lighting", "Lighting",
        choices = unique(df$LightingCondition),
        multiple = TRUE,
        selected = "Daylight"
      ),
      br(),
      p(strong("Disclaimer:"), "In the 2nd tab, peak hours are defined as 
        time periods between 7 to 9 AM and 4 to 6 PM.")
    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Total", dataTableOutput("totals")),
        tabPanel("Graphs", fluidRow(
          column(6, plotlyOutput("annual"), plotlyOutput("weekly")),
          column(6, plotlyOutput("monthly"), plotlyOutput("daily"))
        ))
      )
    )
  )
)

# Define server logic ----
server <- function(input, output) {
# Create reactive inputs
## This reactive expression filters the map based on date range selected
reactive_data = reactive({
  df %>% 
    filter(DateTimeOccurred >= input$daterange[1] & 
             DateTimeOccurred <= input$daterange[2],
           MannerOfCollision %in% input$collisions,
           WeatherCondition %in% input$weather,
           LightingCondition %in% input$lighting) 
  })

## This reactive expression grabs the proportions of peak/off-peak hour crashes
reactive_totals = reactive({
  as.data.frame(reactive_data()) %>% 
    mutate(PeakHour = ifelse(hour(DateTimeOccurred) >= 7 & hour(DateTimeOccurred) <= 9 |
                               hour(DateTimeOccurred) >= 16 & hour(DateTimeOccurred) <= 18, 
                             "Peak", "Off-Peak")) %>% 
    group_by(PeakHour, MannerOfCollision, WeatherCondition, LightingCondition) %>% 
    summarise(Total = n(),
              Percent = round(Total / nrow(reactive_data()), 2)) %>% 
    arrange(desc(Total)) 
})
  
# Generate leaflet map of accident locations with other information  
output$map = renderLeaflet({
  leaflet(reactive_data()) %>% 
    addTiles() %>% 
    addMarkers(clusterOptions = markerClusterOptions(),
               popup = paste("DateTime:", reactive_data()$DateTimeOccurred, "<br>",
                             "Address/Street:", reactive_data()$AddressOrStreetName, "<br>",
                             "Collision Type:", reactive_data()$MannerOfCollision, "<br>",
                             "Weather:", reactive_data()$WeatherCondition, "<br>",
                             "Lighting:", reactive_data()$LightingCondition))
  })

output$totals = renderDataTable({
  datatable(reactive_totals(),
            extensions = "Buttons",
            options = list(
              dom = 'Bfrtip',
              buttons = c('copy', 'csv', 'pdf', 'print')
          )
      )
  })

output$annual = renderPlotly({
  annual = reactive_data() %>% 
    mutate(Year = year(DateTimeOccurred)) %>% 
    group_by(Year) %>% 
    summarize(Accidents = n()) %>% 
    ggplot(aes(Year, Accidents)) +
    geom_line() +
    geom_point() +
    labs(title = "Annual Accidents") +
    scale_x_continuous(breaks = c(2010, 2015, 2020))
  
  ggplotly(annual)
  })

output$monthly = renderPlotly({  
  monthly = reactive_data() %>% 
    filter(DateTimeOccurred >= max(DateTimeOccurred) - months(6)) %>% 
    mutate(Month = month(DateTimeOccurred, label = TRUE, abbr = TRUE)) %>% 
    group_by(Month) %>% 
    summarize(Accidents = n()) %>% 
    ggplot(aes(Month, Accidents)) +
    geom_col() +
    labs(title = "Accidents from the Last 6 Months")

  ggplotly(monthly)
  })

output$weekly = renderPlotly({  
  weekly = reactive_data() %>% 
    filter(DateTimeOccurred >= max(DateTimeOccurred) - weeks(4)) %>% 
    mutate(Week = week(DateTimeOccurred)) %>% 
    group_by(Week) %>% 
    summarize(Accidents = n()) %>% 
    ggplot(aes(Week, Accidents)) +
    geom_col() +
    labs(title = "Accidents from the Last 4 to 5 Weeks")
  
  ggplotly(weekly)
  })

output$daily = renderPlotly({
  daily = reactive_data() %>% 
    filter(DateTimeOccurred >= max(DateTimeOccurred) - days(7)) %>% 
    mutate(Day = day(DateTimeOccurred)) %>% 
    group_by(Day) %>% 
    summarize(Accidents = n()) %>% 
    ggplot(aes(Day, Accidents)) +
    geom_line() +
    geom_point() +
    labs(title = "Accidents from the last 7 to 8 days") +
    scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10))
  
  ggplotly(daily)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
