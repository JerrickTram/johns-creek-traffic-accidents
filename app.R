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
        tabPanel("Total", dataTableOutput("totals"))
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
  as.data.frame(reactive_data()) %>% # Removes sf class
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

}

# Run the app ----
shinyApp(ui = ui, server = server)
