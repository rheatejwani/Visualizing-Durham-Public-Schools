#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Packages
library(shiny)
library(shinydashboard)
library(leaflet)

#Data
durham <- geojsonio::geojson_read("Ten Schools.geojson", what = "sp")
parks <- read.csv("Parks.csv")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Visualizing Durham Public Schools"),
    dashboardSidebar(),
    dashboardBody(
        fluidRow(
            box(width  = 8,
                title = "Map",
                leafletOutput("map")),
            box(width = 4,
                title = "Drop Down Select",
                selectInput("var",
                            label = "Choose a variable to display",
                            choices = c("Parks", "Recreation Centers", "Gardens")),
                selectInput("zone",
                            label = "Choose a school zone to display",
                            choices = c("C C Spaulding", "Jordan High", "EK Powe")),
            )
        ),
        fluidRow(
            box(width = 8,
                title = "Pop Up Description"),
            box(width = 4,
                title = "Additional Resources"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$map <- renderLeaflet({
        leaflet(durham) %>%
            addProviderTiles("CartoDB.Positron") %>%
            addPolygons(data = durham,
                        fillColor = "lightblue",
                        stroke = TRUE,
                        fillOpacity = 0.75,
                        smoothFactor = 1) %>%
            addCircleMarkers(data = parks, lng = ~LONGITUDE, lat= ~LATITUDE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
