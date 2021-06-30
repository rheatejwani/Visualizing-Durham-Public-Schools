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
bus <- read.csv("Bus Stops.csv")
childcare <- read.csv("Childcare Centers.csv")
cultural <- read.csv("Community & Cultural Centers.csv")
gardens <- read.csv("Community Gardens.csv")
grocery <- read.csv("Grocery Stores.csv")
libraries <- read.csv("Libraries.csv")
parks <- read.csv("Parks.csv")
rec <- read.csv("Recreation Centers.csv")
religious <- read.csv("Religious Centers.csv")
schools <- read.csv("School Statistics.csv")


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
                            choices = c("Parks", "Recreation Centers", "Gardens", "Bus Stops", 
                                        "Childcare Centers", "Community & Cultural Centers", 
                                        "Grocery Stores", "Libraries", "Religious Centers"),
                            multiple = FALSE),
                selectInput("zone",
                            label = "Choose a school zone to display",
                            choices = schools$SCHOOL_NAME,
                            multiple = FALSE)
            )
        ),
        fluidRow(
            box(width = 8,
                title = "Pop Up Description"),
            box(width = 4,
                title = "Additional Resources")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    displayVar <- reactive({
        switch(input$var,
               "Parks" = parks, 
               "Recreation Centers" = rec, 
               "Gardens" = gardens, 
               "Bus Stops" = bus, 
               "Childcare Centers" = childcare, 
               "Community & Cultural Centers" = cultural, 
               "Grocery Stores" = grocery, 
               "Libraries" = libraries, 
               "Religious Centers" = religious)
    })
    
    output$map <- renderLeaflet({
        leaflet(durham) %>%
            addProviderTiles("CartoDB.Positron") %>%
            addPolygons(data = durham,
                        fillColor = "lightblue",
                        stroke = TRUE,
                        fillOpacity = 0.75,
                        smoothFactor = 1) %>%
            addMarkers(data = displayVar(), lng = ~LONGITUDE, lat= ~LATITUDE)
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
