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
library(tidyverse)

#GeoJson Data
durham <- geojsonio::geojson_read("Ten Schools.geojson", what = "sp")
cc <- geojsonio::geojson_read("CC Spaulding.geojson", what = "sp")
eastway <- geojsonio::geojson_read("Eastway.geojson", what = "sp")
ek <- geojsonio::geojson_read("EK Powe.geojson", what = "sp")
fayetteville <- geojsonio::geojson_read("Fayetteville St.geojson", what = "sp")
forest <- geojsonio::geojson_read("Forest View.geojson", what = "sp")
hillside <- geojsonio::geojson_read("Hillside.geojson", what = "sp")
jordan <- geojsonio::geojson_read("CEJordan.geojson", what = "sp")
lakewood <- geojsonio::geojson_read("Lakewood.geojson", what = "sp")
parkwood <- geojsonio::geojson_read("Parkwood.geojson", what = "sp")
southwest <- geojsonio::geojson_read("Southwest.geojson", what = "sp")


#Spatial Data
bus <- read.csv("renamed_Bus Stops.csv")
childcare <- read.csv("renamed_Childcare Centers.csv")
cultural <- read.csv("renamed_Community & Cultural Centers.csv")
gardens <- read.csv("renamed_Community Gardens.csv")
grocery <- read.csv("renamed_Grocery Stores.csv")
libraries <- read.csv("renamed_Libraries.csv")
parks <- read.csv("renamed_Parks.csv")
rec <- read.csv("renamed_Recreation Centers.csv")
religious <- read.csv("renamed_Religious Centers.csv")
schools <- read.csv("renamed_School Statistics.csv")

#Icons
Icons <- iconList(
    parks = makeIcon("icons8-oak-tree-24.png", 25, 25),
    rec = makeIcon("icons8-children-30.png", 25, 25),
    gardens = makeIcon("icons8-garden-gloves-50.png", 25, 25),
    bus = makeIcon("icons8-24.png", 25, 25),
    childcare = makeIcon("icons8-rocking-horse-24.png", 25, 25),
    cultural = makeIcon("icons8-people-30.png", 25, 25),
    grocery = makeIcon("icons8-grocery-bag-30.png", 25, 25),
    libraries = makeIcon("icons8-book-30.png", 25, 25),
    religious = makeIcon("icons8-chapel-30.png", 25, 25)
)


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
                            choices = c("C.C. Spaulding Elementary", "Eastway Elementary",
                                        "E.K. Powe Elementary", "Fayetteville Street Elementary", 
                                        "Forest View Elementary", "Hillside High",
                                        "Jordan High School","Lakewood Elementary", 
                                        "Parkwood Elementary", "Southwest Elementary", "All"),
                            multiple = FALSE)),
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
    
    displayIcon <- reactive({
        switch(input$var,
               "Parks" = Icons$parks, 
               "Recreation Centers" = Icons$rec, 
               "Gardens" = Icons$gardens, 
               "Bus Stops" = Icons$bus, 
               "Childcare Centers" = Icons$childcare, 
               "Community & Cultural Centers" = Icons$cultural, 
               "Grocery Stores" = Icons$grocery, 
               "Libraries" = Icons$libraries, 
               "Religious Centers" = Icons$religious)
    })
    
    displayZone <- reactive({
        switch(input$zone,
               "C.C. Spaulding Elementary" = cc, 
               "Eastway Elementary" = eastway,
               "E.K. Powe Elementary" = ek, 
               "Fayetteville Street Elementary" = fayetteville, 
               "Forest View Elementary" = forest,
               "Hillside High" = hillside,
               "Jordan High School" = jordan,
               "Lakewood Elementary" = lakewood, 
               "Parkwood Elementary" = parkwood, 
               "Southwest Elementary" = southwest, 
               "All" = durham)
    })
    
    displayColor <- reactive({
        switch(input$zone,
               "C.C. Spaulding Elementary" = "red", 
               "Eastway Elementary" = "orange",
               "E.K. Powe Elementary" = "yellow", 
               "Fayetteville Street Elementary" = "green", 
               "Forest View Elementary" = "blue",
               "Hillside High" = "violet",
               "Jordan High School" = "pink",
               "Lakewood Elementary" = "darkred", 
               "Parkwood Elementary" = "lightblue", 
               "Southwest Elementary" = "brown", 
               "All" = "gray")
    })
    
    output$map <- renderLeaflet({
        leaflet(displayZone()) %>%
            addProviderTiles("CartoDB.Positron") %>%
            addPolygons(data = displayZone(),
                        fillColor = displayColor(),
                        stroke = TRUE,
                        fillOpacity = 0.75,
                        smoothFactor = 1) %>%
            addMarkers(data = displayVar(), lng = ~LONGITUDE, lat= ~LATITUDE, 
                       #displayIcon is not reacting - to show default icons remove 'icon = displayIcon()'
                       label = displayVar()$name, icon = displayIcon())
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
