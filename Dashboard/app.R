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


iconSet <- iconList(
    parks = makeIcon("https://img.icons8.com/material-rounded/24/000000/deciduous-tree.png", iconWidth=20, iconHeight=20),
    rec = makeIcon("https://img.icons8.com/ios-glyphs/30/000000/children.png", iconWidth=20, iconHeight=20),
    gardens = makeIcon("https://img.icons8.com/doodle/48/000000/flower--v1.png", iconWidth=20, iconHeight=20),
    bus = makeIcon("https://img.icons8.com/material-rounded/24/000000/bus.png", iconWidth=20, iconHeight=20),
    childcare = makeIcon("https://img.icons8.com/material-rounded/24/000000/rocking-horse.png", iconWidth=20, iconHeight=20),
    cultural = makeIcon("https://img.icons8.com/ios-filled/30/000000/crowd.png", iconWidth=20, iconHeight=20),
    grocery = makeIcon("https://img.icons8.com/ios-glyphs/30/000000/grocery-store.png", iconWidth=20, iconHeight=20),
    libraries = makeIcon("https://img.icons8.com/fluent-systems-filled/50/000000/book.png", iconWidth=20, iconHeight=20),
    religious = makeIcon("https://img.icons8.com/ios-filled/50/000000/chapel.png", iconWidth=20, iconHeight=20)
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
                            choices = c("Bus Stops", 
                                        "Childcare Centers", "Community & Cultural Centers", "Gardens",
                                        "Grocery Stores", "Libraries", "Parks", 
                                        "Recreation Centers", "Religious Centers"),
                            multiple = FALSE),
                selectInput("zone",
                            label = "Choose a school zone to display",
                            choices = c("All", "C.C. Spaulding Elementary", "Eastway Elementary",
                                        "E.K. Powe Elementary", "Fayetteville Street Elementary", 
                                        "Forest View Elementary", "Hillside High",
                                        "Jordan High School","Lakewood Elementary", 
                                        "Parkwood Elementary", "Southwest Elementary"),
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
               "Community Gardens" = gardens, 
               "Bus Stops" = bus, 
               "Childcare Centers" = childcare, 
               "Community & Cultural Centers" = cultural, 
               "Grocery Stores" = grocery, 
               "Libraries" = libraries, 
               "Religious Centers" = religious)
    })
    
    displayIcon <- reactive({
        switch(input$var,
               "Parks" = iconSet$parks, 
               "Recreation Centers" = iconSet$rec, 
               "Gardens" = iconSet$gardens, 
               "Bus Stops" = iconSet$bus, 
               "Childcare Centers" = iconSet$childcare, 
               "Community & Cultural Centers" = iconSet$cultural, 
               "Grocery Stores" = iconSet$grocery, 
               "Libraries" = iconSet$libraries, 
               "Religious Centers" = iconSet$religious)
  })
    
    displaySchool <- reactive({
        schools %>% filter(name == input$zone)
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
                       label = displayVar()$name, icon = displayIcon()) %>%
            addMarkers(data = displaySchool(), lng = ~LONGITUDE, lat = ~LATITUDE)
    })
}



# Run the application 
shinyApp(ui = ui, server = server)