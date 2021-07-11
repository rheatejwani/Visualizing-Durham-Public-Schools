
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
library(plotly)
library(dplyr)
library(tidyr)
library(readxl)


#Load Data

Race_SCHOOL_ONLY <- read_excel("~/Desktop/Race SCHOOL ONLY.xlsx")
data_per_student <- read_excel("~/Desktop/data per student.xlsx")
race <- read_excel("~/Desktop/race.xlsx")
race_diff <- read_excel("~/Desktop/race diff.xlsx")
poc_per_school <- read_excel("~/Desktop/poc per school.xlsx")
funding <- read_excel("~/Desktop/funding.xlsx")
all_race <- read_excel("~/Desktop/all race.xlsx")
Data_School_Info <- read_excel("~/Documents/ Other/Data+/DPS Spreadsheets/Data + School Info.xlsx")


# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Visualizing Durham Public Schools"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("School Statistics", tabName = "Tab Example", icon=icon("dashboard"))
        )
    ),
    dashboardBody(
        fluidRow(
            #Box Plot Outputs
            box(plotlyOutput("barplots",width="auto",height = "auto")
            ),
            #Drop Down Widget for Box Plots
            box(box(width = "auto",
                    title = "Select a Measurement",
                    selectInput("select", "Click the drop down menu to select which measurement you would like to view.", 
                                choices = list("Enrollment", "School and Zone Racial Breakdown", "Racial Differential", "POC per School", 
                                               "Funding per Pupil", "Racial Demographics", "Race per School", "Household Income", 
                                               "Homesale Price", "Bachelor Degree Rate", "Sidewalk Coverage", "Diversity per District"))),
                box(width = "auto",
                    title = "Resources",
                    textOutput("resources")
                )
            ),
        )
    ),
)





# Define server logic required to draw a histogram
#server - code and aesthetics of each barplot
server <- function(input, output) {
    
    #School Stat Plot Inputs
    output$barplots <- renderPlotly({
        if(input$select == "Enrollment") {
            p <-  ggplot(Data_School_Info, aes(factor(SCHOOL_NAME), ENROLLMENT)) + 
                geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                coord_flip() +
                theme_minimal() +
                geom_text(aes(label = ENROLLMENT), vjust = 0)+
                labs(title = "Enrollment per School" , x = "School", y = "Enrollment")
            ggplotly(p)
        }
        
        else if(input$select == "School and Zone Racial Breakdown"){
            p <- ggplot(race, aes(factor(place), number, fill = sorz)) + 
                geom_bar(stat="identity", position = "dodge") + 
                coord_flip() +
                theme_minimal() +
                labs(title = "Racial Composition of Schools vs. School Zones" , x = "School", y = "Percentage of Students of Color", fill=" ")
            ggplotly(p)
        } else if(input$select == "Racial Differential"){
            p <- ggplot(race_diff, aes(factor(place), number)) + 
                geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                coord_flip() +
                theme_minimal() +
                geom_text(aes(label = number), vjust = 0)+
                labs(title = "Difference in % of Students of Color between Schools and Zones" , x = "School", y = "Difference in Students of Color (%)")
            ggplotly(p)
        } else if(input$select == "POC per School") {
            p <- ggplot(poc_per_school, aes(factor(place), number)) + 
                geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                coord_flip() +
                theme_minimal() +
                geom_text(aes(label = number), vjust = 0)+
                labs(title = "Percentage of Students of Color" , x = "School", y = "Students of Color (%)")
            ggplotly(p)
        } else if(input$select == "Funding per Pupil") {
            p <- ggplot(funding, aes(factor(place), number)) + 
                geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                coord_flip() +
                theme_minimal() +
                geom_text(aes(label = number), vjust = 0)+
                labs(title = "Funding per Pupil" , x = "School", y = "Funding per Pupil ($)")
            ggplotly(p)
            
            
        } else if(input$select == "Racial Demographics") {
            p3 <- ggplot(all_race, aes(factor(school), number, fill = race)) + 
                geom_bar(stat="identity", position = "dodge") + 
                coord_flip() +
                theme_minimal() +
                labs(title = "Racial Composition of Schools" , x = "School", y = "Percentage of Students", fill="Race")
            ggplotly(p3)
        } 
        
        else if(input$select == "Race per School") {
            
            p <- ggplot(all_race, aes(factor(race), number, fill=race)) + 
                geom_bar(stat="identity", position = "dodge") + 
                coord_flip() +
                theme_minimal() +
                facet_wrap(~school)+
                labs(title = "Racial Composition", subtitle="Faceted by School" , x = "Race", y = "Percentage of Students (%)")
            ggplotly(p)
        }
        
        else if(input$select == "Household Income") {
            p <- ggplot(Data_School_Info, aes(factor(SCHOOL_NAME), MED_HOUSEHOLD_INC)) + 
                geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                coord_flip() +
                theme_minimal() +
                geom_text(aes(label = MED_HOUSEHOLD_INC), vjust = 0)+
                labs(title = "Median Household Income", y = "Median Household Income ($)", x = "School")
            ggplotly(p)
        }
        
        
        else if(input$select == "Homesale Price") {
            p <- ggplot(Data_School_Info, aes(factor(SCHOOL_NAME), MED_HOMESALE_PRICE)) + 
                geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                coord_flip() +
                theme_minimal() +
                geom_text(aes(label = MED_HOMESALE_PRICE), vjust = 0)+
                labs(title = "Median Homesale Price", y = "Median Homesale Price ($)", x = "School")
            ggplotly(p)
        }
        
        
        else if(input$select == "Bachelor Degree Rate") {
            p <- ggplot(Data_School_Info, aes(factor(SCHOOL_NAME), BACHELOR_DEG_RATE)) + 
                geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                coord_flip() +
                theme_minimal() +
                geom_text(aes(label = BACHELOR_DEG_RATE), vjust = 0)+
                labs(title = "Bachelor Degree Rate per School Zone", y = "Bachelor Degree Rate", x = "School")
            ggplotly(p)
        }
        
        
        else if(input$select == "Sidewalk Coverage") {
            p <- ggplot(Data_School_Info, aes(factor(SCHOOL_NAME), SIDEWALK_COVG)) + 
                geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                coord_flip() +
                theme_minimal() +
                geom_text(aes(label = SIDEWALK_COVG), vjust = 0)+
                labs(title = "Sidewalk Coverage per School Zone", y = "Sidewalk Coverage (%)", x = "School")
            ggplotly(p)
        }
        
        else if(input$select == "Diversity per District") {
            p <- ggplot(Data_School_Info, aes(factor(SCHOOL_NAME), DIVERSITY_DISTRICT)) + 
                geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                coord_flip() +
                theme_minimal() +
                geom_text(aes(label = DIVERSITY_DISTRICT), vjust = 0)+
                labs(title = "Diversity per School Zone", y = "Diversity (%)", x = "School")
            ggplotly(p)
        }
    })
    
    output$resources <- renderText({
        if(input$select == "Enrollment") {
            paste("Here are some resouces for school enrollment numbers.")
        }
        else if (input$select == "School and Zone Racial Breakdown") {
            paste("Here are some resouces for differences in school zone and school racial demographics.")
        }
        else if (input$select == "Racial Differential") {
            paste("Here are some resouces for differences in school zone and school racial demographics.")
        }
        else if (input$select == "POC per School"){
            paste("Here are some resouces for the number of students of color in a school.")
            
        }
        else if (input$select == "Funding per Pupil"){
            paste("Here are some resouces for school funding.")
        }
        else if (input$select == "Racial Demographics"){
            paste("Here are some resouces for racial demographics.")
        }
        else if (input$select == "Race per School"){
            paste("Here are some resouces on racial demographics.")
        }
        else if (input$select == "Household Income"){
            paste("Here are some resouces on household income rates.")
        }
        else if (input$select == "Homesale Price"){
            paste("Here are some resouces on homesale prices..")
        }
        else if (input$select == "Bachelor Degree Rate"){
            paste("Here are some resouces about bachelor degree rates.")
        }
        
        else if (input$select == "Sidewalk Coverage"){
            paste("Here are some resouces about sidewalk coverage.")
        }
        
        else if (input$select == "Diversity per District"){
            paste("Here are some resouces about diversity in school districts.")
        }
        
    })
    
}


shinyApp(ui = ui, server = server)




