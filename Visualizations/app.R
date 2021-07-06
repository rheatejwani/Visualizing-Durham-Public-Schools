#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(readxl)

#ui
ui <- shinyUI(
    pageWithSidebar(
        headerPanel("Sample Dashboard"),
        sidebarPanel(
            selectInput("select", "Select a Measurement", 
                        choices = list("Enrollment","School Zone vs. School Racial Demographics",
                                       "Funding per Pupil")
            )),
        
        #Output ggPlot to show bar graphs
        mainPanel(
            plotlyOutput("Enrollment",width="auto",height = "auto"),
            plotlyOutput("School Zone vs. School Racial Demographics", width="auto",height = "auto"),
            plotlyOutput("Funding per Pupil", width="auto",height = "auto")
        )
    ))


#server - code and aesthetics of each barplot
server <- function(input, output){
    
    #Enrollment
    output$Enrollment <- renderPlotly({
        if(input$select == "Enrollment"){
            data_per_student <- read_excel("~/Documents/ Other/Data+/DPS Spreadsheets/data per student.xlsx")
            p1 <- ggplot(data_per_student, aes(x = school)) +
                geom_bar(fill="powder blue", color="white")+
                coord_flip()+
                theme_minimal() +
                labs(title = "Enrollment per School" , x = "School", y = "Enrollment")
            ggplotly(p1)
        }
    })
    
    #Racial Composition
    output$School_Zone_vs._School_Racial_Demographics <- renderPlotly({
        if(input$select == "School Zone vs. School Racial Demographics") {
            race_diff <- read_excel("~/Documents/ Other/Data+/DPS Spreadsheets/race diff.xlsx")
            p2 <- ggplot(race_diff, aes(factor(place), number)) + 
                geom_bar(stat="identity", position = "dodge") + 
                coord_flip() +
                theme_minimal() +
                labs(title = "Difference in % of Students of Color between Schools and Zones" , x = "School", y = "Difference in Students of Color (%)")
            ggplotly(p2)
        }
    })
    
    #Funding per Pupil
    output$Funding_per_Pupil <- renderPlotly({
        if(input$select == "Funding per Pupil") {
            funding <- read_excel("~/Documents/ Other/Data+/DPS Spreadsheets/funding.xlsx")
            p3 <- ggplot(funding, aes(factor(place), number)) + 
                geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                coord_flip() +
                theme_minimal() +
                geom_text(aes(label = number), vjust = 0)+
                labs(title = "Funding per Pupil" , x = "School", y = "Funding per Pupil ($)")
            ggplotly(p3)
        }
    })
    
}
#Run App
shinyApp(ui = ui, server = server)



