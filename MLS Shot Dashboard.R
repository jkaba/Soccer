# MLS Shot Dashboard
# Author: Jameel Kaba

# Load libraries
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(readxl)
library(DT)

# sample input file
MLS <- read_xlsx('MLS_Shot_Data.xlsx')
head(MLS)

# Defining header & sidebar, dashboard header carries title
header <- dashboardHeader(title = "MLS Shot Data")

# Sidebar content 
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashobard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Data Source", icon = icon("send", lib = 'glyphicon'),
             href = "https://www.whoscored.com/Regions/233/Tournaments/85/USA-Major-League-Soccer"),
    menuItem("Data", tabName = "alldata", icon = icon("database"))
  )
)

# Defining body
frow1 <- fluidRow(valueBoxOutput("value1"), valueBoxOutput("value2"), valueBoxOutput("value3"))
frow2 <- fluidRow(box(
  title = "Goals Per Team", status = "primary", solidHeader = TRUE, 
  collapsible = TRUE, plotOutput("placeholder1", height = "300px")
),
box(
  title = "Shots in the Box", status = "primary", solidHeader = TRUE, 
  collapsible = TRUE, plotOutput("placeholder2", height = "300px"))
)

# Create a new row in the UI for Inputs
frow3 <- fluidRow(column(4, selectInput("tm", "Team:", c("All", unique(as.character(MLS$Team))))))
frow4 <- fluidRow(column(12, DT::dataTableOutput("placeholder3"), style = "scroll;overflow-x: scroll;"))

# Merge the fluid rows to create the body
body <- dashboardBody(frow1, frow2, frow3, frow4)

# Complete UI with dashboard page
ui <- dashboardPage(title = 'MLS Dash', header, sidebar, body, skin = 'red')

# Setting up subset of data for ouput 1
ph1 <- MLS %>% group_by(Team) %>% summarise(Goals = sum(Goals))

# Setting up subset of data for output 2
ph2 <- aggregate(MLS$ShotSixYardBox, by = list(Team = MLS$Team), FUN = sum)
colnames(ph2)[colnames(ph2) == "x"] <- "ShotSixYardBox"

# Creating the server functions for the dashboard
server <- function(input, output) {
  
  # Data manipultation to derive KPI box values
  total.goals <- sum(MLS$Goals)
  save.percnt <- MLS %>% group_by(Team) %>% summarise(value = mean(SavePct)) %>% filter(value == max(value))
  shot.target <- MLS %>% group_by(Team) %>% summarise(value = sum(ShotOnTgt)) %>% filter(value == max(value))
  
  # Creating content for value box
  output$value1 <- renderValueBox({
    valueBox(formatC(save.percnt$value, format = "d", big.mark = ','),
             paste('Highest Save Pct:', save.percnt$Team),
             icon = icon("triangle-top", lib = 'glyphicon'),
             color = "purple")
  })
  
  output$value2 <- renderValueBox({
    valueBox(
      formatC(total.goals, format="d", big.mark=','),
      'Total Goals',
      icon = icon("flash",lib='glyphicon'),
      color = "green")  
  })
  
  output$value3 <- renderValueBox({
    valueBox(
      formatC(shot.target$value, format="d", big.mark=','),
      paste('Most Shots on Goal:', shot.target$Team),
      icon = icon("menu-hamburger", lib='glyphicon'),
      color = "yellow")   
  })
  
  # Creating content for plot output
  output$placeholder1 <- renderPlot({
    ggplot(data = ph1, aes(x = Team, y = Goals)) + geom_bar(position = "dodge", stat = "identity", fill = "green4") +
      ylab("Goals") + xlab("Team") + theme(legend.position = "bottom", plot.title = element_text(size = 15, face = "bold")) +
      ggtitle("") + labs(fill = "Goals") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$placeholder2 <- renderPlot({
    ggplot(data = ph2, aes(x = Team, y = ShotSixYardBox)) + geom_bar(position = "dodge", stat = "identity", fill = "darkgoldenrod2") +
      ylab("Shots in the Box") + xlab("Team") + theme(legend.position = "bottom", plot.title = element_text(size = 15, face = "bold")) +
      ggtitle("") + labs(fill = "ShotSixYardBox") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$placeholder3 <- DT::renderDataTable(DT::datatable({
    data <- MLS
    if(input$tm != "All"){
      data <- data[data$Team == input$tm,]
    }
    data
  }
  #options = list(scrollX = TRUE)
  ))
}

# Run the shiny app
shinyApp(ui, server)
