## Author: Ryan Flaherty
## Contact: ryan1.flaherty@gmail.com -- 541-513-093
## Date: 2018-10-24
## Description: Shiny dashboard to visualize water quality data provided by the LWC.

## Install packages, if needed.
# install.packages(c("shiny", "shinydashboard", "tidyverse", "lubridate"))


## Libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggthemes)

## Data

temp_data <- read_csv('2017_DailyDB_PublicSites.csv') %>%
  mutate(DateTimeMax = mdy_hm(DateTimeMax), DateTimeMin = mdy_hm(DateTimeMin), Date = mdy(Date))

## Functions and variables

stations <- temp_data$Station %>%
  unique()

max_consequtive_days <- function(data, station){
  threshold_exceeded_consequtive <- data %>%
    filter(Station == station) %>%
    mutate(threshold = if_else(`sda Max` >= 18, 1, 0, missing = 0))
  
  rle_object <- rle(threshold_exceeded_consequtive$threshold)
  return(with(rle_object, max(lengths[values==1])))
}

threshold_counter <- function(data, station){
  counter <- data %>%
    filter(Station == station & `sda Max` > 18) %>%
    summarize(n = n())
  return(counter$n)
}


## App code

ui <- dashboardPage(
  dashboardHeader(title = "Luckiamute Watershed Council Temperature Dashboard", 
                  titleWidth = 550),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        title = "Sites",
        selectInput("select", "Site:", stations)
      )
    )),
  dashboardBody(
    tags$head(tags$style(HTML(".small-box {height: 125px}"))),
    fluidRow(column(width = 12,
      valueBoxOutput("days_exceeded", width = 6),
      valueBoxOutput("max_consecutive", width = 6)
      ),
    
    fluidRow(box(width=12, plotOutput("sdaMax_line"))))))


server <- function(input, output) {
  # set.seed(122)
  # histdata <- rnorm(500)
  
  output$sdaMax_line <- renderPlot({
    temp_data %>%
      filter(Station == input$select) %>%
      ggplot(aes(Date, `sda Max`)) +
      geom_line() +
      geom_hline(yintercept = 18, color = 'red') +
      theme_tufte(base_size = 16) +
      labs(title = 'Seven Day Average Maximum Temperature',
           subtitle = input$select)
  })
  
  output$days_exceeded <- renderValueBox(
    valueBox(threshold_counter(temp_data, input$select),
             subtitle = "Total Days the SDA Max Temp Exceeded 18 C")
  )
  
  output$max_consecutive <- renderValueBox(
    valueBox(consequtive_days(temp_data, input$select),
             subtitle = "Maximum Consecutive Days the SDA Max Temp Exceeded 18 C")
  )
}

shinyApp(ui, server)

