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
library(plotly)

## Data

temp_data <- read_csv('AIIT_DailyStats.csv') %>%
  mutate(Date = mdy(Date))

## Functions and variables

stations <- temp_data$Station_Name %>%
  unique()

years <- temp_data$Year %>%
  unique()

max_consequtive_days <- function(data, station, year){
  threshold_exceeded_consequtive <- data %>%
    filter(Station_Name == station & Year == year) %>%
    mutate(threshold = if_else(sda_Max >= 18, 1, 0, missing = 0))
  
  rle_object <- rle(threshold_exceeded_consequtive$threshold)
  return(with(rle_object, max(lengths[values==1])))
}

threshold_counter <- function(data, station, year){
  counter <- data %>%
    filter(Station_Name == station & sda_Max > 18 & Year == year) %>%
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
        selectInput("select_site", "Site:", stations)
      ),
      # menuItem(
      #   title = "Year",
      #   selectInput("radio_year", "Year:", years)
      # )
      menuItem(
        title = "Year",
        radioButtons("radio_year", "Year:",
                     c("2017" = "2017", "2018" = "2018"))
      )
    )),
  dashboardBody(
    tags$head(tags$style(HTML(".small-box {height: 125px}"))),
    fluidRow(column(width = 12,
                    valueBoxOutput("days_exceeded", width = 6),
                    valueBoxOutput("max_consecutive", width = 6)
    ),
    
    fluidRow(box(width=12, plotlyOutput("sdaMax_line"))))))


server <- function(input, output) {
  # set.seed(122)
  # histdata <- rnorm(500)
  
  output$sdaMax_line <- renderPlotly({
    temp_lp <- temp_data %>%
      filter(Station_Name == input$select_site, Year == input$radio_year) %>%
      ggplot(aes(Date, sda_Max)) +
      geom_line() +
      geom_hline(yintercept = 18, color = 'red') +
      theme_tufte(base_size = 16) +
      labs(title = 'Seven Day Average Maximum Temperature',
           subtitle = input$select_site,
           y = 'SDA Max Temperature (°C)')
    ggplotly(temp_lp)
    
  })
  
  output$days_exceeded <- renderValueBox(
    valueBox(threshold_counter(temp_data, input$select_site, input$radio_year),
             subtitle = "Total Days the SDA Max Temp Exceeded 18 C in 2017")
  )
  
  output$max_consecutive <- renderValueBox(
    valueBox(max_consequtive_days(temp_data, input$select_site, input$radio_year),
             subtitle = "Maximum Consecutive Days the SDA Max Temp Exceeded 18 C in 2017")
  )
}

shinyApp(ui, server)
