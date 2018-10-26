# Author: Ryan Flaherty
# Contact: ryan1.flaherty@gmail.com -- 541-513-093
# Date: 2018-10-24
# Description: Shiny dashboard to visualize water quality data provided by the LWC.

# Install packages, if needed.
# install.packages(c("shiny", "shinydashboard", "tidyverse", "lubridate"))


## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(ggplot2)

temp_data <- read_csv('2017_DailyDB_PublicSites.csv') %>%
  mutate(DateTimeMax = mdy_hm(DateTimeMax), DateTimeMin = mdy_hm(DateTimeMin), Date = mdy(Date))
stations <- temp_data$Station %>%
  unique()

## app.R ##
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Luckiamute Watershed Council Temperature Dashboard", 
                  titleWidth = 550),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("sdaMax_line", height = 250)),
      
      box(
        title = "Controls",
        selectInput("select", "Site name:", stations)
      )
    )
  )
)

server <- function(input, output) {
  # set.seed(122)
  # histdata <- rnorm(500)
  
  output$sdaMax_line <- renderPlot({
    temp_data %>%
      filter(Station == input$select) %>%
      ggplot(aes(Date, `sda Max`)) +
      geom_line()
  })
}

shinyApp(ui, server)



temp_data %>%
  filter(Station == 'Pedee') %>%
  ggplot(aes(DateTimeMax, `sda Max`)) +
  geom_line() +
  theme_bw(base_size = 16) +
  labs(y = "7-day average maximum temp. (C)")

threshold_exceeded <- temp_data %>%
  filter(Station == "Pedee" & `sda Max` > 18) %>%
  summarize(n = n())

threshold_exceeded_consequtive <- temp_data %>%
  filter(Station == "Pedee") %>%
  mutate(threshold = if_else(`sda Max` >= 18, 1, 0)) %>%
  with(rle(threshold), max(lengths[values==1])) %>%
  
test <- rle(threshold_exceeded_consequtive$threshold)
  
# test<-rle(as.character(threshold_exceeded_consequtive$threshold)
# test2 <- data.frame(unclass(test)) %>%
#   filter(values == 1)

set.seed(24)
No <- rle(sample(c(1, 0), 25, replace=TRUE))
with(No, max(lengths[values==1]))
