# Setting environment
library(shiny)
library(leaflet)
library(mapview)
library(shinycssloaders)
library(dplyr)
library(shinyalert)
library(showtext)
library(readxl)
library(plotly)

showtext_auto(enable = TRUE)

# Settings of spinner
options(spinner.color = "#0275D8", spinner.color.background = "#ffffff", spinner.size = 1)

# Build Shiny UI
shinyUI(fluidPage(
  
  titlePanel("用 iNaturalist 推測台灣公民科學家的特色"),
  
  sidebarLayout(
    # Add sidebar
    sidebarPanel(
      # Add a select input -- 面量圖底圖
      selectInput(
        inputId = "BaseMap",
        label = h4("底圖"),
        choices = c("平均收入", "平均年齡", "物種記錄總筆數"),
        selected = "物種記錄總筆數"),
    ),
    
    mainPanel(
      plotOutput("plot", height = 750) %>% withSpinner(type = 6))
  )
))