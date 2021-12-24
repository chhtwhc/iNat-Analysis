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
      # Add a select input
      selectInput(
        inputId = "BaseMap",
        label = h4("底圖"),
        choices = c("年收入", "兒童比例", "青壯年比例", "老年比例", "物種記錄總筆數"),
        selected = "物種記錄總筆數"),
      # Add a select input
      selectInput(
        inputId = "SpatialScale",
        label = h4("空間尺度"),
        choices = c("鄉鎮市區", "縣市", "台灣各區", "全國"),
        selected = "物種記錄總筆數"),
      # Add a select input
      selectInput(
        inputId = "Line",
        label = h4("折線圖"),
        choices = c("年變化", "月變化", "日變化"),
        selected = "物種記錄總筆數"),
      # Add a select input
      selectInput(
        inputId = "Bar",
        label = h4("長條圖"),
        choices = c("年間差異", "月間差異", "週間差異", "日間差異", "時間差異"),
        selected = "物種記錄總筆數"),
      # Add a date range input
      dateRangeInput(
        inputId = "DateRange",
        label = h3("時間範圍"))
    ),
    
    mainPanel(
      plotOutput("plot", height = 750) %>% withSpinner(type = 6))
  )
))