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
  useShinyalert(),
  
  titlePanel("嘗試用 iNaturalist 推測台灣公民科學家的特色"),
  
  sidebarLayout(
    # Add sidebar
    sidebarPanel(
      # Add a select input -- 面量圖底圖
      selectInput(
        inputId = "BaseMap",
        label = h4("底圖"),
        choices = c("平均收入", "平均年齡", "物種記錄總筆數"),
        selected = "物種記錄總筆數"),
      # Add a select input -- 空間尺度
      selectInput(
        inputId = "SpaceScale",
        label = h4("空間尺度"),
        choices = c("鄉鎮市區", "縣市", "台灣各區"),
        selected = "鄉鎮市區"),
      # Add a select input -- 折線圖
      selectInput(
        inputId = "LinePlot",
        label = h4("折線圖"),
        choices = c("紀錄筆數～年", "紀錄筆數～月"),
        selected = "紀錄筆數～月"),
      # Add a select input -- 長條圖
      selectInput(
        inputId = "BarPlot",
        label = h4("長條圖"),
        choices = c("紀錄總筆數～年度", "紀錄總筆數～月份", "紀錄總筆數～星期幾"),
        selected = "紀錄總筆數～年度")
    ),
    
    mainPanel(
      leafletOutput("map", height = 750) %>% withSpinner(type = 6))
  )
))