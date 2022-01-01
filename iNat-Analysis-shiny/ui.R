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
        label = h4("Base Map"),
        choices = c("Doctor", "Master", "Undergraduate", "Highschool", "Midschool", "Income", "KidRatio", "AdultRatio", "OldRatio", "TotalPopulation"),
        selected = "Doctor"),
      # Add a select input
      selectInput(
        inputId = "SpatialScale",
        label = h4("Spatial Scale"),
        choices = c("NationWide", "Region", "County"),
        selected = "County"),
      # Add a select input
      selectInput(
        inputId = "LineTime",
        label = h4("Line Plot"),
        choices = c("Overall", "Year", "Month", "Day", "Hour"),
        selected = "Year"),
      # Add a select input
      selectInput(
        inputId = "BarTime",
        label = h4("Bar Plot"),
        choices = c("Overall", "Year", "Month", "Weekday", "Hour"),
        selected = "Year")
    ),
    
    mainPanel(
      plotlyOutput("line", height = 750) %>% withSpinner(type = 6),
      plotlyOutput("bar", height = 750) %>% withSpinner(type = 6),
      imageOutput("choropleth") %>% withSpinner(type = 6))
  )
))