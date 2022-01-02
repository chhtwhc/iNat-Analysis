# Setting environment
library(shiny)
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
  
  titlePanel("Use data from iNaturalist to know feature of citizen scientists in Taiwan"),
  
  sidebarLayout(
    # Add sidebar
    sidebarPanel(
      # Add a select input for spatial scale
      selectInput(
        inputId = "SpatialScale",
        label = h4("Spatial Scale"),
        choices = c("NationWide", "Region", "County"),
        selected = "County"),
      # Add a select input for line plot
      selectInput(
        inputId = "LineTime",
        label = h4("Line Plot"),
        choices = c("Overall", "Year", "Month", "Day", "Hour"),
        selected = "Year"),
      # Add a select input for bar plot
      selectInput(
        inputId = "BarTime",
        label = h4("Bar Plot"),
        choices = c("Overall", "Year", "Month", "Weekday", "Hour"),
        selected = "Year"),
      # Add a select input for choropleth map
      selectInput(
        inputId = "BaseMap",
        label = h4("Base Map"),
        choices = c("Doctor", "Master", "Undergraduate", "Highschool", "Midschool", "Income", "KidRatio", "AdultRatio", "OldRatio", "TotalPopulation"),
        selected = "Doctor")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Basic statistics"),
        tabPanel(title = "Line plot",
                 plotlyOutput("line", height = 750) %>% withSpinner(type = 6)),
        tabPanel(title = "Bar plot",
                 plotlyOutput("bar", height = 750) %>% withSpinner(type = 6)),
        tabPanel(title = "Animaion",
                 plotlyOutput("hull", height = 500) %>% withSpinner(type = 6),
                 imageOutput("choropleth") %>% withSpinner(type = 6)))
      )
    
  )
))