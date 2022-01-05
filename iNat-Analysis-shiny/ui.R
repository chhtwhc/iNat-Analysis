# Setting environment
library(shiny)
library(shinycssloaders)
library(dplyr)
library(shinyalert)
library(plotly)
library(shinythemes)

showtext::showtext_auto(enable = TRUE)

load("AllData.Rdata")

genusList = dList[["iNaturalist"]] %>% 
  select(Genus) %>%
  filter(!is.na(Genus)) %>% 
  unique() %>% 
  arrange(Genus)

options(spinner.color = "#0275D8", spinner.color.background = "#ffffff", spinner.size = 1)  # Settings of spinner

# Build Shiny UI
shinyUI(fluidPage(
  theme = shinytheme("superhero"),
  
  titlePanel("iNat Count Count"),
  
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
        choices = c("Year", "Month", "Day", "Hour"),
        selected = "Year"),
      # Add a select input for bar plot
      selectInput(
        inputId = "BarTime",
        label = h4("Bar Plot"),
        choices = c("Overall", "Year", "Month", "Weekday", "Hour"),
        selected = "Year"),
      # Add a select input for top 10
      selectInput(
        inputId = "Top10Time",
        label = h4("Top 10 genus"),
        choices = c("Overall", "Year", "Month", "Weekday", "Hour"),
        selected = "Year"),
      # Add a select input for density map
      selectInput(
        inputId = "DensityGenus",
        label = h4("Genus input"),
        choices = genusList,
        selected = "Ficus"),
      # Add a select input for choropleth map
      selectInput(
        inputId = "BaseMap",
        label = h4("Base Map"),
        choices = c("Doctor", "Master", "Undergraduate", "Highschool", "Midschool", "Income", "KidRatio", "AdultRatio", "OldRatio", "TotalPopulation"),
        selected = "Doctor")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Line plot",
                 plotlyOutput("line", height = 750) %>% withSpinner(type = 6)),
        tabPanel(title = "Bar plot",
                 plotlyOutput("bar", height = 750) %>% withSpinner(type = 6)),
        tabPanel(title = "Top 10 genus",
                 plotOutput("Top10Genus", height = 5000) %>% withSpinner(type = 6)),
        tabPanel(title = "Genus Density map",
                 plotOutput("Density", height = 750) %>% withSpinner(type = 6)),
        tabPanel(title = "Animaion",
                 imageOutput("choropleth", height = 750) %>% withSpinner(type = 6),
                 plotlyOutput("hull", height = 500) %>% withSpinner(type = 6)))
      )
    
  )
))
