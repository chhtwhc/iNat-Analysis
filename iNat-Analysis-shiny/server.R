library(shiny)
library(leaflet)
library(mapview)
library(shinycssloaders)
library(dplyr)
library(shinyalert)
library(showtext)
library(readxl)
library(plotly)

shinyServer(function(input, output) {
  
  output$plot = renderPlot({
    plot(x = 1:12, y = 1:12)
  })
  
})