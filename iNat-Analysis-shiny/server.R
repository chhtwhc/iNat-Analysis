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
library(sf)

# setwd("D:/CHU 2.0/Forest/110-1 Space Time data Viz/Term Project/iNat-Analysis/iNat-Analysis-shiny")  # 上傳到 Shiny 時記得註解掉

# Data processing
iNat = read_xlsx("../Data/Data.xlsx", sheet = "iNaturalist")
edu = read_xlsx("../Data/Data.xlsx", sheet = "Education")
income = read_xlsx("../Data/Data.xlsx", sheet = "Income")
pop = read_xlsx("../Data/Data.xlsx", sheet = "Population")
taiwan = st_read("../Data/TWPop_mainLand_4326.shp", options = "ENCODING=BIG5") %>% select(geometry)


# Build Shiny server
shinyServer(function(input, output) {
  
  output$plot = renderPlot({
    plot(x = 1:12, y = 1:12)
  })
  
})