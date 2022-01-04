# Setting environment
library(shiny)
library(plyr)
library(tidyverse)
library(plotly)
library(sf)
library(xts)
library(TSstudio)
library(magick)
library(spdplyr)
library(gganimate)
library(transformr)
library(directlabels)

showtext::showtext_auto(enable = TRUE)
options(scipen = 999)

# setwd("D:/CHU 2.0/Forest/110-1 Space Time data Viz/Term Project/iNat-Analysis/iNat-Analysis-shiny")  # 上傳到 Shiny 時記得註解掉

# Data processing
if (TRUE){
  # iNaturalist data
  if (TRUE){
    iNat = readxl::read_xlsx("Data/Data.xlsx", sheet = "iNaturalist") %>% 
      mutate(Year = as.character(Year))
  }
  # Bar plot data
  if (TRUE){
    # Nation + Overall
    obsNationOverall = iNat %>%
      dplyr::summarise(TotalObs = n()) %>% 
      mutate(Nation = "Taiwan", Time = 2021) %>% 
      relocate(Nation, Time)
    obsNationOverall[is.na(obsNationOverall)] = 0
    # Nation + Year
    obsNationYear = iNat %>%
      group_by(Year) %>%
      dplyr::summarise(TotalObs = n()) %>% 
      mutate(Nation = "Taiwan",) %>% 
      relocate(Nation)
    obsNationYear[is.na(obsNationYear)] = 0
    # Nation + Month
    obsNationMonth = iNat %>%
      group_by(Month) %>%
      dplyr::summarise(TotalObs = n()) %>% 
      mutate(Nation = "Taiwan",) %>% 
      relocate(Nation)
    obsNationMonth[is.na(obsNationMonth)] = 0
    # Nation + Weekday
    obsNationWeekday = iNat %>%
      group_by(Weekday) %>%
      dplyr::summarise(TotalObs = n()) %>% 
      mutate(Nation = "Taiwan",) %>% 
      relocate(Nation)
    obsNationWeekday[is.na(obsNationWeekday)] = 0
    # Nation + Hour
    obsNationHour = iNat %>%
      group_by(Hour) %>%
      dplyr::summarise(TotalObs = n()) %>% 
      mutate(Nation = "Taiwan",) %>% 
      relocate(Nation)
    obsNationHour[is.na(obsNationHour)] = 0
    # Region + Overall
    obsRegionOverall = iNat %>%
      group_by(Region) %>%
      dplyr::summarise(TotalObs = n()) %>% 
      mutate(Time = 2021) %>% 
      relocate(Region, Time)
    obsRegionOverall[is.na(obsRegionOverall)] = 0
    # Region + Year
    obsRegionYear = iNat %>%
      group_by(Region, Year) %>%
      dplyr::summarise(TotalObs = n())
    obsRegionYear[is.na(obsRegionYear)] = 0
    # Region + Month
    obsRegionMonth = iNat %>%
      group_by(Region, Month) %>%
      dplyr::summarise(TotalObs = n())
    obsRegionMonth[is.na(obsRegionMonth)] = 0
    # Region + Weekday
    obsRegionWeekday = iNat %>%
      group_by(Region, Weekday) %>%
      dplyr::summarise(TotalObs = n())
    obsRegionWeekday[is.na(obsRegionWeekday)] = 0
    # Region + Hour
    obsRegionHour = iNat %>%
      group_by(Region, Hour) %>%
      dplyr::summarise(TotalObs = n())
    obsRegionHour[is.na(obsRegionHour)] = 0
    # County + Overall
    obsCountyOverall = iNat %>%
      group_by(County) %>%
      dplyr::summarise(TotalObs = n()) %>% 
      mutate(Time = 2021) %>% 
      relocate(County, Time)
    obsCountyOverall[is.na(obsCountyOverall)] = 0
    # County + Year
    obsCountyYear = iNat %>%
      group_by(County, Year) %>%
      dplyr::summarise(TotalObs = n())
    obsCountyYear[is.na(obsCountyYear)] = 0
    # County + Month
    obsCountyMonth = iNat %>%
      group_by(County, Month) %>%
      dplyr::summarise(TotalObs = n())
    obsCountyMonth[is.na(obsCountyMonth)] = 0
    # County + Weekday
    obsCountyWeekday = iNat %>%
      group_by(County, Weekday) %>%
      dplyr::summarise(TotalObs = n())
    obsCountyWeekday[is.na(obsCountyWeekday)] = 0
    # County + Hour
    obsCountyHour = iNat %>%
      group_by(County, Hour) %>%
      dplyr::summarise(TotalObs = n())
    obsCountyHour[is.na(obsCountyHour)] = 0
    # Town + Overall
    obsTownOverall = iNat %>%
      group_by(Town) %>%
      dplyr::summarise(TotalObs = n()) %>% 
      mutate(Time = 2021) %>% 
      relocate(Town, Time)
    obsTownOverall[is.na(obsTownOverall)] = 0
    # Town + Year
    obsTownYear = iNat %>%
      group_by(Town, Year) %>%
      dplyr::summarise(TotalObs = n())
    obsTownYear[is.na(obsTownYear)] = 0
    # Town + Month
    obsTownMonth = iNat %>%
      group_by(Town, Month) %>%
      dplyr::summarise(TotalObs = n())
    obsTownMonth[is.na(obsTownMonth)] = 0
    # Town + Weekday
    obsTownWeekday = iNat %>%
      group_by(Town, Weekday) %>%
      dplyr::summarise(TotalObs = n())
    obsTownWeekday[is.na(obsTownWeekday)] = 0
    # Town + Hour
    obsTownHour = iNat %>%
      group_by(Town, Hour) %>%
      dplyr::summarise(TotalObs = n())
    obsTownHour[is.na(obsTownHour)] = 0
    
    # Data list
    obsSTList = list(
      NationWide = list(
        Overall = obsNationOverall,
        Year = obsNationYear,
        Month = obsNationMonth,
        Weekday = obsNationWeekday,
        Hour = obsNationHour),
      Region = list(
        Overall = obsRegionOverall,
        Year = obsRegionYear,
        Month = obsRegionMonth,
        Weekday = obsRegionWeekday,
        Hour = obsRegionHour),
      County = list(
        Overall = obsCountyOverall,
        Year = obsCountyYear,
        Month = obsCountyMonth,
        Weekday = obsCountyWeekday,
        Hour = obsCountyHour),
      Town = list(
        Overall = obsTownOverall,
        Year = obsTownYear,
        Month = obsTownMonth,
        Weekday = obsTownWeekday,
        Hour = obsTownHour)
    )
  }
  # Line plot data
  if (TRUE){
    # Nation + Overall
    obsNationOverallW = obsNationOverall %>%
      mutate(Index = paste(Time, "-01-01 01:01:01", sep = "")) %>%
      mutate(Index = strptime(Index, format = "%Y-%m-%d %H:%M:%S")) %>%
      select(Nation, TotalObs, Index) %>%
      pivot_wider(names_from = Nation, values_from = TotalObs)
    mtsNationOverall = ts(obsNationOverallW[,-1], start = 2021, frequency = 1)
    # Nation + Year
    obsNationYearW = obsNationYear %>%
      mutate(Index = paste(Year, "-01-01 01:01:01", sep = "")) %>%
      mutate(Index = strptime(Index, format = "%Y-%m-%d %H:%M:%S")) %>%
      select(Nation, TotalObs, Index) %>%
      pivot_wider(names_from = Nation, values_from = TotalObs)
    mtsNationYear = ts(obsNationYearW[,-1], start = 2014, frequency = 1)
    # Nation + Month
    obsNationMonthW = iNat %>%
      group_by(Year, Month) %>%
      dplyr::summarise(TotalObs = n()) %>% 
      mutate(Nation = "Taiwan",) %>% 
      relocate(Nation) %>%
      mutate(Index = paste(Year, "-", Month, "-01 01:01:01", sep = "")) %>%
      mutate(Index = strptime(Index, format = "%Y-%m-%d %H:%M:%S")) %>%
      ungroup() %>% 
      select(Nation, TotalObs, Index) %>%
      pivot_wider(names_from = Nation, values_from = TotalObs)
    obsNationMonthW[is.na(obsNationMonthW)] = 0
    mtsNationMonth = ts(obsNationMonthW[,-1], start = c(2014,1), frequency = 12)
    # Nation + Day
    obsNationDayW = iNat %>%
      group_by(Year, Month, Day) %>%
      dplyr::summarise(TotalObs = n()) %>% 
      mutate(Nation = "Taiwan",) %>% 
      relocate(Nation) %>%
      mutate(Index = paste(Year, "-", Month, "-", Day, " 01:01:01", sep = "")) %>%
      mutate(Index = strptime(Index, format = "%Y-%m-%d %H:%M:%S")) %>%
      ungroup() %>% 
      select(Nation, TotalObs, Index) %>%
      pivot_wider(names_from = Nation, values_from = TotalObs) %>% 
      complete(Index = seq.POSIXt(min(Index), max(Index), by = "day"))
    obsNationDayW[is.na(obsNationDayW)] = 0
    mtsNationDay = ts(obsNationDayW[,-1], start = c(2014, 1), frequency = 365)
    # Nation + Hour
    obsNationHourW = iNat %>%
      group_by(Year, Month, Day, Hour) %>%
      dplyr::summarise(TotalObs = n()) %>% 
      mutate(Nation = "Taiwan",) %>% 
      relocate(Nation )%>%
      mutate(Index = paste(Year, "-", Month, "-", Day, " ", Hour, ":01:01", sep = "")) %>%
      mutate(Index = strptime(Index, format = "%Y-%m-%d %H:%M:%S")) %>%
      ungroup() %>% 
      select(Nation, TotalObs, Index) %>%
      pivot_wider(names_from = Nation, values_from = TotalObs) %>% 
      complete(Index = seq.POSIXt(min(Index), max(Index), by = "hour"))
    obsNationHourW[is.na(obsNationHourW)] = 0
    mtsNationHour = ts(obsNationHourW[,-1], start = c(2014, 1), frequency = 8760)
    # Region + Overall
    obsRegionOverallW = obsRegionOverall %>%
      mutate(Index = paste(Time, "-01-01 01:01:01", sep = "")) %>%
      mutate(Index = strptime(Index, format = "%Y-%m-%d %H:%M:%S")) %>%
      select(Region, TotalObs, Index) %>%
      pivot_wider(names_from = Region, values_from = TotalObs)
    mtsRegionOverall = ts(obsRegionOverallW[,-1], start = 2021, frequency = 1)
    # Region + Year
    obsRegionYearW = obsRegionYear %>%
      mutate(Index = paste(Year, "-01-01 01:01:01", sep = "")) %>%
      mutate(Index = strptime(Index, format = "%Y-%m-%d %H:%M:%S")) %>%
      select(Region, TotalObs, Index) %>%
      pivot_wider(names_from = Region, values_from = TotalObs)
    mtsRegionYear = ts(obsRegionYearW[,-1], start = 2014, frequency = 1)
    # Region + Month
    obsRegionMonthW = iNat %>%
      group_by(Region, Year, Month) %>%
      dplyr::summarise(TotalObs = n()) %>% 
      relocate(Region) %>%
      mutate(Index = paste(Year, "-", Month, "-01 01:01:01", sep = "")) %>%
      mutate(Index = strptime(Index, format = "%Y-%m-%d %H:%M:%S")) %>%
      ungroup() %>% 
      select(Region, TotalObs, Index) %>%
      pivot_wider(names_from = Region, values_from = TotalObs)
    obsRegionMonthW[is.na(obsRegionMonthW)] = 0
    mtsRegionMonth = ts(obsRegionMonthW[,-1], start = c(2014,1), frequency = 12)
    # Region + Day
    obsRegionDayW = iNat %>%
      group_by(Region, Year, Month, Day) %>%
      dplyr::summarise(TotalObs = n()) %>% 
      relocate(Region) %>%
      mutate(Index = paste(Year, "-", Month, "-", Day, " 01:01:01", sep = "")) %>%
      mutate(Index = strptime(Index, format = "%Y-%m-%d %H:%M:%S")) %>%
      ungroup() %>% 
      select(Region, TotalObs, Index) %>%
      pivot_wider(names_from = Region, values_from = TotalObs) %>% 
      complete(Index = seq.POSIXt(min(Index), max(Index), by = "day"))
    obsRegionDayW[is.na(obsRegionDayW)] = 0
    mtsRegionDay = ts(obsRegionDayW[,-1], start = c(2014, 1), frequency = 365)
    # Region + Hour
    obsRegionHourW = iNat %>%
      group_by(Region, Year, Month, Day, Hour) %>%
      dplyr::summarise(TotalObs = n()) %>% 
      relocate(Region)%>%
      mutate(Index = paste(Year, "-", Month, "-", Day, " ", Hour, ":01:01", sep = "")) %>%
      mutate(Index = strptime(Index, format = "%Y-%m-%d %H:%M:%S")) %>%
      ungroup() %>% 
      select(Region, TotalObs, Index) %>%
      pivot_wider(names_from = Region, values_from = TotalObs) %>% 
      complete(Index = seq.POSIXt(min(Index), max(Index), by = "hour"))
    obsRegionHourW[is.na(obsRegionHourW)] = 0
    mtsRegionHour = ts(obsRegionHourW[,-1], start = c(2014, 1), frequency = 8760)
    # County + Overall
    obsCountyOverallW = obsCountyOverall %>%
      mutate(Index = paste(Time, "-01-01 01:01:01", sep = "")) %>%
      mutate(Index = strptime(Index, format = "%Y-%m-%d %H:%M:%S")) %>%
      select(County, TotalObs, Index) %>%
      pivot_wider(names_from = County, values_from = TotalObs)
    obsCountyOverallW[is.na(obsCountyOverallW)] = 0
    mtsCountyOverall = ts(obsCountyOverallW[,-1], start = 2021, frequency = 1)
    # County + Year
    obsCountyYearW = obsCountyYear %>%
      mutate(Index = paste(Year, "-01-01 01:01:01", sep = "")) %>%
      mutate(Index = strptime(Index, format = "%Y-%m-%d %H:%M:%S")) %>%
      select(County, TotalObs, Index) %>%
      pivot_wider(names_from = County, values_from = TotalObs)
    obsCountyYearW[is.na(obsCountyYearW)] = 0
    mtsCountyYear = ts(obsCountyYearW[,-1], start = 2014, frequency = 1)
    # County + Month
    obsCountyMonthW = iNat %>%
      group_by(County, Year, Month) %>%
      dplyr::summarise(TotalObs = n()) %>% 
      relocate(County) %>%
      mutate(Index = paste(Year, "-", Month, "-01 01:01:01", sep = "")) %>%
      mutate(Index = strptime(Index, format = "%Y-%m-%d %H:%M:%S")) %>%
      ungroup() %>% 
      select(County, TotalObs, Index) %>%
      pivot_wider(names_from = County, values_from = TotalObs)
    obsCountyMonthW[is.na(obsCountyMonthW)] = 0
    mtsCountyMonth = ts(obsCountyMonthW[,-1], start = c(2014,1), frequency = 12)
    # County + Day
    obsCountyDayW = iNat %>%
      group_by(County, Year, Month, Day) %>%
      dplyr::summarise(TotalObs = n()) %>% 
      relocate(County) %>%
      mutate(Index = paste(Year, "-", Month, "-", Day, " 01:01:01", sep = "")) %>%
      mutate(Index = strptime(Index, format = "%Y-%m-%d %H:%M:%S")) %>%
      ungroup() %>% 
      select(County, TotalObs, Index) %>%
      pivot_wider(names_from = County, values_from = TotalObs) %>% 
      complete(Index = seq.POSIXt(min(Index), max(Index), by = "day"))
    obsCountyDayW[is.na(obsCountyDayW)] = 0
    mtsCountyDay = ts(obsCountyDayW[,-1], start = c(2014, 1), frequency = 365)
    # County + Hour
    obsCountyHourW = iNat %>%
      group_by(County, Year, Month, Day, Hour) %>%
      dplyr::summarise(TotalObs = n()) %>% 
      relocate(County)%>%
      mutate(Index = paste(Year, "-", Month, "-", Day, " ", Hour, ":01:01", sep = "")) %>%
      mutate(Index = strptime(Index, format = "%Y-%m-%d %H:%M:%S")) %>%
      ungroup() %>% 
      select(County, TotalObs, Index) %>%
      pivot_wider(names_from = County, values_from = TotalObs) %>% 
      complete(Index = seq.POSIXt(min(Index), max(Index), by = "hour"))
    obsCountyHourW[is.na(obsCountyHourW)] = 0
    mtsCountyHour = ts(obsCountyHourW[,-1], start = c(2014, 1), frequency = 8760)
    # Town + Overall
    obsTownOverallW = obsTownOverall %>%
      mutate(Index = paste(Time, "-01-01 01:01:01", sep = "")) %>%
      mutate(Index = strptime(Index, format = "%Y-%m-%d %H:%M:%S")) %>%
      select(Town, TotalObs, Index) %>%
      pivot_wider(names_from = Town, values_from = TotalObs)
    obsTownOverallW[is.na(obsTownOverallW)] = 0
    mtsTownOverall = ts(obsTownOverallW[,-1], start = 2021, frequency = 1)
    # Town + Year
    obsTownYearW = obsTownYear %>%
      mutate(Index = paste(Year, "-01-01 01:01:01", sep = "")) %>%
      mutate(Index = strptime(Index, format = "%Y-%m-%d %H:%M:%S")) %>%
      select(Town, TotalObs, Index) %>%
      pivot_wider(names_from = Town, values_from = TotalObs)
    obsTownYearW[is.na(obsTownYearW)] = 0
    mtsTownYear = ts(obsTownYearW[,-1], start = 2014, frequency = 1)
    # Town + Month
    obsTownMonthW = iNat %>%
      group_by(Town, Year, Month) %>%
      dplyr::summarise(TotalObs = n()) %>% 
      relocate(Town) %>%
      mutate(Index = paste(Year, "-", Month, "-01 01:01:01", sep = "")) %>%
      mutate(Index = strptime(Index, format = "%Y-%m-%d %H:%M:%S")) %>%
      ungroup() %>% 
      select(Town, TotalObs, Index) %>%
      pivot_wider(names_from = Town, values_from = TotalObs)
    obsTownMonthW[is.na(obsTownMonthW)] = 0
    mtsTownMonth = ts(obsTownMonthW[,-1], start = c(2014,1), frequency = 12)
    # Town + Day
    obsTownDayW = iNat %>%
      group_by(Town, Year, Month, Day) %>%
      dplyr::summarise(TotalObs = n()) %>% 
      relocate(Town) %>%
      mutate(Index = paste(Year, "-", Month, "-", Day, " 01:01:01", sep = "")) %>%
      mutate(Index = strptime(Index, format = "%Y-%m-%d %H:%M:%S")) %>%
      ungroup() %>% 
      select(Town, TotalObs, Index) %>%
      pivot_wider(names_from = Town, values_from = TotalObs) %>% 
      complete(Index = seq.POSIXt(min(Index), max(Index), by = "day"))
    obsTownDayW[is.na(obsTownDayW)] = 0
    mtsTownDay = ts(obsTownDayW[,-1], start = c(2014, 1), frequency = 365)
    # Town + Hour
    obsTownHourW = iNat %>%
      group_by(Town, Year, Month, Day, Hour) %>%
      dplyr::summarise(TotalObs = n()) %>% 
      relocate(Town)%>%
      mutate(Index = paste(Year, "-", Month, "-", Day, " ", Hour, ":01:01", sep = "")) %>%
      mutate(Index = strptime(Index, format = "%Y-%m-%d %H:%M:%S")) %>%
      ungroup() %>% 
      select(Town, TotalObs, Index) %>%
      pivot_wider(names_from = Town, values_from = TotalObs) %>% 
      complete(Index = seq.POSIXt(min(Index), max(Index), by = "hour"))
    obsTownHourW[is.na(obsTownHourW)] = 0
    mtsTownHour = ts(obsTownHourW[,-1], start = c(2014, 1), frequency = 8760)
    
    # Data list
    mtsList = list(
      NationWide = list(
        Overall = mtsNationOverall,
        Year = mtsNationYear,
        Month = mtsNationMonth,
        Day = mtsNationDay,
        Hour = mtsNationHour),
      Region = list(
        Overall = mtsRegionOverall,
        Year = mtsRegionYear,
        Month = mtsRegionMonth,
        Day = mtsRegionDay,
        Hour = mtsRegionHour),
      County = list(
        Overall = mtsCountyOverall,
        Year = mtsCountyYear,
        Month = mtsCountyMonth,
        Day = mtsCountyDay,
        Hour = mtsCountyHour),
      Town = list(
        Overall = mtsTownOverall,
        Year = mtsTownYear,
        Month = mtsTownMonth,
        Day = mtsTownDay,
        Hour = mtsTownHour)
    )
  }
  # Choropeth animation data
  if (TRUE){
    # Taiwan map and centroid in town scale
    taiTown = st_read("Data/TWPop_mainLand_4326.shp", options = "ENCODING=BIG5") %>% 
      select(geometry, TOWN)
    colnames(taiTown) = c("Town", "geometry")
    taiTownCen = st_read("Data/TaiTownCen_4326.shp", options = "ENCODING=UTF-8")
    colnames(taiTownCen) = c("Town", "geometry")
    # Taiwan map and centroid in county scale
    taiCounty = st_read("Data/TaiCounty_4326.shp", options = "ENCODING=UTF-8")
    colnames(taiCounty) = c("County", "geometry")
    taiCountyCen = st_read("Data/TaiCountyCen_4326.shp", options = "ENCODING=UTF-8")
    colnames(taiCountyCen) = c("County", "geometry")
    # Taiwan map and centroid in region scale
    taiRegion = st_read("Data/TaiRegion_4326.shp", options = "ENCODING=UTF-8")
    taiRegionCen = st_read("Data/TaiRegionCen_4326.shp", options = "ENCODING=UTF-8")
    # Taiwna map and centroid in nationwide scale
    taiNation = st_read("Data/TaiNation_4326.shp", options = "ENCODING=UTF-8") %>% 
      mutate(Nation = "Taiwan") %>% 
      select(Nation, geometry)
    taiNationCen = st_read("Data/TaiNationCen_4326.shp", options = "ENCODING=UTF-8") %>% 
      mutate(Nation = "Taiwan") %>% 
      select(Nation, geometry)
    # Education data
    edu = readxl::read_xlsx("Data/Data.xlsx", sheet = "Education") %>% 
      mutate(Year = as.character(Year))
    # Tax data, represent income
    income = readxl::read_xlsx("Data/Data.xlsx", sheet = "Income") %>% 
      mutate(Year = as.character(Year))
    # population data, 2015 only
    pop = readxl::read_xlsx("Data/Data.xlsx", sheet = "Population") %>% 
      mutate(Year = as.character(Year))
    # Data list
    ChoroList = list(
      content = list(
        Doctor = edu %>% select(-c(Master, Undergraduate, Highschool, Midschool)),
        Master = edu %>% select(-c(Doctor, Undergraduate, Highschool, Midschool)),
        Undergraduate = edu %>% select(-c(Master, Doctor, Highschool, Midschool)),
        Highschool = edu %>% select(-c(Master, Doctor, Undergraduate, Midschool)),
        Midschool = edu %>% select(-c(Master, Doctor, Undergraduate, Highschool)),
        Income = income,
        KidRatio = pop %>% select(Year, Nation, Region, Town, County, KidRatio),
        AdultRatio = pop %>% select(Year, Nation, Region, Town, County, AdultRatio),
        OldRatio = pop %>% select(Year, Nation, Region, Town, County, OldRatio),
        TotalPopulation = pop %>% select(Year, Nation, Region, Town, County, TotalPopulation)
      ),
      mapPoly = list(
        Nationwide = taiNation,
        Region = taiRegion,
        County = taiCounty,
        Town = taiTown
      ),
      mapPt = list(
        Nationwide = taiNationCen,
        Region = taiRegionCen,
        County = taiCountyCen,
        Town = taiTownCen
      )
    )
  }
}

# Build Shiny server
shinyServer(function(input, output) {
  
  # Bar plot
  output$bar = renderPlotly({
    
    barData = obsSTList[[input$SpatialScale]][[input$BarTime]]
    colnames(barData) = c("Space", "Time", "TotalObs")
    barData = mutate(barData, Time = as.factor(Time)) %>% 
      mutate(barData, Space = as.factor(Space))
    
    p = ggplot(barData, aes(x = Time, y = TotalObs)) +
      geom_bar(fill = "#4682B4", stat = "identity") +
      #scale_y_continuous(trans = "sqrt") +
      labs(x = input$BarTime,
           y = "Total Observations",
           title = paste("Comparation Between", input$BarTime, sep = " ")) +
      theme(plot.title = element_text(size = 14L, hjust = 0.5),
            axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
      facet_wrap(vars(Space))
    
    ggplotly(p)
  })
  
  # Line plot
  output$line = renderPlotly({
    lineData = mtsList[[input$SpatialScale]][[input$LineTime]]
    
    ts_plot(lineData, 
            Xtitle = input$LineTime, 
            Ytitle = "Total Observations",
            title = "Change of Total Observations along Time Seris",
            Ygrid = T,
            slider = T)
  })
  
  # Animated choropleth map
  output$choropleth = renderImage({
    outfile = tempfile(fileext = ".gif")  # A temp file to save the output and will be removed later by renderImage

    # prepare the data for animation
    choroData = ChoroList[["content"]][[input$BaseMap]]
    colnames(choroData)[which(colnames(choroData) == input$BaseMap)] = "Value"
    colnames(choroData)[which(colnames(choroData) == input$SpatialScale)] = "SpatialScale"
    choroData = choroData %>% 
      select(Year, SpatialScale, Value) %>% 
      group_by(Year, SpatialScale) %>% 
      dplyr::summarise(Value = sum(Value))
    
    choroMapPoly = ChoroList[["mapPoly"]][[input$SpatialScale]]
    colnames(choroMapPoly)[which(colnames(choroMapPoly) == input$SpatialScale)] = "SpatialScale"
    
    choroObs = obsSTList[[input$SpatialScale]][["Year"]] %>%
      filter(Year >= min(choroData$Year) & Year <= max(choroData$Year))
    choroMapPt = ChoroList[["mapPt"]][[input$SpatialScale]]
    

    # choroData$Value = scale(choroData$Value) %>% as.vector()
    # choroData$Value = choroData$Value * 100

    choroBaseMap = left_join(choroMapPoly, choroData)
    choroPt = right_join(choroMapPt, choroObs)

    # Draw the animate
    p = ggplot() +
      geom_sf(data = choroBaseMap, aes(fill = Value)) +
      geom_sf(data = choroPt, aes(size = TotalObs), color = "red", alpha = 0.5) +
      coord_sf(crs = st_crs(choroBaseMap)) +
      theme_void() +
      scale_fill_continuous(n.breaks = 6) +
      # gganimate
      transition_manual(Year) +
      labs(title = paste(input$BaseMap, "in Year {current_frame} and total observations in each", input$SpatialScale)) +
      theme(plot.title = element_text(size = 10L, hjust = 0.5))

    # Output the animate to ui.R
    anim_save("outfile.gif", animate(p))
    list(src = "outfile.gif", contentType = 'image/gif', height = 650)

  }, deleteFile = TRUE)
  
  # Hull plot
  output$hull = renderPlotly({
    choroData = ChoroList[["content"]][[input$BaseMap]]
    colnames(choroData)[which(colnames(choroData) == input$BaseMap)] = "Value"
    colnames(choroData)[which(colnames(choroData) == input$SpatialScale)] = "SpatialScale"
    choroData = choroData %>% 
      select(Year, SpatialScale, Value) %>% 
      group_by(Year, SpatialScale) %>% 
      dplyr::summarise(Value = sum(Value))
    
    choroObs = obsSTList[[input$SpatialScale]][["Year"]] %>% 
      filter(Year >= min(choroData$Year) & Year <= max(choroData$Year)) %>% 
      ungroup()
    colnames(choroObs)[which(colnames(choroObs) == input$SpatialScale)] = "SpatialScale"
    
    hullData = left_join(choroObs, choroData)
    
    # Plot
    find_hull = function(df) df[chull(df$Value, df$TotalObs), ]
    hulls = ddply(hullData, "Year", find_hull)
    
    p = ggplot(hullData, aes(x = Value, y = TotalObs, colour = Year, fill = Year)) +
      geom_point() +
      ggConvexHull::geom_convexhull(alpha = 0.3) +
      labs(x = input$BaseMap,
           y = "Total Observations",
           title = paste(input$BaseMap, "in different year")) +
      theme(plot.title = element_text(size = 14L, hjust = 0.5))
    ggplotly(p)
  })
  
  # Top10Genus
  output$Top10Genus = renderPlot({
    
    if (input$Top10Time != "Overall"){
      Top10Genus = iNat %>%
        group_by_at(vars(input$Top10Time, "Genus")) %>%
        dplyr::summarise(TotalObs = n()) %>%
        ungroup() %>%
        group_by_at(input$Top10Time) %>%
        top_n(10) %>%
        dplyr::arrange(!!rlang::sym(input$Top10Time), desc(TotalObs)) %>%
        ungroup()
      
      plotList = list(NA)
      
      for (n in 1:length(unique(Top10Genus[[input$Top10Time]]))){
        k = unique(Top10Genus[[input$Top10Time]])[[n]]
        
        Top10Genusk = Top10Genus %>% filter(!!rlang::sym(input$Top10Time) == k)
        
        pk = ggplot(Top10Genusk, aes(x = reorder(Genus, -TotalObs), y = TotalObs)) +
          geom_bar(stat = "identity") +
          labs(x = "Genus",
               y = "Total Observations",
               title = paste("Top Ten Genus in", input$Top10Time, k)) +
          theme(plot.title = element_text(size = 14L, hjust = 0.5),
                axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))
        
        plotList[[n]] = pk
      }
      
      ggpubr::ggarrange(plotlist = plotList, ncol = 1)
    } else {
      Top10Genus = iNat %>%
        group_by_at("Genus") %>%
        dplyr::summarise(TotalObs = n()) %>%
        top_n(10) %>%
        dplyr::arrange(desc(TotalObs))
      
      ggplot(Top10Genus, aes(x = reorder(Genus, -TotalObs), y = TotalObs)) +
        geom_bar(stat = "identity") +
        labs(x = "Genus",
             y = "Total Observations",
             title = paste("Top Ten Genus")) +
        theme(plot.title = element_text(size = 14L, hjust = 0.5),
              axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))
    }
    
  })
  
  # Genus Density map
  output$Density = renderPlot({
    iNatGenus = iNat %>% 
      filter(Genus == input$DensityGenus)
    
    ggplot() +
      geom_sf(data = taiCounty, fill = "grey") +
      stat_density2d(data = iNatGenus, aes(fill = ..level.., x = Long, y = Lat), alpha = 0.5,
                     geom = "polygon") +
      geom_point(data = iNatGenus, aes(x = Long, y = Lat), color = "darkorange", alpha = 0.2) +
      scale_fill_viridis_c() +
      xlim(st_bbox(taiCounty)$xmin - 0.3, st_bbox(taiCounty)$xmax + 0.3) +
      ylim(st_bbox(taiCounty)$ymin - 0.3, st_bbox(taiCounty)$ymax + 0.3) +
      labs(x = NULL, 
           y = NULL, 
           title = paste("Density Map of", input$DensityGenus)) +
      theme(plot.title = element_text(size = 14L, hjust = 0.5))
  })
})