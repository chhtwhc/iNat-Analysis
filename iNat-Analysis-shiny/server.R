# Setting environment
library(shiny)
library(leaflet)
library(mapview)
library(shinycssloaders)
library(tidyverse)
library(shinyalert)
library(showtext)
library(readxl)
library(plotly)
library(sf)
library(xts)
library(TSstudio)
library(esquisse)

# setwd("D:/CHU 2.0/Forest/110-1 Space Time data Viz/Term Project/iNat-Analysis/iNat-Analysis-shiny")  # 上傳到 Shiny 時記得註解掉

# Data processing
if (TRUE){
  # Basic data 
  if (TRUE){
    # iNaturalist data
    regionN = c("臺北市", "新北市", "基隆市", "新竹市", "桃園市", "新竹縣", "宜蘭縣")
    regionM = c("臺中市", "苗栗縣", "彰化縣", "南投縣", "雲林縣")
    regionE = c("花蓮縣", "臺東縣")
    regionS = c("高雄市", "臺南市", "嘉義市", "嘉義縣", "屏東縣")
    iNat = read_xlsx("../Data/Data.xlsx", sheet = "iNaturalist") %>%
      filter(Town != "") %>% 
      mutate(Time = strptime(Time, format = "%Y-%m-%dT%H:%M:%SZ")) %>% 
      filter(Time >= "2014-01-01 00:00:00") %>% 
      mutate(Year = format(Time, "%Y"), Month = format(Time, "%m"),
             Day = format(Time, "%d"), Hour = format(Time, "%H"), 
             Weekday = format(Time, "%w")) %>% 
      mutate(Region = ifelse(County %in% regionN, "北部", 
                             ifelse(County %in% regionM, "中部", 
                                    ifelse(County %in% regionE, "東部", "南部"))))
    # Education data
    edu = read_xlsx("../Data/Data.xlsx", sheet = "Education")
    # Tax data, represent income
    income = read_xlsx("../Data/Data.xlsx", sheet = "Income")
    # population data, 2015 only
    pop = read_xlsx("../Data/Data.xlsx", sheet = "Population")
    # Taiwan map in town scale
    taiTown = st_read("../Data/TWPop_mainLand_4326.shp", options = "ENCODING=BIG5") %>% 
      select(geometry, TOWN, COUNTY)
    # Taiwan map in county scale
    taiCounty = st_read("../Data/TaiCounty_4326.shp", options = "ENCODING=UTF-8")
  }
  # Bar plot data
  if (TRUE){
    # Nation + Overall
    obsNationOverall = iNat %>%
      summarise(TotalObs = n()) %>% 
      mutate(Nation = "Taiwan", Time = 2021) %>% 
      relocate(Nation, Time)
    obsNationOverall[is.na(obsNationOverall)] = 0
    # Nation + Year
    obsNationYear = iNat %>%
      group_by(Year) %>%
      summarise(TotalObs = n()) %>% 
      mutate(Nation = "Taiwan",) %>% 
      relocate(Nation)
    obsNationYear[is.na(obsNationYear)] = 0
    # Nation + Month
    obsNationMonth = iNat %>%
      group_by(Month) %>%
      summarise(TotalObs = n()) %>% 
      mutate(Nation = "Taiwan",) %>% 
      relocate(Nation)
    obsNationMonth[is.na(obsNationMonth)] = 0
    # Nation + Weekday
    obsNationWeekday = iNat %>%
      group_by(Weekday) %>%
      summarise(TotalObs = n()) %>% 
      mutate(Nation = "Taiwan",) %>% 
      relocate(Nation)
    obsNationWeekday[is.na(obsNationWeekday)] = 0
    # Nation + Hour
    obsNationHour = iNat %>%
      group_by(Hour) %>%
      summarise(TotalObs = n()) %>% 
      mutate(Nation = "Taiwan",) %>% 
      relocate(Nation)
    obsNationHour[is.na(obsNationHour)] = 0
    # Region + Overall
    obsRegionOverall = iNat %>%
      group_by(Region) %>%
      summarise(TotalObs = n()) %>% 
      mutate(Time = 2021) %>% 
      relocate(Region, Time)
    obsRegionOverall[is.na(obsRegionOverall)] = 0
    # Region + Year
    obsRegionYear = iNat %>%
      group_by(Region, Year) %>%
      summarise(TotalObs = n())
    obsRegionYear[is.na(obsRegionYear)] = 0
    # Region + Month
    obsRegionMonth = iNat %>%
      group_by(Region, Month) %>%
      summarise(TotalObs = n())
    obsRegionMonth[is.na(obsRegionMonth)] = 0
    # Region + Weekday
    obsRegionWeekday = iNat %>%
      group_by(Region, Weekday) %>%
      summarise(TotalObs = n())
    obsRegionWeekday[is.na(obsRegionWeekday)] = 0
    # Region + Hour
    obsRegionHour = iNat %>%
      group_by(Region, Hour) %>%
      summarise(TotalObs = n())
    obsRegionHour[is.na(obsRegionHour)] = 0
    # County + Overall
    obsCountyOverall = iNat %>%
      group_by(County) %>%
      summarise(TotalObs = n()) %>% 
      mutate(Time = 2021) %>% 
      relocate(County, Time)
    obsCountyOverall[is.na(obsCountyOverall)] = 0
    # County + Year
    obsCountyYear = iNat %>%
      group_by(County, Year) %>%
      summarise(TotalObs = n())
    obsCountyYear[is.na(obsCountyYear)] = 0
    # County + Month
    obsCountyMonth = iNat %>%
      group_by(County, Month) %>%
      summarise(TotalObs = n())
    obsCountyMonth[is.na(obsCountyMonth)] = 0
    # County + Weekday
    obsCountyWeekday = iNat %>%
      group_by(County, Weekday) %>%
      summarise(TotalObs = n())
    obsCountyWeekday[is.na(obsCountyWeekday)] = 0
    # County + Hour
    obsCountyHour = iNat %>%
      group_by(County, Hour) %>%
      summarise(TotalObs = n())
    obsCountyHour[is.na(obsCountyHour)] = 0
    # Town + Overall
    obsTownOverall = iNat %>%
      group_by(Town) %>%
      summarise(TotalObs = n()) %>% 
      mutate(Time = 2021) %>% 
      relocate(Town, Time)
    obsTownOverall[is.na(obsTownOverall)] = 0
    # Town + Year
    obsTownYear = iNat %>%
      group_by(Town, Year) %>%
      summarise(TotalObs = n())
    obsTownYear[is.na(obsTownYear)] = 0
    # Town + Month
    obsTownMonth = iNat %>%
      group_by(Town, Month) %>%
      summarise(TotalObs = n())
    obsTownMonth[is.na(obsTownMonth)] = 0
    # Town + Weekday
    obsTownWeekday = iNat %>%
      group_by(Town, Weekday) %>%
      summarise(TotalObs = n())
    obsTownWeekday[is.na(obsTownWeekday)] = 0
    # Town + Hour
    obsTownHour = iNat %>%
      group_by(Town, Hour) %>%
      summarise(TotalObs = n())
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
      summarise(TotalObs = n()) %>% 
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
      summarise(TotalObs = n()) %>% 
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
      summarise(TotalObs = n()) %>% 
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
      summarise(TotalObs = n()) %>% 
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
      summarise(TotalObs = n()) %>% 
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
      summarise(TotalObs = n()) %>% 
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
      summarise(TotalObs = n()) %>% 
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
      summarise(TotalObs = n()) %>% 
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
      summarise(TotalObs = n()) %>% 
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
      summarise(TotalObs = n()) %>% 
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
      summarise(TotalObs = n()) %>% 
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
      summarise(TotalObs = n()) %>% 
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
}



# Build Shiny server
shinyServer(function(input, output) {
  
  # Bar plot
  output$bar = renderPlot({
    
    barData = obsSTList[[input$SpatialScale]][[input$BarTime]]
    colnames(barData) = c("Space", "Time", "TotalObs")
    
    ggplot(barData, aes(x = Time, weight = TotalObs)) +
      geom_bar(fill = "#4682B4") +
      #scale_y_continuous(trans = "sqrt") +
      labs(x = input$BarTime,
           y = "Total Observations",
           title = paste("Comparation Between", input$BarTime, sep = " ")) +
      theme(plot.title = element_text(size = 14L, hjust = 0.5)) +
      facet_wrap(vars(Space))
    
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
  
  # Heatmap plot
  output$heatmap = renderPlotly({
    heatmapData = mtsList[[input$SpatialScale]][[input$LineTime]]
    
    if (input$SpatialScale %in% c("Region", "County") & input$LineTime %in% c("Year", "Month", "Day")){
      for (n in 1:ncol(heatmapData)){
        heatmapData[,n] %>% ts_heatmap()
      }
    }
    
  })
  
})