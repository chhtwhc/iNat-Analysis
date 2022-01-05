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
library(gifski)
library(transformr)
library(directlabels)

showtext::showtext_auto(enable = TRUE)
options(scipen = 999)

load("AllData.Rdata")

# Build Shiny server
shinyServer(function(input, output) {
  
  # Bar plot
  output$bar = renderPlotly({
    
    barData = dList[["bar"]][[input$SpatialScale]][[input$BarTime]]
    colnames(barData) = c("Space", "Time", "TotalObs")
    barData = barData %>% 
      mutate(Time = as.factor(Time), Space = as.factor(Space))
    
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
    lineData = dList[["line"]][[input$SpatialScale]][[input$LineTime]]
    
    ts_plot(lineData, 
            Xtitle = input$LineTime, 
            Ytitle = "Total Observations",
            title = "Change of Total Observations along Time Seris",
            Ygrid = T,
            slider = T)
  })
  
  # Animated choropleth map
  output$choropleth = renderImage({
    # outfile = tempfile(fileext = ".gif")  # A temp file to save the output and will be removed later by renderImage

    # prepare the data for animation
    choroData = dList[["animate"]][["content"]][[input$BaseMap]]
    colnames(choroData)[which(colnames(choroData) == input$BaseMap)] = "Value"
    colnames(choroData)[which(colnames(choroData) == input$SpatialScale)] = "SpatialScale"
    choroData = choroData %>% 
      select(Year, SpatialScale, Value) %>% 
      group_by(Year, SpatialScale) %>% 
      dplyr::summarise(Value = sum(Value))
    
    choroMapPoly = dList[["animate"]][["mapPoly"]][[input$SpatialScale]]
    colnames(choroMapPoly)[which(colnames(choroMapPoly) == input$SpatialScale)] = "SpatialScale"
    
    choroObs = dList[["bar"]][[input$SpatialScale]][["Year"]] %>%
      filter(Year >= min(choroData$Year) & Year <= max(choroData$Year))
    choroMapPt = dList[["animate"]][["mapPt"]][[input$SpatialScale]]
    

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
    choroData = dList[["animate"]][["content"]][[input$BaseMap]]
    colnames(choroData)[which(colnames(choroData) == input$BaseMap)] = "Value"
    colnames(choroData)[which(colnames(choroData) == input$SpatialScale)] = "SpatialScale"
    choroData = choroData %>% 
      select(Year, SpatialScale, Value) %>% 
      group_by(Year, SpatialScale) %>% 
      dplyr::summarise(Value = sum(Value))
    
    choroObs = dList[["bar"]][[input$SpatialScale]][["Year"]] %>% 
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
      Top10Genus = dList[["iNaturalist"]] %>%
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
      Top10Genus = dList[["iNaturalist"]] %>%
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
    iNatGenus = dList[["iNaturalist"]] %>% 
      filter(Genus == input$DensityGenus)
    
    ggplot() +
      geom_sf(data = dList[["animate"]][["mapPoly"]][["County"]], fill = "grey") +
      stat_density2d(data = iNatGenus, aes(fill = ..level.., x = Long, y = Lat), alpha = 0.5,
                     geom = "polygon") +
      geom_point(data = iNatGenus, aes(x = Long, y = Lat), color = "darkorange", alpha = 0.2) +
      scale_fill_viridis_c() +
      xlim(st_bbox(dList[["animate"]][["mapPoly"]][["County"]])$xmin - 0.3, st_bbox(dList[["animate"]][["mapPoly"]][["County"]])$xmax + 0.3) +
      ylim(st_bbox(dList[["animate"]][["mapPoly"]][["County"]])$ymin - 0.3, st_bbox(dList[["animate"]][["mapPoly"]][["County"]])$ymax + 0.3) +
      labs(x = NULL, 
           y = NULL, 
           title = paste("Density Map of", input$DensityGenus)) +
      theme(plot.title = element_text(size = 14L, hjust = 0.5))
  })
})