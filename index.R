rm(list = ls(all.names = TRUE))

library(tidyverse)
library(plyr)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(rgdal)
library(sf)
library(leaflet.minicharts)
library(leaflet.extras)


iDir <- "."

wards  <- readOGR(paste0(iDir, "/data/ward_boundaries.shp"))
woredas  <- readOGR(paste0(iDir, "/data/woreda_boundaries.shp"))
ben_wards <- read_csv(paste0(iDir, "/data/", "benefeciaries.csv"))

# #spatial join
ben_wards <- merge(wards, ben_wards, by.x = "ward", by.y = "ward_name", duplicateGeoms = T, sort=FALSE, all.x = FALSE)

ben_wards_data <- read_csv(paste0(iDir, "/data/", "benefeciaries_collapsed.csv"))

ben_wards_data$total_beneficiaries <- apply(ben_wards_data[,4:16], 1, sum, na.rm=TRUE)

demo_trials <- read.csv(paste0(iDir, "/data/", "demo_trials.csv"), stringsAsFactors=FALSE)

#writeOGR(combinedData, paste0(iDir, "/combinedData.shp"), "combinedData", driver="ESRI Shapefile")

#labels
ward_names <- paste("Ward Name: ", ben_wards@data$ward)
woreda_names <- paste("Woreda Name: ", woredas@data$admin_name)
popup1 <- paste0("Trial Name: ", demo_trials$Trial_Name)

# colors <- c("red", "green", "yellow", "violet", "blue", "khaki", "cyan" )

colors <- c("#6A3D9A", "#FB9A99", "#BEAED4", "#B3E2CD", "#F4CAE4", "#FFF2AE", "#B3CDE3", "#984EA3", "#DECBE4", "#FB8072", "#F1E2CC", "#666666", "#A65628")

map.ll <- leaflet() %>%

  addProviderTiles("Esri.WorldStreetMap") %>%

  addMiniMap(position = "bottomright") %>%

  setView(34.491865, 0.220047, zoom = 10) %>%
  
  addMarkers(
    demo_trials$Long, demo_trials$Lat,
    popup = popup1,
    group = "Demo Trials") %>% 

  addPolygons(data = ben_wards,
              weight = 2,
              color = "brown" ,
              dashArray = "3",
              fillOpacity = 0,
              group = "Ward boundaries",
              label = ward_names,
              labelOptions = labelOptions(style = list(`font-weight` = "normal",
                                                       padding = "3px 8px"), textsize = "15px", direction = "auto")) %>%
  
  addPolygons(data = woredas,
              weight = 2,
              color = "brown" ,
              dashArray = "3",
              fillOpacity = 0,
              group = "Woreda boundaries",
              label = woreda_names,
              labelOptions = labelOptions(style = list(`font-weight` = "normal",
                                                       padding = "3px 8px"), textsize = "15px", direction = "auto")) %>%
  

  addMinicharts(ben_wards_data$POINT_X, ben_wards_data$POINT_Y,
                type = "pie",
                chartdata = ben_wards_data[, c("Brachiaria cayman", "Brachiaria MG4", "Brachiaria mulato II", "Brachiaria xaraes", 
                                               "Panicum masaai", "Panicum mombasa", "Panicum tanzania", "Pennisetum purpureum", "Brachiaria mutica", 
                                               "Brachiaria mulato I", "Panicum maximam", "Pennisetum pedicellatum", "Brachiaria decumbens Stapf")],
                colorPalette = colors,
                width = 60 * sqrt(ben_wards_data$total_beneficiaries) / sqrt(max(ben_wards_data$total_beneficiaries)), transitionTime = 0) %>%
  

  # addLegend("topright",
  #           colors = colors, opacity = 1,
  #           labels = c("Brachiaria Cayman", "Brachiar MG4", "Brachiar Mulato II", "Brachiar Xaraes", "Panicum Masaai", "Panicum Mombasa", "Panicum Tanzania")) %>%
  
  addLayersControl(overlayGroups = c("Ward boundaries", "Woreda boundaries", "Demo Trials"), options = layersControlOptions(collapsed = FALSE)) %>% 
  
  addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, 
                                     autoCenter = TRUE, maxZoom = 60, 
                                     setView = TRUE)) 
# activateGPS()


saveWidget(map.ll, file = paste0(iDir, "/", "index", ".html", sep = ""))

