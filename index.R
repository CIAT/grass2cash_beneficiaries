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


iDir <- "."

wards  <- readOGR(paste0(iDir, "/data/ward_boundaries.shp"))
ben_wards <- read_csv(paste0(iDir, "/data/", "benefeciaries.csv"))

# #spatial join
ben_wards <- merge(wards, ben_wards, by.x = "ward", by.y = "ward_name", duplicateGeoms = T, sort=FALSE, all.x = FALSE)

ben_wards_data <- read_csv(paste0(iDir, "/data/", "benefeciaries_collapsed.csv"))

ben_wards_data$total_beneficiaries <- apply(ben_wards_data[,9:15], 1, sum, na.rm=TRUE)

demo_trials <- read.csv(paste0(iDir, "/data/", "demo_trials.csv"), stringsAsFactors=FALSE)

#writeOGR(combinedData, paste0(iDir, "/combinedData.shp"), "combinedData", driver="ESRI Shapefile")

#labels
ward_names <- paste("Ward Name: ", ben_wards@data$ward)
popup1 <- paste0("Trial Name: ", demo_trials$Trial_Name)

#colors <- c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3" )

colors <- c("red", "green", "yellow", "violet", "blue", "khaki", "cyan" )

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
              # highlight = highlightOptions(weight = 5,
              #                              color = "white",
              #                              dashArray = "",
              #                              bringToFront = TRUE),
              label = ward_names,
              labelOptions = labelOptions(style = list(`font-weight` = "normal",
                                                       padding = "3px 8px"), textsize = "15px", direction = "auto")) %>%

  addMinicharts(ben_wards_data$POINT_X, ben_wards_data$POINT_Y,
                type = "pie",
                chartdata = ben_wards_data[, c("Brachiaria Cayman", "Brachiaria MG4", "Brachiaria Mulato II", "Brachiaria Xaraes", "Panicum Masaai", "Panicum Mombasa", "Panicum Tanzania")],
                colorPalette = colors,
                width = 60 * sqrt(ben_wards_data$total_beneficiaries) / sqrt(max(ben_wards_data$total_beneficiaries)), transitionTime = 0) %>%

  # addLegend("topright",
  #           colors = colors, opacity = 1,
  #           labels = c("Brachiaria Cayman", "Brachiar MG4", "Brachiar Mulato II", "Brachiar Xaraes", "Panicum Masaai", "Panicum Mombasa", "Panicum Tanzania")) %>%
  
  addLayersControl(overlayGroups = c("Ward boundaries", "Demo Trials"), options = layersControlOptions(collapsed = FALSE))


saveWidget(map.ll, file = paste0(iDir, "/", "index", ".html", sep = ""))
