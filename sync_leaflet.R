library(tidyverse)
library(leaflet)
library(htmlwidgets)

# load organizations csv  
geo_data <- read_csv("geo_all.csv") 

# define colors for each group
AR <-"#ff0000"  # Red - Animal-Related
ACH <-  "#008080"  # Teal - Arts, Culture, and Humanities
U <- "#000000"  # Black - Unknown

# define palette for circle markers
pal <- colorFactor(c(AR, ACH, U), domain= c('Animal-Related', 'Arts, Culture and Humanities', 'Unknown'))

map <- leaflet(data = geo_data, options = leafletOptions(minZoom = 11, maxZoom = 18)) %>%
  
  addTiles() %>% 
  
  # restrict boundaries to around Baltimore (doesn't seem to work as expected)
  fitBounds(lng1 = min(geo_data$lon) - 0.11, 
            lat1 = min(geo_data$lat) - 0.11,
            lng2 = max(geo_data$lon) + 0.11, 
            lat2 = max(geo_data$lat) + 0.11) %>% 
  
  # set default view to downtown Baltimore
  setView(lng= -76.62, lat=39.29,zoom=12) %>% 
  
  # add legend to bottom right of map
  addLegend(
    title="Tax-Exempt Organizations in Baltimore",
    position = 'bottomright',
    colors = c(AR,ACH, U), 
    labels = c("Animal-Related", "Arts, Culture, and Humanities", "Unknown"))  %>%
  
  ## Animal-Related group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Animal-Related',],~lon, ~lat, stroke=FALSE,
                 radius = ~log(ASSET_AMT), fillOpacity = .5, color = ~pal(codes), #color defined above
                 #create pop-up window with information for each marker
                 popup = ~ paste(NAME, "<br/>",
                                 "Address:", STREET,"<br/>",
                                 "Assets:", ASSET_AMT, "<br/>",
                                 "Income:", INCOME_AMT, "<br/>",
                                 "Revenue:", REVENUE_AMT, "<br/>",
                                 "Category:", codes),
                 
                 group="Animal-Related") %>% 
  
  ## Arts, Culture, & Humanities group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Arts, Culture and Humanities',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Arts, Culture and Humanities") %>% 
  
  ## Unknown group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Unknown',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT), fillOpacity = .5, color = ~pal(codes), 
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Unknown") %>% 

  # Add user controls to toggle groups displayed
  addLayersControl(
    overlayGroups = c("Animal-Related","Arts, Culture and Humanities", "Unknown"),
    options = layersControlOptions(collapsed = TRUE)
  )
  
map

# save map as html document 
sav.file <- "/Users/jbjrV/OneDrive/Code for Baltimore/sync_leaflet.html"
saveWidget(map, file=sav.file, selfcontained = F)

