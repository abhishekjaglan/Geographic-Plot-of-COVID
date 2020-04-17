#Libraries
library(leaflet)
library(ggmap)
library(tidyverse)
library(leaflet.extras)
library(htmltools)
library(ggplot2)
library(maps)
library(mapproj)
library(mapdata)

#World Map
w <- map_data('world')
icj <- map_data('world',
                region = c('India','China','Japan'))
ggplot(icj, aes(x = long, y = lat, group = group,fill = region))+
  geom_polygon(color = 'black') 

ggplot(icj, aes(x = long, y = lat, group = group,fill = region))+
  geom_polygon(color = 'black') + 
  coord_map('polyconic')

#US States
s <- map_data('state')

ggplot(s, aes(x = long, y = lat, group = group))+
  geom_polygon(color = 'black') + 
  coord_map('polyconic')

ggplot(s, aes(x = long, y = lat, group = group,fill = region))+
  geom_polygon(color = 'black') + 
  coord_map('polyconic')+
  guides(fill = F)

#covid data - usa
c <- read.csv(file.choose(), header = T)
usa <- c %>% filter(country=='United States')
ind <- c %>% filter(country=='India')
chi <- c %>% filter(country == 'China')

usa <- usa %>% group_by(province) %>%
  summarise(count = n())%>%
  arrange(desc(count))

chi <- chi %>% group_by(province) %>%
  summarise(count = n())%>%
  arrange(desc(count))

ind <- ind %>% group_by(province) %>%
  summarise(count = n())%>%
  arrange(desc(count))

#Merge Data
usa$province <- tolower(usa$province)
data <- merge(s, usa,
              by.x = 'region',
              by.y = 'province')

#Case in each state
ggplot(data, aes(x = long, y = lat,
                 group = group,
                 fill = count)) +
  geom_polygon(color = 'grey')+
  coord_map('polyconic') +
  scale_fill_gradient2(low = 'white', high = 'red') +
  ggtitle('COVID cases in US')

#USA- times squre 
leaflet() %>% addTiles('CartoDB') %>%
  setView(lng = -73.9855, lat = 40.7580,
          zoom = 15) %>%
  addMarkers(lng = -73.9855, lat = 40.7580)

# New Delhi, Delhi
leaflet() %>% addTiles('CartoDB') %>%
  setView(lng = 77.2090, lat = 28.6139,
          zoom = 15) %>%
  addCircleMarkers(lng = 77.2090,
                   lat = 28.6139,
                   radius = 5,
                   color = 'blue')
#COVID data - leaflet map of US
usa <- c %>% filter(country == "United States")
usa <- usa %>% group_by(city, province, longitude, latitude) %>%
  summarise(count = n())%>%
  arrange(desc(count))

#US map for each county
mycolor <- colorNumeric(palette = 'RdBu',
                        domain = c(1:1000),
                        reverse = T)

usa %>%
  leaflet()%>%
  addProviderTiles('CartoDB') %>%
  addCircleMarkers(radius = ~0.01*count,
                   color = ~mycolor(count),
                   popup = ~paste0(city,
                                   "<br/>",
                                   count))%>%
addLegend(pal = mycolor,
          values = c(1:1000),
          opacity = 0.75,
          title = 'Count',
          position = 'topleft')
