# playing around with maps

rm(list=ls())

library(ggplot2)
library(ggmap)
library(dplyr)
library(tidyr)
library(openxlsx)
library(rmarkdown)
library(sysfonts)
font_add_google("Roboto Mono")

states_map <- map_data("state")

latlong <- data.frame(rbind(
  c(1, "Pipers",47.711661, -122.379126),
  c(2, "Molendorph",47.711820, -122.370965),
  c(3, "Venema",47.711451, -122.370642),
  c(4,"North Creek", 47.709538, -122.366411),
  c(5,"Viewlands", 47.706920, -122.364522),
  c(6,"Pipers", 47.705040, -122.364296),
  c(7, "Pipers",47.704931, -122.363795),
  c(8, "Trib M",47.703975, -122.363343))) %>%
  rename(sitenumber=X1
         ,waterBody = X2
         ,latitude=X3
         ,longitude = X4) %>%
  mutate(region = "washington",
         temp = sitenumber)

latlong %>%
  ggplot() +
  geom_map(map = states_map,
           aes(map_id = region), color = "white", fill = NA, linewidth = 0.2)+
  geom_point(aes(x = longitude, y = latitude),
             alpha = 1, size = 4, color = "green1")



ggplot(latlong, aes(longitude, latitude)) +
  geom_polygon(aes(fill = temp)) +
  coord_map("albers",  lat0 = 45.5, lat1 = 29.5) +
  geom_point()

seattle <- c(left = -123, bottom = 47, right = -122, top = 48)
get_stamenmap(seattle, zoom = 12, maptype = "toner-lite") %>% ggmap()





ggmap(get_map(location = c(lon = -122.335167, lat = 47.608013),
                    zoom = 11, scale = 2)) +
  geom_point(aes(x = Longitude, y = Latitude,
                 colour = waterBody), data = i2, size = 0.5)
