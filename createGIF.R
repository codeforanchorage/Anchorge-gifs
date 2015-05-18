library(rgdal)
require(maptools)
require(ggplot2)
require(plyr)
library(ggplot2)
library(ggmap)
library(animation)
library(dplyr)
no_formatting  <-   theme(axis.line=element_blank(),
                          axis.text.x=element_blank(),
                          axis.text.y=element_blank(),
                          axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          legend.position="none",
                          panel.background=element_blank(),
                          panel.border=element_blank(),
                          panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),
                          plot.background=element_blank())



map = readOGR("output.geojson", "OGRGeoJSON")
#plot(map)
map@data$id = rownames(map@data)
gCentroid(map, byid = T)
map_points <- gCentroid(map, byid = T)@coords
year_built <- as.numeric(as.character((map@data$year_built)))
map_data <- data.frame(cbind(map_points, year_built))
map_data <- map_data[!is.na(map_data$year_built),]

anc_map <- get_map("Anchorage, AK", maptype = "toner", zoom = 10)


saveGIF({
for(i in sort(unique(map_data$year_built))[-1]) {

this_year <- map_data %>%
     filter(year_built == i) 

previous_years <- map_data %>%
  filter(year_built < i)

print(ggmap(anc_map, extent = "device") + geom_point(data = previous_years, aes(x = x, y = y), color = "darkgrey", alpha = 0.4, size = 0.8)  + 
                   geom_point(data = this_year, aes(x = x, y = y), color = "darkorchid", alpha = 0.8, size = 2) + #no_formatting +
  annotate("text", x = -150.2, y = 61.35, label = paste("Year:", i), fontface = "italic") +
  annotate("text", x = -149.9, y = 61.4, label = "New Construction in Anchorage's First Century", fontface = "bold", size = 6) 
  )
}}, filename = "AnchorageGrowth.gif", interval = 0.5)
