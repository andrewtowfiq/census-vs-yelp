library(ggplot2)

states <- map_data("state")
west_coast <- subset(states, region %in% c("washington"))
only_wa <- subset(states, region == "washington")


all.counties <- map_data("county")
washington_county <- subset(all.counties, region == "washington")
head(washington_county)

create_base <- ggplot(data = only_wa, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "white") #color = perimeter of map , fill = whole map color fill 
create_base + theme_nothing() + 
   geom_polygon(data = washington_county, fill = NA, color = "red") + # color = color of county outline  
   geom_polygon(color = "black", fill = NA) # color = perimeter of map that should be kept same as previous color


