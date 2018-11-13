# library
library(tidyverse)

# Hexbin data downloaded from here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map. 

library(geojsonio)
spdf <- geojson_read("C:/Users/Eeslami/Desktop/us_states_hexgrid.geojson",  what = "sp")

# I need to 'fortify' the data to be able to show it with ggplot2 (we need a data frame format)
library(broom)
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

ibdata <- read_csv("C:/Users/Eeslami/Desktop/IBdata.csv")

spdf_fortified <- inner_join(spdf_fortified, ibdata, "id")

spdf_fortified$rating <- factor(spdf_fortified$rating, levels = c("N/A", "Low", "Very low", "Medium", "High", "Very high"))

# Calculate the centroid of each hexagon to add the label:
library(rgeos)
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

centers <- centers %>% mutate(text_color = ifelse(id %in% c("CA", "TN", "VT", "WA"), "white", "black"))

# Now I can plot this shape easily as described before:
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group, fill=factor(rating)), color="black", size = .6) +
  geom_text(data=centers, aes(x=x, y=y, label=id), size = 6, color = centers$text_color, fontface = "bold") + 
  theme_void() +
  theme(plot.background = element_rect("white")) +
  scale_fill_brewer(direction = 1, name = "") +
  coord_map()
