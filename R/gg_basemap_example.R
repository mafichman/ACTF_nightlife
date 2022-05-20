# bbox for philly and ggmap basemap

library(tidyvere)
library(sf)
library(ggmap)

base_map <- get_stamenmap(bbox = unname(st_bbox(ll(st_buffer(phila_shp,1500)))),
                          force = TRUE, maptype = "toner-lite", zoom = 12)

ggmap(base_map)+
  geom_sf(data = ll(phila_shp), 
          inherit.aes = FALSE,
          fill = "transparent", 
          color = "blue") +
  geom_sf(data = ll(licenses %>% 
                      filter(licensetype == "Special Assembly Occupancy", 
                             licensestatus == "Active") %>% 
                      st_join(., zones) %>% 
                      filter(is.na(assembly_allowed) == FALSE) %>%
                      st_transform(st_crs(phila_shp))), 
          inherit.aes = FALSE,
          color = "red",
          size = 1,
          alpha = 0.7)+
  ggtitle("Philly and Surroundings") +
  facet_wrap(~assembly_allowed)+
  mapTheme
