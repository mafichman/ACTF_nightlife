# Firearm violence and license locations

# Libraries

library(tidyverse)
library(sf)
library(lubridate)
library(RSocrata)
library(viridis)
library(tigris)
library(ggmap)
library(jsonlite)
library(mapview)

plotTheme <- theme(
  plot.title =element_text(size=12),
  plot.subtitle = element_text(size=8),
  plot.caption = element_text(size = 6),
  axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
  axis.text.y = element_text(size = 10),
  axis.title.y = element_text(size = 10),
  # Set the entire chart region to blank
  panel.background=element_blank(),
  plot.background=element_blank(),
  #panel.border=element_rect(colour="#F0F0F0"),
  # Format the grid
  panel.grid.major=element_line(colour="#D0D0D0",size=.75),
  axis.ticks=element_blank())

mapTheme <- theme(plot.title =element_text(size=12),
                  plot.subtitle = element_text(size=8),
                  plot.caption = element_text(size = 6),
                  axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_line(colour = 'transparent'),
                  panel.grid.minor=element_blank(),
                  legend.direction = "vertical", 
                  legend.position = "right",
                  plot.margin = margin(1, 1, 1, 1, 'cm'),
                  legend.key.height = unit(1, "cm"), legend.key.width = unit(0.2, "cm"))



# https://cityofphiladelphia.github.io/carto-api-explorer/#incidents_part1_part2
# Jan 1, 2018 to June 7, 2022
philaCrime <- read.csv("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272018-01-01%27%20AND%20dispatch_date_time%20%3C%20%272022-06-06%27") #%>%
  #filter(str_detect(text_general_code, "Assault") == TRUE)

# Keep only violent crime types, fix typos in homicide data entry,
# Create time variables
# Filter out data with bad geo

violentCrime <- philaCrime %>% 
  filter(text_general_code %in% c("Other Assaults", 
                                               "Thefts", 
                                               "Aggravated Assault Firearm", 
                                               "Aggravated Assault No Firearm", 
                                               "Robbery Firearm", 
                                               "Robbery No Firearm", 
                                               "Homicide - Criminal", 
                                               "Homicide - Criminal ", 
                                               "Rape", 
                                               "Other Sex Offenses (Not Commercialized")) %>%
  mutate(text_general_code = ifelse(str_detect(text_general_code, "Homicide") == TRUE,
         "Homicide - Criminal", text_general_code)) %>%
  mutate(interval60 = floor_date(ymd(dispatch_date), unit = "hour"),
         week = week(interval60),
         month = month(interval60),
         dotw = wday(interval60, label=TRUE),
         year = year(interval60),
         hour = hour(hms(dispatch_time))) %>%
  mutate(after_six = ifelse(hour > 17 | hour < 7, "6PM - 6AM", "6AM - 6PM"))

# Look at trends by month and year

violentCrime %>%
  group_by(year, month, after_six) %>%
  tally() %>%
ggplot()+
  geom_line(aes(x= month, y = n, color = as.factor(year)))+
  facet_wrap(~after_six)+
  plotTheme

# Make the crime data spatial using lat/lon
# Filter out data with bad/wrong lat/lon ~ 3K observations

violentCrime_shp <- violentCrime %>%
  filter(point_y > 1,
         point_x > -76,
         point_x < -73) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(2272)

# Take the licenses for restaurants, amusement, assembly, food establishment
# Create 100 ft buffers around current licenses
# Then do a similar thing for older licenses for comparison - maybe 2018 or 19

licenses <- st_read("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+business_licenses&filename=business_licenses&format=geojson&skipfields=cartodb_id") %>%
  filter(licensetype %in% c("Amusement", "Special Assembly Occupancy", "Sidewalk Cafe") |
           str_detect(licensetype, "Food Estab")) %>%
  st_transform(crs = 2272)
  
current_buffer <- licenses %>%
  filter(licensestatus == "Active") %>%
  st_union() %>%
  st_buffer(200) %>%
  st_as_sf(crs = 2272) %>%
  mutate(in_buffer = "in buffer")

# Incidents relative to license locations

# It's safer at any time of day to be within 200 feet of a restaurant, bar,
# or place of amusement
st_join(violentCrime_shp %>% 
          filter(year > 2020) %>%
          filter(text_general_code %in% c("Aggravated Assault Firearm", 
                                          "Aggravated Assault No Firearm", 
                                          "Robbery Firearm", 
                                          "Robbery No Firearm", 
                                          "Homicide - Criminal", 
                                          "Homicide - Criminal ", 
                                          "Rape", 
                                          "Other Sex Offenses (Not Commercialized)")), current_buffer) %>%
  mutate(Proximate_To_License = ifelse(is.na(in_buffer) == TRUE, "Over 200 ft Away", "Within 200 ft")) %>%
  group_by(Proximate_To_License, dotw, hour) %>%
  tally() %>%
    ggplot()+
    geom_bar(aes(x = hour, y = n, fill = Proximate_To_License), 
             stat = "identity", position = "dodge")+
  facet_wrap(~dotw)+
  labs(title="Violent Crimes, 2021-2022 by Proximity to Restaurant, Assembly, and Amusement Licenses",
       subtitle= "Homicide, Aggravated Assault, Rape, Robbery, Theft - with and without Firearms. Source: Philadelphia Dept. Of Licenses & Inspections, Philadelphia Police Dept.",
       x="Hour of the Day", 
       y="Number of Licenses")+
  plotTheme

# Gun crime relative to license locations

# It's safer at any time of day to be within 200 feet of a restaurant, bar,
# or place of amusement
st_join(violentCrime_shp %>% 
          filter(year > 2020) %>%
          filter(text_general_code %in% c("Aggravated Assault Firearm", 
                                          "Robbery Firearm", 
                                          "Homicide - Criminal", 
                                          "Homicide - Criminal ")), current_buffer) %>%
  mutate(Proximate_To_License = ifelse(is.na(in_buffer) == TRUE, "Over 200 ft Away", "Within 200 ft")) %>%
  group_by(Proximate_To_License, dotw, hour) %>%
  tally() %>%
  ggplot()+
  geom_bar(aes(x = hour, y = n, fill = Proximate_To_License), 
           stat = "identity", position = "dodge")+
  facet_wrap(~dotw)+
  labs(title="Reported Firearm Incidents, 2021-2022 by Proximity to Restaurant, Assembly, and Amusement Licenses",
       subtitle= "Homicide and Aggravated Assault, Robbery with Firearms. Source: Philadelphia Dept. Of Licenses & Inspections, Philadelphia Police Dept.",
       x="Hour of the Day", 
       y="Number of Licenses")+
  plotTheme

# It's safer at any time of day to be within 200 feet of a restaurant, bar,
# or place of amusement
st_join(violentCrime_shp %>% 
          filter(text_general_code %in% c("Aggravated Assault Firearm", 
                                          "Robbery Firearm", 
                                          "Homicide - Criminal", 
                                          "Homicide - Criminal " )), 
        current_buffer) %>%
  as.data.frame() %>%
  mutate(Proximate_To_License = ifelse(is.na(in_buffer) == TRUE, "Over 200 ft Away", "Within 200 ft")) %>%
  group_by(Proximate_To_License, after_six, year, month) %>%
  tally() %>%
  mutate(date = lubridate::my(paste(month, year))) %>%
  filter(after_six == "6PM - 6AM") %>%
  ggplot()+
  geom_line(aes(x = date, y = n, color = Proximate_To_License))+
  labs(title="Monthly Nighttime Firearm Incidents By Proximity to\nAssembly, Restaurant or Amusement License, 2018-2022",
       subtitle= "Homicide, Aggravated Assault, Rape, Robbery, Theft - with and without Firearms.6PM-6AM.\n Source: Philadelphia Dept. Of Licenses & Inspections, Philadelphia Police Dept.",
       x="Month", 
       y="Number of Reported Incidents")+
  plotTheme

# Now see what's up with south street

# Load corridors

corridors <- st_read("https://opendata.arcgis.com/datasets/f43e5f92d34e41249e7a11f269792d11_0.geojson") %>%
  st_as_sf() %>%
  st_transform(crs = 2272)

st_join(violentCrime_shp %>% 
          filter(text_general_code %in% c("Aggravated Assault Firearm", 
                                          "Robbery Firearm", 
                                          "Homicide - Criminal", 
                                          "Homicide - Criminal " )), 
        corridors) %>%
  mutate(in_Corridor = ifelse(is.na(NAME) == TRUE, 
                                       "Outside Corridor", "Inside Corridor")) %>%
  group_by(in_Corridor, after_six, year, month) %>%
  tally() %>%
  mutate(date = lubridate::my(paste(month, year))) %>%
  filter(after_six == "6PM - 6AM") %>%
  ggplot()+
  geom_line(aes(x = date, y = n, color = in_Corridor))+
  labs(title="Monthly Nighttime Firearm Incidents Inside and Outside Commercial Corridors, 2018-2022",
       subtitle= "Homicide and Aggravated Assault, Robbery with Firearms. 6PM-6AM.\n Source: Philadelphia Dept. Of Licenses & Inspections, Philadelphia Police Dept.",
       x="Month", 
       y="Number of Reported Incidents")+
  plotTheme


# By corridor

st_join(violentCrime_shp %>% 
          filter(text_general_code %in% c("Aggravated Assault Firearm", 
                                          "Robbery Firearm", 
                                          "Homicide - Criminal", 
                                          "Homicide - Criminal " )), 
        corridors) %>%
  filter(is.na(NAME) == FALSE) %>%
  filter(str_detect(NAME, "South")) %>%
  filter(after_six == "6PM - 6AM") %>%
  group_by(NAME, year) %>%
  tally() %>%
  #mutate(date = lubridate::my(paste(month, year))) %>% 
  ggplot()+
  geom_bar(aes(x = year, y = n), stat = "identity")+
  facet_wrap(~NAME)+
  labs(title="Yearly Nighttime Firearm Incidents In Selected Commercial Corridors, 2018-2022",
       subtitle= "Homicide and Aggravated Assault, Robbery with Firearms, 6PM-6AM.\n Source: Philadelphia Dept. Of Licenses & Inspections, Philadelphia Police Dept.",
       x="Month", 
       y="Number of Reported Incidents")+
  plotTheme

# Mapping all the corridors

st_join(violentCrime_shp %>% 
          filter(text_general_code %in% c("Aggravated Assault Firearm", 
                                          "Robbery Firearm", 
                                          "Homicide - Criminal", 
                                          "Homicide - Criminal " )), 
        corridors) %>%
  filter(is.na(NAME) == FALSE) %>%
 # filter(str_detect(NAME, "South")) %>%
  filter(after_six == "6PM - 6AM") %>%
  group_by(NAME, year) %>%
  tally() %>%
  as.data.frame() %>%
  select(-geometry) %>%
  #pivot_wider(names_from = year, values_from = n) %>%
  left_join(., corridors %>%
              select(NAME), by = c("NAME")) %>%
  st_as_sf() %>%
  st_transform(2272) %>%
  ggplot()+
  geom_sf(aes(fill = n), color = "transparent")+
  facet_wrap(~year)+
  mapTheme


st_join(violentCrime_shp %>% 
          filter(text_general_code %in% c("Aggravated Assault Firearm", 
                                          "Robbery Firearm", 
                                          "Homicide - Criminal", 
                                          "Homicide - Criminal " )), 
        corridors) %>%
  filter(is.na(NAME) == FALSE) %>%
  # filter(str_detect(NAME, "South")) %>%
  filter(after_six == "6PM - 6AM") %>%
  group_by(NAME, year) %>%
  tally() %>%
  as.data.frame() %>%
  select(-geometry) %>%
  pivot_wider(names_from = year, values_from = n) %>%
  left_join(., corridors %>%
              select(NAME), by = c("NAME")) %>%
  st_as_sf() %>%
  st_transform(2272) %>%
  mapView(zcol = "2021")

# Try to pop up some plots - this isn't working yet.
# Look here - https://stackoverflow.com/questions/61686111/how-to-fix-distorted-interactive-popup-of-ggplot-figure-in-leaflet-map

plots <- st_join(violentCrime_shp %>% 
          filter(text_general_code %in% c("Aggravated Assault Firearm", 
                                          "Robbery Firearm", 
                                          "Homicide - Criminal", 
                                          "Homicide - Criminal " )), 
        corridors) %>%
  filter(is.na(NAME) == FALSE) %>%
  filter(after_six == "6PM - 6AM") %>%
  filter(str_detect(NAME, "South")) %>%
  group_by(NAME, year) %>%
  tally() %>%
  as_tibble() %>%
  mutate(data = map(NAME,
                    ~tibble(year = year, n = n))) %>%
  mutate(ggp = map2(data, NAME, 
                    ~ggplot(data = .x)+
                      geom_bar(aes(x = year, y = n), stat = "identity"))+
  labs(title="6PM-6AM Firearm Incidents",
       subtitle= "Source: Philadelphia Dept. Of Licenses & Inspections, Philadelphia Police Dept.",
       x="Month", 
       y="Number of Reported Incidents")+
  plotTheme)
