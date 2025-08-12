# STEP 0
library(tidyverse)
library(leaflet)
library(tidycensus)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(lubridate)
library(usmap)
library(sf)
theme_set(theme_bw())

# STEP 1
wapo.data <- read.csv("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/v2/fatal-police-shootings-data.csv", 
                      stringsAsFactors = FALSE)

# STEP 2
wapo.data$date <- as.Date(wapo.data$date)
wapo.data$month <- substr(wapo.data$date, 6, 7)
wapo.data$year <- substr(wapo.data$date, 0, 4)
wapo.data$yearmonth <- paste(wapo.data$year, wapo.data$month, sep = "-")
wapo.data$statecity <- paste(wapo.data$state, wapo.data$city, sep = "-")

unique(wapo.data$race)
# Option 1: leave them as is
# Option 2: label them all as Other
# Option 3: take the first value
# Option 4: all races as binary variables
wapo.data$race <- gsub("W;B;N", "O", wapo.data$race)
wapo.data$race <- gsub("N;H", "O", wapo.data$race)
wapo.data$race <- gsub("W;H", "O", wapo.data$race)
wapo.data$race <- gsub("B;H", "O", wapo.data$race)
wapo.data$race <- gsub("W;B", "O", wapo.data$race)
wapo.data$race <- gsub("W;A", "O", wapo.data$race)
unique(wapo.data$race)

wapo.data.map <- subset(wapo.data, !is.na(wapo.data$latitude))

# STEP 3
sum.race <- wapo.data %>%
  group_by(race) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

sum.city <- wapo.data %>%
  group_by(statecity) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

sum.year <- wapo.data %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

sum.race.mental <- wapo.data %>%
  group_by(race, was_mental_illness_related) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

sum.city.race <- wapo.data %>%
  group_by(statecity, race) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

sum.city.year <- wapo.data %>%
  group_by(statecity, year) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

# STEP 4
ggplot(wapo.data) +
  geom_bar(aes(x = race), stat = "count", fill = "blue")

ggplot(sum.race.mental, aes(x = factor(race), y = pct, 
                            fill = factor(was_mental_illness_related))) + 
  geom_bar(stat="identity", width = 0.7) + 
  labs(title = "Police Shootings by Race and Mental Illness",
       x = "Race", y = "Percent", fill = "Mental Illness Related") + 
  theme_minimal(base_size = 14)

ggplot(wapo.data) + 
  geom_bar(aes(x = was_mental_illness_related), stat = "count", fill = "orange") + 
  facet_wrap(~ race, nrow = 3)

sum.city.armed <- wapo.data %>%
  group_by(statecity, armed_with) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

ggplot(sum.city.armed, aes(x = statecity, y = count, fill = factor(armed_with))) + 
  geom_bar (stat = "identity") +
  labs(x = "State-City", y = "Number of Police Shootings", 
       title = "Police Shootings by Victim Weapon Type", subtitle = "XXX", 
       fill = "Victims Weapon") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### example
# creates the summary table
sum.city.armed <- wapo.data %>%
  group_by(statecity, armed_with) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

# clean/filter select weapons based on DC's values
city.weapon <- subset(sum.city.armed, 
                           armed_with %in% c("replica", "gun", "unarmed",
                                             "knife","vehicle"))

# filter to select cities based on DC's overall count
wapo.city.select <- subset(city.weapon, statecity %in% 
                             c("DC-Washington","FL-Orlando","MI-Detroit","MD-Baltimore", 
                               "TN-Memphis","NC-Charlotte","OR-Portland"))

# graph
ggplot(wapo.city.select, aes(x = statecity, y = count, fill = factor(armed_with))) + 
  geom_bar (stat = "identity") +
  labs(x = "City", y = "Number of Police Shootings", 
       title = "Police Shootings by Victim Weapon Type", 
       subtitle = "xxx", fill = "Victims Weapon") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### maps
world <- ne_countries(scale = "medium", returnclass = "sf") # this builds a list of countries
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) # this cleans up the US states

# transparent points
ggplot(data = world) + 
  geom_sf(data = states, fill = NA) + 
  geom_point(data = wapo.data.map, aes(x = longitude, y = latitude), 
             size = 2, alpha = 0.025) + 
  coord_sf(xlim = c(-130, -65), ylim = c(25, 50), expand = FALSE)

ggplot(data = world) + 
  geom_sf(data = states, fill = NA) + 
  geom_point(data = wapo.data.map, aes(x = longitude, y = latitude), 
             size = 2, alpha = 0.025) + 
  coord_sf(xlim = c(-130, -65), ylim = c(25, 50), expand = FALSE) +
  facet_wrap(~race, nrow = 3)

leaflet(wapo.data.map) %>%
  addTiles() %>%
  addMarkers(lng = ~longitude, lat = ~latitude, clusterOptions = markerClusterOptions())



