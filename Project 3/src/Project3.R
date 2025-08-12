library(ggplot2)
library(dplyr)
library(readr)
library(ggmap)

data <- read_csv("/Users/melanie/Desktop/gsa_properties_2025-03-05.csv")

city_counts <- data %>%
  group_by(City, State) %>%
  summarize(Count = n(), .groups = 'drop')

register_google(key = "your_api_key_here")

data_with_coords <- city_counts %>%
  mutate(full_address = paste(City, State)) %>%
  rowwise() %>%
  mutate(
    geocode_result = list(geocode(full_address)),
    latitude = geocode_result[[1]]$lat,
    longitude = geocode_result[[1]]$lon
  )

ggplot(city_counts, aes(x = longitude, y = latitude, size = count)) +
  geom_point(alpha = 0.7) + 
  scale_size_continuous(range = c(2, 10)) + # Adjust size range as necessary
  theme_minimal() +
  labs(title = "City Occurrences by Size",
       subtitle = "Larger points indicate more occurrences")
