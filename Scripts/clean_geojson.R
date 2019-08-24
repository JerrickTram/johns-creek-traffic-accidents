library(lubridate)

# Change to proper datetime format
df$DateTimeOccurred = as_datetime(df$DateTimeOccurred)

# Extract time and fix all caps
df = df %>% 
  mutate(TimeOccurred = format(strptime(DateTimeOccurred, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S"),
         LightingCondition = str_to_title(LightingCondition),
         WeatherCondition = str_to_title(WeatherCondition),
         AddressOrStreetName = str_to_title(AddressOrStreetName),
         TrafficFlow = str_to_title(TrafficFlow))  