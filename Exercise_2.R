# 1 Task: Load data

library(readr) 
library(dplyr) 
library(ggplot2)
library(sf)
library(terra)
#library(lubridate) 
library(tmap) 

# Import the downloaded csv 
wildschwein_BE <- read_delim("data/wildschwein_BE_2056.csv", ",")
# format to spatial object
wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE) 

# Task 2 

# calc time difference
wildschwein_BE <- wildschwein_BE |> 
  group_by(TierID) |> 
  mutate(timelag_sec = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs"))) 

# 1. How many individuals were tracked?
wildschwein_BE$TierName |> unique() 
# 3 individuals

# 2. For how long were the individuals tracked? Are there gaps?

wildschwein_BE |> 
  select(TierName, DatetimeUTC)|>
  group_by(TierName) |>       
  summarise(DateTime_min = min(DatetimeUTC, na.rm = TRUE),
            DateTime_max = max(DatetimeUTC, na.rm = TRUE)) 
  
#TierName DateTime_min        DateTime_max       
#<chr>    <dttm>              <dttm>             
#  1 Rosa     2014-11-07 07:45:44 2015-06-29 23:45:11
#2 Ruth     2014-11-07 18:00:43 2015-07-27 09:45:15
#3 Sabi     2014-08-22 21:00:12 2015-07-27 11:00:14

#No gaps


# 3. Were all individuals tracked concurrently or sequentially?

wildschwein_BE |>
  filter(DatetimeUTC < "2014-08-24") |>
  ggplot(aes(DatetimeUTC, timelag_sec, col = TierID)) +
  geom_point()


ggplot(wildschwein_BE, aes(timelag_sec/60)) +
  geom_histogram(binwidth = 1) +
  lims(x = c(0, 5000/60)) +
  scale_y_log10()
# the sampling intervals were between 5-75 min. peak around 15 min with outliers above and under 

# Task 3 Deriving movement parameters I: Speed
# calculate steplength and speed
wildschwein_BE <- wildschwein_BE |>
  group_by(TierID) |>
  mutate(steplength_m = sqrt((E - lead(E))^2 + (N - lead(N))^2))|>
  mutate(speed_ms = steplength_m/timelag_sec)


hist(log10(wildschwein_BE$speed_ms), 100)


# Task 4: Cross-scale movement analysis

# Import data
caro <- read_delim("data/caro60.csv")
# format
caro <- st_as_sf(caro, coords = c("E", "N"), crs = 2056, remove = FALSE) 
# 1. create sequences
seq_3 <- seq(from = 1, to = 200, by = 3)
seq_6 <- seq(from = 1, to = 200, by = 6)
seq_9 <- seq(from = 1, to = 200, by = 9)
# 2. slice the data
caro_3 <- dplyr::slice(caro, seq_3)
caro_6 <- dplyr::slice(caro, seq_6)
caro_9 <- dplyr::slice(caro, seq_9)
# check 
nrow(caro)
## [1] 200
nrow(caro_3)
## [1] 67
nrow(caro_6)
## [1] 34
nrow(caro_9)
## [1] 23
# Now calculate timelag, steplength and speed for these data sets (without reduced granularity, with 3, 6, 9), just as you did in the last task
caro <- caro |> 
  group_by(TierID) |> 
  mutate(timelag_sec = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")),
         steplength_m = sqrt((E - lead(E))^2 + (N - lead(N))^2),
         speed_ms = steplength_m/timelag_sec)
caro_3 <- caro_3 |> 
  group_by(TierID) |> 
  mutate(timelag_sec = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")),
         steplength_m = sqrt((E - lead(E))^2 + (N - lead(N))^2),
         speed_ms = steplength_m/timelag_sec)
caro_6 <- caro_6 |> 
  group_by(TierID) |> 
  mutate(timelag_sec = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")),
         steplength_m = sqrt((E - lead(E))^2 + (N - lead(N))^2),
         speed_ms = steplength_m/timelag_sec)
caro_9 <- caro_9 |> 
  group_by(TierID) |> 
  mutate(timelag_sec = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")),
         steplength_m = sqrt((E - lead(E))^2 + (N - lead(N))^2),
         speed_ms = steplength_m/timelag_sec)
# comparing original with 3, 6 or 9 minutes resamples data 
# Interpret the line plot, what do the different lines for the different temporal granularities tell you?
ggplot() +
  geom_path(data = caro, aes(E, N, color = "caro")) +
  geom_path(data = caro_3, aes(E, N, color = "caro_3")) +
  theme_minimal() +
  scale_color_manual(name = "Tracetory",
                     values = c("caro" = "violet", "caro_3" = "lightblue"), 
                     labels = c("1 minute", "3 minutes")) +
  labs(title = "Comparing original- with 3 minutes-resampled data")

# Compare original with 6 min. 
ggplot() +
  geom_path(data = caro, aes(E, N, color = "caro")) +
  geom_path(data = caro_6, aes(E, N, color = "caro_6")) +
  theme_minimal() +
  scale_color_manual(name = "Tracetory",
                     values = c("caro" = "violet", "caro_6" = "lightblue"), 
                     labels = c("1 minute", "6 minutes")) +
  labs(title = "Comparing original- with 6 minutes-resampled data")
# Compare original with 9 min. 
ggplot() +
  geom_path(data = caro, aes(E, N, color = "caro")) +
  geom_path(data = caro_9, aes(E, N, color = "caro_9")) +
  theme_minimal() +
  scale_color_manual(name = "Tracetory",
                     values = c("caro" = "violet", "caro_9" = "lightblue"), 
                     labels = c("1 minute", "9 minutes")) +
  labs(title = "Comparing original- with 9 minutes-resampled data")
# Comparing derived speed at different sampling intervalls 
ggplot() +
  geom_line(data = caro, aes(DatetimeUTC, speed_ms, color = "caro")) +
  geom_line(data = caro_3, aes(DatetimeUTC, speed_ms, color = "caro_3")) +
  geom_line(data = caro_6, aes(DatetimeUTC, speed_ms, color = "caro_6")) +
  geom_line(data = caro_9, aes(DatetimeUTC, speed_ms, color = "caro_9")) +
  theme_minimal() +
  scale_color_manual(name = "Sampling Intervals",
                     values = c("caro" = "red", "caro_3" = "green", "caro_6" = "lightblue", "caro_9" = "violet"), 
                     labels = c("1 minute", "3 minutes", "6 minutes", "9 minutes")) +
  labs(title = "Comparing derived speed at different sampling intervals", x = "Time", y = "Speed (m/s)")

# With higher time intervalls we are smoothing out the volatility in the movement which helps to understand the pattern. 


# Task 6 and 7: Add your movement data to your respitory and explore your morvement data

# import data 
posmo <- read_delim("data/posmo_current_data.csv", ",") 
# to convert it to a spatial object
posmo <- st_as_sf(posmo, coords = c("lon_x", "lat_y"), crs = 4326, remove = FALSE) 
# Convert data to CH1903+ LV95
st_transform(posmo, crs = 2056)
# Make a map of your data using ggplot2 or tmap.
tmap_mode("view")
tm_shape(posmo) +
  tm_bubbles(col = "transport_mode")




