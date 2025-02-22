#--------------------------------------------------------------------------
#                                   Q2
#--------------------------------------------------------------------------

library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)

#---------------------------- Moving Average ------------------------------

# Filter for anomolies first bassically from last as1 where row and col that has a Z-score > 3 or < -3
df_cleaned <- df_zscores %>%
  filter(!apply(select(., ends_with("_z")), 1, function(x) any(abs(x) > 3)))

# also dont need Z-score col
df_cleaned <- df_cleaned %>% select(-ends_with("_z"))

# Identify week start date for each row 
df_cleaned$weekStart <- floor_date(
  df_cleaned$DateTime,
  unit = "week",
  week_start = 1
)

# Slice dataset into complete weeks 
df_cleaned <- df_cleaned %>%
  group_by(weekStart) %>%
  filter(n_distinct(Date) == 7) %>%
  ungroup()

# Initialize smoothened_Global_intensity column
df_cleaned$smoothened_Global_intensity <- NA

window_size <- 7

# Compute moving average 
for (week in unique(df_cleaned$weekStart)) {
  # Rows for the current week
  rows <- df_cleaned$weekStart == week
  smooth_values <- rollmean(df_cleaned$Global_intensity[rows],
                            k = window_size,
                            fill = NA,
                            align = "center")
  
  df_cleaned$smoothened_Global_intensity[rows] <- smooth_values
}

# Each smoothened week is a dataframe 
smoothened_weeks <- split(df_cleaned, df_cleaned$weekStart)

# Rename the weeks from: smoothened_week_1 to smoothened_week_n
names(smoothened_weeks) <- paste0(
  "smoothened_week_",
  seq_along(smoothened_weeks))

#------------------------- Average Smoothened Week ------------------------

# Time series from the average Smoothed week 
average_smoothened_week <- df_cleaned %>%
  group_by(Time = format(DateTime, "%H:%M:%S")) %>%
  summarise(average_intensity = mean(smoothened_Global_intensity, na.rm = TRUE))

print(average_smoothened_week)

#----------------------- Most & Least Anomalous Weeks ---------------------

# Function to compute Mean Absolute Deviation (MAD)
mad_score <- function(actual, predicted) {
  mean(abs(actual - predicted), na.rm = TRUE)
}

# Store into a data point 
anomaly_scores <- data.frame(Week = names(smoothened_weeks), Score = NA)

# Compute the score for each week
for (i in seq_along(smoothened_weeks)) {
  week_data <- smoothened_weeks[[i]]
  
  # Merged both with similar 'Time'
  merged_data <- merge(week_data, average_smoothened_week, by="Time")
  
  # than compute mad score
  anomaly_scores$Score[i] <- mad_score(merged_data$smoothened_Global_intensity, merged_data$average_intensity)
}

# Get least and most anomalous_week
most_anomalous_week <- anomaly_scores[which.max(anomaly_scores$Score), "Week"]
least_anomalous_week <- anomaly_scores[which.min(anomaly_scores$Score), "Week"]

# Results
print(anomaly_scores)
cat("Most Anomalous Week:", most_anomalous_week, "\n")
cat("Least Anomalous Week:", least_anomalous_week, "\n")

#----------------------- Visualization ---------------------

# Convert to POSIXct 
average_smoothened_week$Time <- as.POSIXct(average_smoothened_week$Time, format="%H:%M:%S", tz="UTC")
most_anomalous_data$Time <- as.POSIXct(most_anomalous_data$Time, format="%H:%M:%S", tz="UTC")
least_anomalous_data$Time <- as.POSIXct(least_anomalous_data$Time, format="%H:%M:%S", tz="UTC")

# Plot the data
ggplot() +
  geom_line(data=most_anomalous_data, aes(x=Time, y=smoothened_Global_intensity, color="Most Anomalous Week"), size=1.2, linetype="solid") + 
  geom_line(data=least_anomalous_data, aes(x=Time, y=smoothened_Global_intensity, color="Least Anomalous Week"), size=1.2, linetype="dotted") + 
  geom_line(data=average_smoothened_week, aes(x=Time, y=average_intensity, color="Average Week"), size=1.5) + 
  labs(
    title="Comparison of Smoothened Weeks",
    x="Time of Day",
    y="Global Intensity",
    color="Week Type"
  ) +
  scale_color_manual(values=c("blue", "red", "green")) +
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "top", 
    axis.text.x = element_text(angle = 45, hjust = 1),  
    panel.grid.major = element_line(size = 0.5, linetype = "solid", color = "gray80"), 
    panel.grid.minor = element_blank() 
  ) +
  scale_x_datetime(labels = scales::time_format("%H:%M"), breaks = "2 hours")
