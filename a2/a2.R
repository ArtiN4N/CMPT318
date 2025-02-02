#--------------------------------------------------------------------------
#                                   Q2
#--------------------------------------------------------------------------

library(dplyr)
library(lubridate)
library(zoo)

#---------------------------- Moving Average ------------------------------

df_smoothened <- df_zscores

# Identify week start date for each row 
df_smoothened$weekStart <- floor_date(
  df_smoothened$DateTime,
  unit = "week",
  week_start = 1)

# Slice dataset into complete weeks 
df_smoothened <- df_smoothened %>%
  group_by(weekStart) %>%
  filter(n_distinct(Date) == 7) %>%
  ungroup()

# Initialize smoothened_Global_intensity column
df_smoothened$smoothened_Global_intensity <- NA

window_size <- 7

# Compute moving average 
for (week in unique(df_smoothened$weekStart)) {
  # Rows for the current week
  rows <- df_smoothened$weekStart == week
  smooth_values <- rollmean(df_smoothened$Global_intensity[rows],
                          k = window_size,
                          fill = NA,
                          align = "center")
  
  df_smoothened$smoothened_Global_intensity[rows] <- smooth_values
}

# Each smoothened week is a dataframe 
smoothened_weeks <- split(df_smoothened, data$weekStart)

# Rename the weeks from: smoothened_week_1 to smoothened_week_n
names(smoothened_weeks) <- paste0(
  "smoothened_week_",
  seq_along(smoothened_weeks))

#------------------------- Average Smoothened Week ------------------------

#----------------------- Most & Least Anomalous Weeks ---------------------
