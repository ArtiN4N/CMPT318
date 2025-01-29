library(zoo)
library(ggplot2)

#--------------------------------------------------------------------------
#                                 Part 1
#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
# (i) Linear Interpolation
#--------------------------------------------------------------------------

Group_Assignment_Dataset <- read.csv("Group_Assignment_Dataset.txt")

numeric_cols <- names(Group_Assignment_Dataset)[sapply(Group_Assignment_Dataset, is.numeric)]

df_interpolated <- Group_Assignment_Dataset
cat("Number of missing values per column before interpolation:\n")
print(colSums(is.na(df_interpolated)))

for (col in numeric_cols) {
  df_interpolated[[col]] <- na.approx(df_interpolated[[col]], rule = 2)
}

cat("Number of missing values per column after interpolation:\n")
print(colSums(is.na(df_interpolated)))

#--------------------------------------------------------------------------
# (ii) Point anomalies
#-------------------------------------------------------------------------- 

# Calculate Z-scores

df_zscores <- df_interpolated

for (col in numeric_cols) {
  col_mean <- mean(df_interpolated[[col]])
  col_stdev <- sd(df_interpolated[[col]])
  col_zscore <- (df_interpolated[[col]] - col_mean) / col_stdev 
  
  # Store results in new columns 
  new_col <- paste0(col, "_z")
  df_zscores[[new_col]] <- col_zscore
}

# Track anomalies in a data frame

df_anomalies <- data.frame(
  feature = character(),
  num_anomalies = numeric(),
  total_points = numeric(),
  percent_anomalies = numeric()
)

overall_anomalies <- 0
overall_points <- 0

for (col in numeric_cols) {
  # Access results from z-scores 
  z_col <- paste0(col, "_z")
  z_vals <- df_zscores[[z_col]]
  
  is_anomaly <- abs(z_vals) > 3 # Boolean value tracks if point is anomaly
  num_anomalies <- sum(is_anomaly)
  total_points  <- length(z_vals)
  
  # Anomalies in the column = # anomalies / # points * 100
  percent_anomalies <- (num_anomalies / total_points) * 100
  
  # Store results 
  df_anomalies <- rbind(
    df_anomalies,
    data.frame(
      feature = col,
      num_anomalies = num_anomalies,
      total_points = total_points,
      percent_anomalies = percent_anomalies
    )
  )
  
  # Aggregate total results 
  overall_anomalies <- overall_anomalies + num_anomalies
  overall_points    <- overall_points + total_points
}

overall_percent <- (overall_anomalies / overall_points) * 100

cat("\nAnomaly summary per feature:\n")
print(df_anomalies)

cat("\nOverall anomaly summary:\n")
cat(round(overall_percent, 2), "%\n")

#--------------------------------------------------------------------------
# (iii) Extracting data for Week 5
#-------------------------------------------------------------------------- 

df_zscores$DateTime <- as.POSIXct(
  paste(df_zscores$Date, df_zscores$Time),
  format = "%d/%m/%Y %H:%M:%S"
)

start_date <- as.POSIXct("29/1/2007 00:00:00", format = "%d/%m/%Y %H:%M:%S")
end_date   <- as.POSIXct("4/2/2007 00:00:00",  format = "%d/%m/%Y %H:%M:%S")

df_Week5 <- subset(df_zscores, DateTime >= start_date & DateTime <= end_date)
print(start_date)

#--------------------------------------------------------------------------
#                                 Part 2
#--------------------------------------------------------------------------

# this is the empty correlation matrix we will fill
mat <- matrix(0, nrow = 7, ncol = 7)

# working with vectors to make things easy
numeric_vectors <- lapply(numeric_cols, function(col) Group_Assignment_Dataset[[col]])

#
for (col1 in 1:6) {
  for (col2 in (col1 + 1):7) {
    if (col == col2) {
      break
    }

    set1 <- numeric_vectors[[col1]]
    set2 <- numeric_vectors[[col2]]

    # from given function in a1 description
    # use = pairwise.complete.obs will ignore empty positions in the data
    corel <- cor(set1, set2, method = "pearson", use = "pairwise.complete.obs")
    mat[col1, col2] <- corel
  }
}



# putting our matrix into a data fram for ggplot
matdf <- expand.grid(x = seq_len(nrow(mat)), y = seq_len(ncol(mat)))
matdf$value <- as.vector(mat)

# ggplot to create a heatmap visual of the correlation matrix
ggplot(matdf, aes(x = x, y = y, fill = value)) +
geom_tile() +
scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
labs(title = "Visual of the corellation matrix of responses")


#--------------------------------------------------------------------------
#                                 Part 3
#--------------------------------------------------------------------------


Group_Assignment_Dataset$DateTime <- as.POSIXct(paste(Group_Assignment_Dataset$Date
                                                      ,Group_Assignment_Dataset$Time),
                                                format="%d/%m/%Y %H:%M:%S")
# adding hour and day rows to the data set 
# doing this because it is easier to filter the data into different 
# categories
Group_Assignment_Dataset$Hour <- format(Group_Assignment_Dataset$DateTime, "%H:%M")
Group_Assignment_Dataset$Day <- weekdays(Group_Assignment_Dataset$DateTime)  
Group_Assignment_Dataset$Hour <- as.POSIXct(Group_Assignment_Dataset$DateTime, format = "%H:%M")
str(Group_Assignment_Dataset)

#print(Group_Assignment_Dataset)
# subsetting the data into day time hours and night time hours
# using the hours specified in the assignment description
day_time_window <- Group_Assignment_Dataset[Group_Assignment_Dataset$Time >= "07:30:00"
                                            & Group_Assignment_Dataset$Time <= "17:00:00",]
night_time_window <- Group_Assignment_Dataset[!(Group_Assignment_Dataset$Time >= "07:30:00" 
                            & Group_Assignment_Dataset$Time <= "17:00:00"), ]
day_time_window$Time
# subsetting the data based on weekday vs weekend and time window
# because the assignment made a distinction when talking about weekday and weekend
# NOTE: I'm not actually sure if we're supposed to make a distinction between
# weekdays and weekends
weekdays_data_day <- day_time_window[day_time_window$Day 
                                     %in% c("Monday", "Tuesday", "Wednesday", 
                                                    "Thursday", "Friday"), ]
weekends_data_day <- day_time_window[day_time_window$Day 
                                     %in% c("Saturday","Sunday"), ]

weekdays_data_night <- night_time_window[night_time_window$Day 
                                     %in% c("Monday", "Tuesday", "Wednesday", 
                                                    "Thursday", "Friday"), ]

weekends_data_night <- night_time_window[night_time_window$Day 
                                         %in% c("Saturday","Sunday"),]


# testing to see if I filtered the data correctly
#print(Group_Assignment_Dataset)
#print(day_time_window)
#print(night_time_window)
#print(weekdays_data_day)
#print(weekends_data_day)
#print(weekdays_data_night)
#print(weekends_data_night)

table(weekends_data_night$Day)
table(weekends_data_day$Day)
table(weekdays_data_night$Day)
table(weekdays_data_day$Day)

# computing the averages, each time(Hour) with the same value, is grouped together
weekdays_avg_day <- aggregate(Global_intensity ~ Hour, data = weekdays_data_day, FUN = mean)
weekends_avg_day <- aggregate(Global_intensity ~ Hour, data = weekends_data_day, FUN = mean)

weekdays_avg_night <- aggregate(Global_intensity ~ Hour, data = weekdays_data_night, FUN = mean)
weekends_avg_night <- aggregate(Global_intensity ~ Hour, data = weekends_data_night, FUN = mean)


# fitting the models
# the time has to be converted to seconds cause it's not a numeric type on its own
# The seconds returned is the seconds after epoch (Jan 1st, 1970)
fit1 <- lm(Global_intensity ~ as.numeric(as.POSIXct(Hour, format="%H:%M")), data = weekdays_avg_day)
fit2 <- lm(Global_intensity ~ as.numeric(as.POSIXct(Hour, format="%H:%M")), data = weekends_avg_day)
fit3 <- lm(Global_intensity ~ as.numeric(as.POSIXct(Hour, format="%H:%M")), data = weekdays_avg_night)
fit4 <- lm(Global_intensity ~ as.numeric(as.POSIXct(Hour, format="%H:%M")), data = weekdays_avg_night)
polyfit1 = lm(Global_intensity ~ poly(as.numeric(as.POSIXct(Hour, format="%H:%M")), 2, raw=TRUE), data = weekdays_avg_day)
polyfit2 = lm(Global_intensity ~ poly(as.numeric(as.POSIXct(Hour, format="%H:%M")), 2, raw=TRUE), data = weekends_avg_day)
polyfit3 = lm(Global_intensity ~ poly(as.numeric(as.POSIXct(Hour, format="%H:%M")), 2, raw=TRUE), data = weekdays_avg_night)
polyfit4 = lm(Global_intensity ~ poly(as.numeric(as.POSIXct(Hour, format="%H:%M")), 2, raw=TRUE), data = weekends_avg_night)



#trying to plot
#I don't believe this is correct
ggplot() +
  geom_smooth(data = weekdays_avg_day, 
              aes(x = as.POSIXct(Hour, format="%H:%M"), 
              y = Global_intensity),method = "lm", 
              formula = y ~ x, se = FALSE, color = "red") +
  geom_smooth(data = weekends_avg_day, 
              aes(x = as.POSIXct(Hour, format="%H:%M"), 
                  y = Global_intensity),method = "lm", 
              formula = y ~ x, se = FALSE, color = "blue") + 
  geom_smooth(data = weekdays_avg_night, 
              aes(x = as.POSIXct(Hour, format="%H:%M"), 
                  y = Global_intensity),method = "lm", 
              formula = y ~ x, se = FALSE, color = "green") +
  geom_smooth(data = weekends_avg_night, 
              aes(x = as.POSIXct(Hour, format="%H:%M"), 
                  y = Global_intensity),method = "lm", 
              formula = y ~ x, se = FALSE, color = "yellow") +
  scale_x_datetime(
    date_breaks = "1 hour",
    date_labels = "%H:%M"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels by 45 degrees
  )


ggplot() +
  geom_smooth(data = weekdays_avg_day, 
              aes(x = as.POSIXct(Hour, format="%H:%M"), 
                  y = Global_intensity),method = "lm", 
              formula = y ~ poly(x,2,raw = TRUE), se = FALSE, color = "red") +
  geom_smooth(data = weekends_avg_day, 
              aes(x = as.POSIXct(Hour, format="%H:%M"), 
                  y = Global_intensity),method = "lm", 
              formula = y ~ poly(x,2,raw = TRUE), se = FALSE, color = "blue") + 
  geom_smooth(data = weekdays_avg_night, 
              aes(x = as.POSIXct(Hour, format="%H:%M"), 
                  y = Global_intensity),method = "lm", 
              formula = y ~ poly(x,2,raw = TRUE), se = FALSE, color = "green") +
  geom_smooth(data = weekends_avg_night, 
              aes(x = as.POSIXct(Hour, format="%H:%M"), 
                  y = Global_intensity),method = "lm", 
              formula = y ~ poly(x,2,raw = TRUE), se = FALSE, color = "yellow") +
  scale_x_datetime(
    date_breaks = "1 hour",
    date_labels = "%H:%M"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels by 45 degrees
  )


