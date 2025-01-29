library(zoo)
library(ggplot2)
library(reshape2)

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
# Part 2
#-------------------------------------------------------------------------- 


vars <- c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")


c_matrix <- cor(df_Week5[, vars], method = "pearson")

#print the correlation matrix

c_matrix

# Hepler funct to reorder values
reorder_cormat <- function(cormat) {
  dd <- as.dist((1 - cormat) / 2)
  hc <- hclust(dd)  
  cormat <- cormat[hc$order, hc$order] 
  return(cormat)
}

# Funct to get rid of duplicate
get_upper_tri <- function(cormat) {
  cormat[lower.tri(cormat)] <- NA 
  return(cormat)
}

c_matrix <- reorder_cormat(c_matrix)
upper_tri <- get_upper_tri(c_matrix)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

#heatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +  
  scale_fill_gradient2(low = "#50e991", high = "#e60049", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Pearson") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal"
  ) +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

# Print heatmap
print(ggheatmap)


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


