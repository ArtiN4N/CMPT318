library(zoo)

#--------------------------------------------------------------------------
#                                 Part 1
#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
# (i) Linear Interpolation
#--------------------------------------------------------------------------

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
end_date   <- as.POSIXct("5/2/2007 00:00:00",  format = "%d/%m/%Y %H:%M:%S")

df_Week5 <- subset(df_zscores, DateTime >= start_date & DateTime <= end_date)
