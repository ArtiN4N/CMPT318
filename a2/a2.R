#--------------------------------------------------------------------------
#                                   Q2
#--------------------------------------------------------------------------

library(dplyr)
library(lubridate)
library(zoo)

data <- df_zscores  # Store preprocessed data 
data$weekStart <- floor_date(data$DateTime, unit = "week", week_start = 1)
window_size = 10

# Moving average 