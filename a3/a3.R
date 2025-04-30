library(depmixS4)
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)

#-------------------------------------------------------
#  Question 1
#-------------------------------------------------------

# ------ Choose a time window & filter ------ #
chosen_day_of_week <- 2  # 1=Monday, 2=Tuesday, 3=Wednesday, ...
start_hour <- 12
end_hour   <- 18

# Filter data for that day-of-week + hour range
df_window <- df_cleaned %>%
  filter(
    wday(DateTime) == chosen_day_of_week,
    hour(DateTime) >= start_hour & hour(DateTime) < end_hour
  )

# Get number of rows per week based on week start date
counts_by_week <- df_window %>%
  group_by(weekStart) %>%
  summarise(nrows = n())

# Extract number of rows for each week into a vector 
ntimes_vec <- counts_by_week$nrows

# ------ HMM's ------ #

# State range for models 
state_range <- 4:16

# Data frame to store results
results <- data.frame(
  numStates = integer(),
  logLik    = numeric(),
  BIC       = numeric(),
  stringsAsFactors = FALSE
)

for (ns in state_range) {
  # Build the HMM model
  model <- depmix(
    response = Global_active_power ~ 1,
    data     = df_window,
    nstates  = ns,
    ntimes   = ntimes_vec
  )
  
  # Fit the model
  fit_model <- fit(model, verbose = FALSE)
  
  # Get log-likelihood and BIC values 
  ll  <- logLik(fit_model)
  bic <- BIC(fit_model)
  
  # Save
  results <- rbind(
    results,
    data.frame(
      numStates = ns,
      logLik = as.numeric(ll),
      BIC = bic
    )
  )
}

# ------ Inspect results ------ #

print(results)
summary(fit_model)

# Plot -> numStates vs. LL
plot(results$numStates, results$logLik,
     type = "b", pch = 19,
     xlab = "Number of States",
     ylab = "Log-Likelihood",
     main = "States vs. log-likelihood"
)

# Plot -> numStates vs. BIC
plot(results$numStates, results$BIC,
     type = "b", pch = 19, col = "red",
     xlab = "Number of States",
     ylab = "BIC",
     main = "States vs. BIC"
)
#-------------------------------------------------------
#  Question 2
#-------------------------------------------------------

# Rounding the global active power variable to the nearest half or whole number
df_round <- df_window
# must multiply by 2 and then divide by 2 because otherwise we wouldn't be able
# to round the the half integer case
df_round$Global_active_power <- round(df_round$Global_active_power * 2) / 2

