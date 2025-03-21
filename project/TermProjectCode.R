#-----------------------------------------------------------------------------#
#                              Term Project                                   #
#-----------------------------------------------------------------------------#

# ---- Part 1: Feature Scaling ---- #

library(zoo)
library(ggbiplot)

# Import text dataset and convert all columns except date and time to numeric
df <- read.table("TermProjectData.txt", sep = ",", header = TRUE, stringsAsFactors = FALSE)

str(df)

df_processed <- df
df_processed[,3:9] <- lapply(df[,3:9], as.numeric)

# Linearly interpolate values to handle NAs in numerical columns
df_processed[,3:9] <- lapply(df_processed[,3:9], function(x) {
  na.approx(x, rule = 2)
})

# Z-score standardization for each numerical column
df_scaled <- df_processed
for(col in 3:9) {
  col_mean <- mean(df_processed[,col])  # Calculate column mean
  col_sd <- sd(df_processed[,col])      # Calculate column standard deviation
  df_scaled[,col] <- (df_processed[,col] - col_mean) / col_sd # Apply formula
}

# Verify that mean = 0 (or very close) and standard deviation = 1 for each numerical column
colMeans(df_scaled[,3:9])
apply(df_scaled[,3:9], 2, sd)

# Verify that all NA values are removed
print("Number of NA values in columns:")
print(colSums(is.na(df_scaled)))


# ---- Part 2: Principal Component Analysis ---- #

# Run PCA
numerical_data = df_scaled[3:9]
pca_result <- prcomp(numerical_data, scale = FALSE)  # data is already scaled
summary(pca_result)

# Create biplot
ggbiplot(pca_result,
         obs.scale = 1,
         var.scale = 2,        # arrow scale
         circle = TRUE,
         varname.size = 4,     # variable name size
         varname.adjust = 1.5, # label position
         alpha = 0.1,          # points transparent to see patterns better
         varname.color = "blue") + # variable text color is blue for visibility
  theme_minimal() +
  ggtitle("PCA of Power Consumption Data")

# ---- Part 3---- #


library(depmixS4)   


# Create a DateTime column (POSIXct). 
df_scaled$DateTime <- as.POSIXct(
  paste(df_scaled$Date, df_scaled$Time),
  format = "%Y-%m-%d %H:%M:%S"
)

# Extract year for splitting. 
cat("Extracting Year from DateTime...\n")
df_scaled$Year <- as.numeric(format(df_scaled$DateTime, "%Y"))

# Split into train vs. test (Years 2006, 2007, 2008 -> train; 2009 -> test).
cat("Splitting data into train (2006-2008) and test (2009)...\n")
train_data <- subset(df_scaled, Year %in% c(2006, 2007, 2008))
test_data  <- subset(df_scaled, Year == 2009)

cat("Train rows:", nrow(train_data), "\n")
cat("Test  rows:", nrow(test_data),  "\n")

# Subset by day of week & time window. I am using Friday from 6am-10am. 
train_data$Weekday <- weekdays(train_data$DateTime)
test_data$Weekday  <- weekdays(test_data$DateTime)

train_sub <- subset(train_data,
                    Weekday == "Friday" &
                      format(DateTime, "%H:%M:%S") >= "06:00:00" &
                      format(DateTime, "%H:%M:%S") <= "10:00:00"
)
test_sub  <- subset(test_data,
                    Weekday == "Friday" &
                      format(DateTime, "%H:%M:%S") >= "06:00:00" &
                      format(DateTime, "%H:%M:%S") <= "10:00:00"
)

cat("train_sub rows:", nrow(train_sub), "\n")
cat("test_sub  rows:", nrow(test_sub),  "\n")

# Select PCA-based features from Part 2. 
cat("\nSelecting PCA-based numeric columns...\n")
train_mat <- train_sub[, c("Global_active_power", "Voltage", "Sub_metering_1")]
test_mat  <- test_sub[,  c("Global_active_power", "Voltage", "Sub_metering_1")]

cat("Check for NAs in train_mat:\n")
print(colSums(is.na(train_mat)))
cat("Check for NAs in test_mat:\n")
print(colSums(is.na(test_mat)))

cat("\nDimensions of train_mat:\n")
print(dim(train_mat))
cat("Dimensions of test_mat:\n")
print(dim(test_mat))

# Train multiple HMMs with different state numbers. 
cat("\nTraining multiple HMMs with depmixS4...\n")

possible_states <- c(4, 5, 6, 8, 10, 12, 15, 20)
results_list <- list()

for (nst in possible_states) {
  cat("Fitting HMM with", nst, "states...\n")
  
  # Define the model spec. 
  # ~1 means just an intercept for each feature
  mod_spec <- depmix(
    list(train_mat[,1] ~ 1,
         train_mat[,2] ~ 1,
         train_mat[,3] ~ 1),
    data    = train_mat,
    nstates = nst,
    family  = list(gaussian(), gaussian(), gaussian()),
    ntimes  = nrow(train_mat)   # single contiguous sequence
  )
  
  # Fit the model
  set.seed(123)
  mod_fit <- fit(mod_spec, verbose = FALSE)
  
  # Extract logLik and BIC
  ll_val  <- logLik(mod_fit)
  bic_val <- BIC(mod_fit)
  
  cat("   logLik:", ll_val, " BIC:", bic_val, "\n")
  
  #Store results
  results_list[[as.character(nst)]] <- list(
    nstates = nst,
    model   = mod_fit,
    logLik  = ll_val,
    BIC     = bic_val
  )
}

# Pick the best model by scanning logLik & BIC 
cat("\nComparison of models:\n")
for (nst in names(results_list)) {
  cat("Num States:", nst,
      "| logLik:", results_list[[nst]]$logLik,
      "| BIC:",    results_list[[nst]]$BIC, "\n")
}

# From the results best model is with 6 states:
best_num_states <- 6  # Adjust best fit (we can mess aorund to see)
best_model <- results_list[[as.character(best_num_states)]]$model

cat("Chosen best_num_states =", best_num_states, "\n")

# Evaluate the chosen best model on the Test set. 
# forwardbackward to compute test logLik.
cat("\nEvaluating on the test data...\n")

best_params <- getpars(best_model)

test_spec <- depmix(
  list(
    test_mat[,1] ~ 1,
    test_mat[,2] ~ 1,
    test_mat[,3] ~ 1
  ),
  data    = test_mat,
  nstates = best_num_states,
  family  = list(gaussian(), gaussian(), gaussian()),
  ntimes  = nrow(test_mat)
)

# Impose the trained parameters onto the test_spec
test_model <- setpars(test_spec, best_params)

# forward-backward on the test data
fb_test     <- forwardbackward(test_model)
test_logLik <- fb_test$logLike
train_logLik <- logLik(best_model)

cat("Train logLik =", train_logLik, "\n")
cat("Test  logLik =", test_logLik,  "\n")

# Normalize by #rows for a fair comparison
norm_train_ll <- as.numeric(train_logLik) / nrow(train_mat)
norm_test_ll  <- test_logLik / nrow(test_mat)

cat("\nNormalized Train LL =", norm_train_ll, 
    "\nNormalized Test  LL =", norm_test_ll, "\n")




