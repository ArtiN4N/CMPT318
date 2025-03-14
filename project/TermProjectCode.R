#-----------------------------------------------------------------------------#
#                              Term Project                                   #
#-----------------------------------------------------------------------------#

# ---- Part 1: Feature Scaling ---- #

library(zoo)
library(ggbiplot)

# Import text dataset and convert all columns except date and time to numeric 
df <- TermProjectData
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

