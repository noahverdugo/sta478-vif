# Calculate VIF function
# Input: dataframe
# Output: dataframe results (predictor | VIF)
VIF <- function(dataframe) {
  # Length of dataframe
  length <- ncol(dataframe)
  # List of VIF values, length: Length of dataframe..
  VIF_values <- numeric(length)
  # For each predictor
  for(i in 1:length)
  {
    # Calculate model from dataframe
    # The way this works is it takes the current predictor as a response, and uses the rest of the predictors as predictors
    model <- lm(dataframe[[i]] ~ ., data = dataframe[-i])
    # And grab the R2 from it
    R2 <- summary(model)$r.squared
    # Calculate vif 
    VIF_values[i] = (1)/(1-R2)
  }
  # Create table from the VIF values
  # Turn into a dataframe, with each Predictor being a column and their respective VIF values as another
  results <- data.frame(Predictor = colnames(dataframe), VIF = VIF_values)
  # Requirement : SOrt largest to smallest (using tidyverse)
  results <- arrange(results, desc(VIF))
  return(results) 
}