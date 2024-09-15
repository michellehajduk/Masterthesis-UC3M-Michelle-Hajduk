################################# LIBRARIES ####################################
library(caret)
library(ggplot2)
library(GGally)
library(VIM)
library(dplyr)
library(readxl)
library(e1071)
library(stringr)
library(randomForest)
library(car)

################################ LOADING DATA ##################################
property_data_cleaned <- read_xlsx("idealista_properties_cleaned.xlsx")
property_data_cleaned <- as.data.frame(property_data_cleaned)

# Verify there are no missing values in the dataset, there shouldn't be any
colSums(sapply(property_data_cleaned, is.na))
head(property_data_cleaned)


########################### ADAPT TARGET VARIABLE ##############################
# Calculate price per square meter to reduce redundancy and prevent data leakage.
property_data_cleaned$price_per_m2 <- NA

for (i in 1:nrow(property_data_cleaned)) {
  property_data_cleaned$price_per_m2[i] <- round(property_data_cleaned$price[i] / property_data_cleaned$m2_built[i], 2)
}

property_data_cleaned <- property_data_cleaned[, -which(names(property_data_cleaned) %in% c("price"))]
head(property_data_cleaned)


############################## DATA STRUCTURE ##################################
# Convert integer and categorical variables to appropriate data types.
integer_variables <- c("m2_built", "landPlot", "bedrooms", "bathrooms", "numFloors", "distanceTrainStation", "distanceHospitals", "distanceSchools")
for (variable in integer_variables) {
  property_data_cleaned[[variable]] <- as.integer(property_data_cleaned[[variable]])
}

categorical_variables <- c("autonomous_community", "province", "propertyType", "floorLevel", "exterior", "built_in", "condition", "energy_certification", "heatingType", "heatingDetails", "hasBalcony", "hasTerrace", "hasStoreroom", "hasFireplace", "hasFittedWardrobes", "hasEquippedKitchen", "isFurnished", "hasGarden", "hasAirConditioning", "hasGreenAreas", "hasParkingSpace", "hasLift", "hasSwimmingPool")
for (variable in categorical_variables) {
  property_data_cleaned[[variable]] <- as.factor(property_data_cleaned[[variable]])
}

str(property_data_cleaned)

############################### VISUALIZATION ##################################
# Visualizing categorical variables
## Plot frequency and distribution of categorical variables
par(mfrow = c(2, 4), mar = c(1, 1, 4, 2))
for (variable in categorical_variables) {
  barplot(table(property_data_cleaned[[variable]]), 
          main = paste("Bar Plot of\n", variable), 
          xlab = variable, ylab = "Frequency", col = "royalblue1")
  
  boxplot(property_data_cleaned$price_per_m2 ~ property_data_cleaned[[variable]], 
          col = "royalblue1", main = paste("Distribution of SalePrice \nby", variable),
          xlab = variable, ylab = "SalePrice")
}

## Remove Extreme Outliers
# Filter out extreme outliers based on price per square meter
indices_to_remove <- which(property_data_cleaned$price_per_m2 > 200)
property_data_cleaned2 <- property_data_cleaned[-indices_to_remove, ]

## Visualizing Categorical Variables After Outlier Removal
# Re-plot frequency and distribution after removing outliers
par(mfrow = c(2, 4), mar = c(1, 1, 4, 2))
for (variable in categorical_variables) {
  barplot(table(property_data_cleaned2[[variable]]), 
          main = paste("Bar Plot of\n", variable), 
          xlab = variable, ylab = "Frequency", col = "royalblue1")
  
  boxplot(property_data_cleaned2$price_per_m2 ~ property_data_cleaned2[[variable]], 
          col = "royalblue1", main = paste("Distribution of \nSalePrice by", variable),
          xlab = variable, ylab = "SalePrice")
}


## Visualizing Numerical Variables
# Use ggpairs to visualize relationships between numerical variables.
numerical_variables <- c("m2_built", "landPlot", "bedrooms", "lat", "lon", "bathrooms", "numFloors", "distanceTrainStation", "distanceHospitals", "distanceSchools", "price_per_m2")
ggpairs(property_data_cleaned2[, numerical_variables])
property_data_cleaned2 <- property_data_cleaned2[, -which(names(property_data_cleaned2) %in% c("lat", "lon"))]

################################# SKEWNESS ####################################
# Check skewness of numerical variables.
numerical_variables2 <- c("m2_built", "landPlot", "bedrooms", "bathrooms", "numFloors", "distanceTrainStation", "distanceHospitals", "distanceSchools", "price_per_m2") 
skewness_values <- sapply(property_data_cleaned2[, numerical_variables2], skewness)
print(skewness_values)

## Transforming Variables
# Log-transform variables to reduce skewness.
variables_to_log <- c("m2_built", "numFloors", "distanceTrainStation", "distanceHospitals", "distanceSchools", "price_per_m2") 
for (variable in variables_to_log) {
  property_data_cleaned2[[variable]] <- log(property_data_cleaned2[[variable]])
}

variables_to_log_plus1 <- c("landPlot", "bedrooms", "bathrooms") 
for (variable in variables_to_log_plus1) {
  property_data_cleaned2[[variable]] <- log(property_data_cleaned2[[variable]] + 1)
}

# Re-check skewness after transformation.
skewness_values <- sapply(property_data_cleaned2[, numerical_variables2], skewness)
print(skewness_values)

## Re-visualize Numerical Variables
# Use ggpairs to visualize relationships after transformations.
ggpairs(property_data_cleaned2[, numerical_variables2])

## Check Land Plot and Other Variables
# Examine density plots for specific variables.
par(mfrow = c(1, 2), mar = c(1, 1, 4, 2))
plot(density(property_data_cleaned2$landPlot))
plot(density(property_data_cleaned2$numFloors))

# Check proportion of zero values.
sum(property_data_cleaned2$landPlot == 0) / nrow(property_data_cleaned2)
sum(property_data_cleaned2$numFloors == 0) / nrow(property_data_cleaned2)

################################## OUTLIERS ####################################
## Outlier Detection and Removal
# Plot scatter plots to identify and remove outliers.
plot(property_data_cleaned2$bedrooms, property_data_cleaned2$price_per_m2,
     xlab = "Bedrooms", ylab = "Rent Price per square meter",
     main = "Scatter Plot of Rent Price vs. Bedrooms")
outliers <- which(property_data_cleaned2$bedrooms > 2.5 | property_data_cleaned2$price_per_m2 < 0)
points(property_data_cleaned2$bedrooms[outliers], property_data_cleaned2$price_per_m2[outliers],
       col = "red", pch = 16)

plot(property_data_cleaned2$m2_built, property_data_cleaned2$price_per_m2,
     xlab = "Square meters", ylab = "Rent Price per square meter",
     main = paste("Scatter Plot of \n Rent Price vs. m2_built"))

outliers <- which(property_data_cleaned2$m2_built > 7.5 | property_data_cleaned2$price_per_m2 < 0)
points(property_data_cleaned2$m2_built[outliers], property_data_cleaned2$price_per_m2[outliers],
       col = "red", pch = 16)

plot(property_data_cleaned2$distanceTrainStation, property_data_cleaned2$price_per_m2,
     xlab = "Distance to train station", ylab = "Rent Price per square meter",
     main = paste("Scatter Plot of \n Rent Price vs. distanceTrainStation"))

outliers <- which(property_data_cleaned2$distanceTrainStation > 12 | property_data_cleaned2$distanceTrainStation < 2 | property_data_cleaned2$price_per_m2 < 0)
points(property_data_cleaned2$distanceTrainStation[outliers], property_data_cleaned2$price_per_m2[outliers],
       col = "red", pch = 16)


plot(property_data_cleaned2$distanceHospitals, property_data_cleaned2$price_per_m2,
     xlab = "distanceHospitals", ylab = "Rent Price per square meter",
     main = paste("Scatter Plot of \n Rent Price vs. distanceHospitals"))

outliers <- which(property_data_cleaned2$distanceHospitals > 11.5 | property_data_cleaned2$distanceHospitals < 3 | property_data_cleaned2$price_per_m2 < 0)
points(property_data_cleaned2$distanceHospitals[outliers], property_data_cleaned2$price_per_m2[outliers],
       col = "red", pch = 16)


# Remove outliers based on the specified conditions.
property_data_cleaned3 <- subset(property_data_cleaned2, 
                                 !(price_per_m2 < 0 |
                                     m2_built > 7.5 | 
                                     bedrooms > 2.5 | 
                                     bathrooms < 0.5 |
                                     bathrooms > 2.2 |
                                     distanceTrainStation > 12 |
                                     distanceTrainStation < 2 |
                                     distanceSchools > 11.5 | 
                                     distanceSchools < 0.5 |
                                     distanceHospitals > 11.5 | 
                                     distanceHospitals < 3))


################################################################################
################################################################################

############################ FEATURE SELECTION #################################
## Linear Model for Feature Selection
# Build null and full models for stepwise regression
null_model <- lm(price_per_m2 ~ 1, data = property_data_cleaned3)  
full_model <- lm(price_per_m2 ~ ., data = property_data_cleaned3)  

# Perform stepwise selection to choose the best subset of features
set.seed(999) 
selected_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "both")
# Display the summary of the selected model.
summary(selected_model)

# Plot diagnostics for the selected model.
plot(selected_model)

# Identify and remove outliers based on indices provided.
outlier_indices <- c(11794, 8253, 2564, 20955, 20948)
property_data_cleaned3 <- property_data_cleaned3[-outlier_indices, ]

## Random Forest for Feature Importance
rf_model <- randomForest(price_per_m2 ~ ., data = property_data_cleaned3, importance = TRUE)
importance(rf_model)

## Plot Feature Importance
importance_df <- as.data.frame(importance(rf_model))
importance_df$Feature <- rownames(importance_df)
importance_df <- importance_df[order(-importance_df$`%IncMSE`), ]

ggplot(importance_df, aes(x = reorder(Feature, `%IncMSE`), y = `%IncMSE`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Feature Importance by %IncMSE", x = "Feature", y = "% Increase in MSE")

## Remove Unimportant Variables
# Remove variables with low importance based on results
property_data_cleaned3 <- property_data_cleaned3[, -which(names(property_data_cleaned3) %in% c("hasFireplace", "exterior", "hasEquippedKitchen"))]

## Check for Multicollinearity
# Check aliasing in the linear model to identify collinear variables
alias(full_model)

# Remove 'autonomous_community' due to aliasing with 'province'
property_data_cleaned3 <- property_data_cleaned3[, -which(names(property_data_cleaned3) %in% c("autonomous_community"))]

# Calculate Variance Inflation Factor for multicollinearity
full_model <- lm(price_per_m2 ~ ., data = property_data_cleaned3)
vif(full_model)

## Interaction between 'propertyType' and 'hasLift' identified
table(property_data_cleaned3$propertyType, property_data_cleaned3$hasLift)

## Feature Importance with caret
importance <- varImp(full_model, scale = FALSE)
sorted_importance <- importance[order(-importance$Overall), ]
print(sorted_importance)

## Model Comparison
# Compare model performance with and without certain features.
# Results without 'propertyType':
# RMSE: 0.4052934, Rsquared: 0.5477161, MAE: 0.2972719

# Results without 'hasLift':
# RMSE: 0.4043664, Rsquared: 0.5495537, MAE: 0.2966042

# Results with both but multicollinearity:
# RMSE: 0.4042637, Rsquared: 0.5499162, MAE: 0.2967027

# Based on comparison, remove 'hasLift' due to multicollinearity.
property_data_cleaned3 <- property_data_cleaned3[, -which(names(property_data_cleaned3) %in% c("hasLift"))]

############################### FINAL DATASET ##################################
write_xlsx(property_data_cleaned3, "property_data_final.xlsx")
