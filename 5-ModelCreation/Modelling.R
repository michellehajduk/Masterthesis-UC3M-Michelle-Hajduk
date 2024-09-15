################################# LIBRARIES ####################################
library(caret)
library(randomForest)
library(glmnet)
library(xgboost)
library(readxl)

################################# LOAD DATA ####################################
property_data_final <- read_xlsx("property_data_final.xlsx")

############################## TRAIN CONTROL ###################################
# Set up cross-validation with 10 folds
train_control <- trainControl(method = "cv", number = 10, savePredictions = "all")
set.seed(123)
################################# MODELING #####################################
# --- Linear Regression (lm) ---
lm_model <- train(
  price_per_m2 ~ ., 
  data = property_data_final, 
  method = "lm",
  trControl = train_control,
  preProcess = c("center", "scale") 
)

# Print results
print(lm_model$results)

# --- Generalized Linear Model with Elastic Net Regularization (glmnet) ---
# Define grid of hyperparameters
glmnet_grid <- expand.grid(
  alpha = seq(0, 1, length = 3), 
  lambda = 10^seq(0, -2, length = 20)  
)

glmnet_model <- train(
  price_per_m2 ~ ., 
  data = property_data_final, 
  method = "glmnet", 
  trControl = train_control,
  tuneGrid = glmnet_grid,
  preProcess = c("center", "scale")  
)

# Print results and best tuning parameters
print(glmnet_model$results)
print(glmnet_model$bestTune)

# --- Random Forest (randomForest) ---
# Define grid of hyperparameters
rf_grid1 <- expand.grid(
  mtry = c(1, 20, 30, 40, 50, 60) 
)
rf_grid2 <- expand.grid(
  mtry = c(37, 38, 39, 40, 41, 42, 43) 
)

rf_model <- train(
  price_per_m2 ~ ., 
  data = property_data_final, 
  method = "rf", 
  trControl = train_control,
  tuneGrid = rf_grid2,
  preProcess = c("center", "scale") 
)

# Print results and best tuning parameters
print(rf_model$results)
print(rf_model$bestTune)

# --- XGBoost (xgbTree) ---
# Define grid of hyperparameters
xgb_grid <- expand.grid(
  nrounds = c(50, 100),          
  max_depth = c(3, 6),           
  eta = c(0.1),                  
  gamma = c(0, 0.1),            
  colsample_bytree = c(0.7),     
  min_child_weight = c(1, 5),   
  subsample = c(0.7)            
)

xgb_model <- train(
  price_per_m2 ~ ., 
  data = property_data_final, 
  method = "xgbTree", 
  trControl = train_control,
  tuneGrid = xgb_grid,
  preProcess = c("center", "scale")  
)

# Print results and best tuning parameters
print(xgb_model$results)
print(xgb_model$bestTune)

################################ SAVE MODELS ###################################
# Save trained models to files
save(lm_model, file = "lm_model2.RData")
save(glmnet_model, file = "glmnet_model2.RData")
save(rf_model, file = "rf_model2.RData")
save(xgb_model, file = "xgb_model2.RData")
