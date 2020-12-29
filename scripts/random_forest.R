#' Run Random Forest Algorithm
#' Inputs : Training DataFrame, Testing DataFrame
#' Outputs: Predicted_Y_Test_Prob, Predicted_Y_Test_Class
random_forest <- function(train_dataframe, test_dataframe, max_ntree = 200, random_seed = 88) {
   
   y_train <- train_dataframe$BAD
   prior_prob <- (table(y_train) / length(y_train)) %>% as.numeric()
   
   # set the seed
   set.seed(random_seed)
   
   # Find Optimal Number of Trees in the Forest
   model_0 <- randomForest::randomForest(
      BAD ~ .,
      data = train_dataframe,
      classwt = prior_prob,
      ntree = max_ntree,
      nodesize = 5
   )
   oob_errors <- model_0$err.rate[, 1]
   optim_ntree <- which.min(oob_errors)
   
   # Train Model with Optimal Number of Trees
   model_1 <- randomForest::randomForest(
      BAD ~ .,
      data = train_dataframe,
      classwt = prior_prob,
      ntree = optim_ntree,
      nodesize = 5
   )
   
   # Predictions
   list(
      Predicted_Y_Test_Prob  = predict(model_1, test_dataframe, type = "prob")[, 2] %>% as.numeric(),
      Predicted_Y_Test_Class = predict(model_1, test_dataframe) %>% as.character() %>% as.numeric()
   )
   
}