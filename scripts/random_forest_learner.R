#' Run Random Forest Algorithm
#' Inputs : Training DataFrame, Testing DataFrame
#' Outputs: Predicted_Y_Test_Prob, Predicted_Y_Test_Class
random_forest_learner <- function(train_dataframe, test_dataframe, max_ntree = 200, 
                                  random_seed = 88, var_imp_type = 1) {
   
   y_train <- train_dataframe$BAD
   prior_prob <- (table(y_train) / length(y_train))
   
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
      nodesize = 5,
      importance = TRUE
   )
   
   # Get variable importance
   var_ranks <- randomForest::importance(model_1, type = var_imp_type)[, 1]
   var_ranks <- dplyr::tibble(
      Predictor = names(var_ranks),
      Importance = var_ranks
   ) %>% dplyr::arrange(
      dplyr::desc(Importance)
   )
   
   # Predictions
   list(
      Predicted_Y_Test_Prob  = predict(model_1, test_dataframe, type = "prob")[, "1"] %>% as.numeric(),
      Predicted_Y_Test_Class = predict(model_1, test_dataframe) %>% as.character() %>% as.numeric(),
      var_ranks              = var_ranks,
      optim_ntree            = optim_ntree
   )
   
}