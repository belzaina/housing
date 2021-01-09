library(magrittr)

source("scripts/rules_utilities.R")


pltr_learner <- function(train_dataframe, test_dataframe, predictors_pairs) {
   
   for (pair in predictors_pairs) {
      
      # Learn only from a pair at a time
      train_subset <- train_dataframe %>% 
         dplyr::select("BAD", dplyr::all_of(pair))
      
      tree_model <- rpart::rpart(
         BAD ~ .,
         data = train_subset,
         model = TRUE,
         control = list(maxdepth = 2, cp = -1)
      )
      
      rules_list <- extract_rules(tree_model)
      
      for (rule in rules_list) {
         
         train_predicted_nodes <- rpart.plot::rpart.predict(tree_model, nn = TRUE)$nn
         test_predicted_nodes  <- rpart.plot::rpart.predict(tree_model, 
                                                            newdata = test_dataframe,
                                                            nn = TRUE)$nn
         
         train_dataframe %<>% dplyr::mutate(
            
            "{rule[['rule_name']]}" := as.numeric(train_predicted_nodes == rule[['node_number']])
            
         )
         
         test_dataframe %<>% dplyr::mutate(
            
            "{rule[['rule_name']]}" := as.numeric(test_predicted_nodes == rule[['node_number']])
            
         )
         
      }
      
   }
   
   # Remove duplicated rules
   train_dataframe %<>% .[!duplicated(as.list(.))]
   test_dataframe  %<>% dplyr::select(
      dplyr::all_of(colnames(train_dataframe))
   )
   
   # CV (10-fold) Ridge to Get the LASSO Penalty Factor
   ridge_cv <- glmnet::cv.glmnet(
      x = train_dataframe %>% dplyr::select(-BAD) %>% data.matrix(),
      y = train_dataframe$BAD,
      family = "binomial",
      type.measure = "auc",
      nfolds = 10,
      alpha = 0,
   )
   
   # -1 to exclude the intercept
   best_ridge_coef <- as.numeric(coef(ridge_cv, s = ridge_cv$lambda.min))[-1]
   
   # CV (10-fold) Adaptive LASSO
   ada_lasso <- glmnet::cv.glmnet(
      x = train_dataframe %>% dplyr::select(-BAD) %>% data.matrix(),
      y = train_dataframe$BAD,
      family = "binomial",
      type.measure = "auc",
      nfolds = 10,
      alpha = 1,
      penalty.factor = 1 / abs(best_ridge_coef)
   )
   
   # Predict on test set
   predicted_test_class <- predict(
      ada_lasso, 
      newx = test_dataframe %>% dplyr::select(-BAD) %>% data.matrix(), 
      s = "lambda.min", 
      type = "class"
   ) %>% as.numeric()
   
   predicted_test_prob <- predict(
      ada_lasso, 
      newx = test_dataframe %>% dplyr::select(-BAD) %>% data.matrix(), 
      s = "lambda.min", 
      type = "response"
   ) %>% as.numeric()
   
   list(
      Predicted_Y_Test_Prob  = predicted_test_prob,
      Predicted_Y_Test_Class = predicted_test_class
   )
   
}





