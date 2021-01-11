#' Run LASSO || RIDGE || Non-Regularization
#' Inputs : 
#'    - Training DataFrame
#'    - Testing DataFrame
#'    - Penalty (1: LASSO, 0: RIDGE)
#' Outputs: Predicted_Y_Test_Prob, Predicted_Y_Test_Class
penalized_learner <- function(train_dataframe, test_dataframe, penalty = c(0, 1, 2)) {
   
   # CASE 1: Adaptive LASSO
   if (penalty == 2) {
      
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
      glmnet_model <- glmnet::cv.glmnet(
         x = train_dataframe %>% dplyr::select(-BAD) %>% data.matrix(),
         y = train_dataframe$BAD,
         family = "binomial",
         type.measure = "auc",
         nfolds = 10,
         alpha = 1,
         penalty.factor = 1 / abs(best_ridge_coef)
      )
      
   } else {
      
      # CASE 2: RIDGE (penalty = 0) OR LASSO (penalty = 1)
      # 10-FOLD CV
      glmnet_model <- glmnet::cv.glmnet(
         x = train_dataframe %>% dplyr::select(-BAD) %>% data.matrix(),
         y = train_dataframe$BAD,
         family = "binomial",
         type.measure = "auc",
         nfolds = 10,
         alpha = penalty
      )
      
   }
   
   # coef_ranks
   cs <- as.matrix(coef(glmnet_model, s = "lambda.min"))
   coef_ranks <- cs[-1, 1]
   
   coef_ranks <- dplyr::tibble(
      Predictor = names(coef_ranks),
      Coefficient = coef_ranks,
      Coefficient_Magnitude = abs(coef_ranks)
   ) %>% dplyr::arrange(
      dplyr::desc(Coefficient_Magnitude)
   )
   
   # MAKE PREDICTIONS
   predicted_test_class <- predict(
      glmnet_model, 
      newx = test_dataframe %>% dplyr::select(-BAD) %>% data.matrix(), 
      s = "lambda.min", 
      type = "class"
   ) %>% as.numeric()
   
   predicted_test_prob <- predict(
      glmnet_model, 
      newx = test_dataframe %>% dplyr::select(-BAD) %>% data.matrix(), 
      s = "lambda.min", 
      type = "response"
   ) %>% as.numeric()
   
   list(
      Predicted_Y_Test_Prob  = predicted_test_prob,
      Predicted_Y_Test_Class = predicted_test_class,
      coef_ranks             = coef_ranks
   )
   
}













