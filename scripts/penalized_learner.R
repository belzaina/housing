#' Run LASSO || RIDGE 
#' Inputs : 
#'    - Training DataFrame
#'    - Testing DataFrame
#'    - Penalty (1: LASSO, 0: RIDGE)
#' Outputs: Predicted_Y_Test_Prob, Predicted_Y_Test_Class
penalized_learner <- function(train_dataframe, test_dataframe, penalty = c(0, 1)) {
   
   explanatory_train <- train_dataframe %>%
      dplyr::select(-BAD) %>%
      dplyr::mutate(Fold = "TRAIN")
   
   dependant_train <- train_dataframe$BAD
   
   explanatory_test <- test_dataframe %>%
      dplyr::select(-BAD) %>%
      dplyr::mutate(Fold = "TEST")
   
   dependant_test <- test_dataframe$BAD
   
   # MERGE TRAIN & TEST to CREATE DUMMIES
   train_test_explanatory <- rbind(explanatory_train, explanatory_test) %>%
      fastDummies::dummy_columns(select_columns = c("JOB", "REASON"), remove_selected_columns = TRUE)
   
   # GET BACK TRAIN & TEST EXPLANATORY
   explanatory_train <- train_test_explanatory %>% 
      dplyr::filter(Fold == "TRAIN") %>%
      dplyr::select(-Fold)
   
   explanatory_test  <- train_test_explanatory %>% dplyr::filter(Fold == "TEST") %>%
      dplyr::select(-Fold)
   
   # INTERACTION TERMS
   M <- ncol(explanatory_train)
   for (i in 1:(M - 1)) {
      
      for (j in (i + 1):M) {
         
         var_1_name <- colnames(explanatory_train)[i]
         var_2_name <- colnames(explanatory_train)[j]
         
         col_name <- paste0(var_1_name, "_", var_2_name)
         
         explanatory_train %<>% dplyr::mutate(
            
            "{col_name}" := (dplyr::pull(explanatory_train, var_1_name) * 
                                dplyr::pull(explanatory_train, var_2_name))
            
         )
         
         explanatory_test %<>% dplyr::mutate(
            
            "{col_name}" := (dplyr::pull(explanatory_test, var_1_name) * 
                                dplyr::pull(explanatory_test, var_2_name))
            
         )
         
      }
      
   }
   
   # Remove duplicate variables if any
   explanatory_train %<>% .[!duplicated(as.list(.))]
   explanatory_test  %<>% dplyr::select(
      dplyr::all_of(colnames(explanatory_train))
   )
   
   # Quadratic Terms
   quant_variables <- colnames(explanatory_train)[1:10]
   for (qv in quant_variables) {
      
      qv_name <- paste0(qv, "^2")
      
      explanatory_train %<>% dplyr::mutate(
         "{qv_name}" := (dplyr::pull(explanatory_train, qv) ^ 2)
      )
      
      explanatory_test %<>% dplyr::mutate(
         "{qv_name}" := (dplyr::pull(explanatory_test, qv) ^ 2)
      )
      
   }
   
   # 10-FOLD CV
   glmnet_model <- glmnet::cv.glmnet(
      x = explanatory_train %>% data.matrix(),
      y = dependant_train,
      family = "binomial",
      type.measure = "auc",
      nfolds = 10,
      alpha = penalty
   )
   
   # MAKE PREDICTIONS
   predicted_test_class <- predict(
      glmnet_model, 
      newx = explanatory_test %>% data.matrix(), 
      s = "lambda.min", 
      type = "class"
   ) %>% as.numeric()
   
   predicted_test_prob <- predict(
      glmnet_model, 
      newx = explanatory_test %>% data.matrix(), 
      s = "lambda.min", 
      type = "response"
   ) %>% as.numeric()
   
   list(
      Predicted_Y_Test_Prob  = predicted_test_prob,
      Predicted_Y_Test_Class = predicted_test_class
   )
   
}













