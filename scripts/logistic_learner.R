#' Run Logistic Regression
#' 
#' Inputs :
#'  
#'    - Training DataFrame
#'    - Testing DataFrame
#'      
#'      NB: Qualitative predictors should be encoded as R factor to avoid low rank warning
#'          In our application, use "clean_housing_dataset_eda" tibble to generate cv partitions
#' 
#' Output: List of
#'  
#'    - Predicted_Y_Test_Prob  
#'    - Predicted_Y_Test_Class
#'    - coef_ranks (tibble)
#' 
logistic_learner <- function(train_dataframe, test_dataframe) {
   
   # Train
   logistic_model <- glm(BAD ~ ., data = train_dataframe, family = "binomial")
   
   # coef_ranks
   cs <- coef(logistic_model)
   coef_ranks <- cs[-1]
   
   coef_ranks <- dplyr::tibble(
      Predictor = names(coef_ranks),
      Coefficient = coef_ranks,
      Coefficient_Magnitude = abs(coef_ranks)
   ) %>% dplyr::arrange(
      dplyr::desc(Coefficient_Magnitude)
   )
   
   # Test
   predicted_probs <- predict(logistic_model, test_dataframe, type = "response")
   
   list(
      
      Predicted_Y_Test_Prob  = predicted_probs,
      Predicted_Y_Test_Class = (predicted_probs > 0.5) %>% as.numeric(),
      coef_ranks             = coef_ranks
      
   )
   
}