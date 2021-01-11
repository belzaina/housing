#' Run Logistic Regression
#' Inputs : Training DataFrame, Testing DataFrame
#' Outputs: Predicted_Y_Test_Prob, Predicted_Y_Test_Class
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