#' Run SVM Algorithm
#' Inputs : 
#'    - Training DataFrame
#'    - Testing DataFrame
#'    - ... (extra argument to be passed to svm function, e.g. kernel...)
#' Outputs: Predicted_Y_Test_Prob, Predicted_Y_Test_Class
svm_learner <- function(train_dataframe, test_dataframe, ...) {
   
   y_train <- train_dataframe$BAD
   prior_prob <- (table(y_train) / length(y_train))
   
   # Train
   svm_model <- e1071::svm(
      BAD ~ .,
      data = train_dataframe,
      class.weights = prior_prob,
      probability = TRUE,
      ...
   )
   
   # Test
   list(
      
      Predicted_Y_Test_Prob  = predict(svm_model, test_dataframe, probability = TRUE) %>%
         attr("probabilities") %>%
         .[, "1"],
      
      Predicted_Y_Test_Class = predict(svm_model, test_dataframe) %>% as.character() %>% as.numeric()
      
   )
   
}