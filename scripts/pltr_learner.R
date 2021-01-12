library(magrittr)

source("scripts/rules_utilities.R")
source("scripts/penalized_learner.R")

#' Penalty
#'    0: Ridge
#'    1: LASSO
#'    2: ADAPTIVE LASSO

pltr_learner <- function(train_dataframe, test_dataframe, predictors_pairs, penalty = c(0, 1, 2)) {
   
   n_predictors <- ncol(train_dataframe) - 1 
   
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
   
   # Count number of rules
   rules_count <- ncol(train_dataframe) - 1 - n_predictors
   
   # Penalize
   penalty_results <- penalized_learner(train_dataframe, test_dataframe, penalty = penalty)
   
   pretty_predictors_names <- penalty_results[["coef_ranks"]]$Predictor %>%
      purrr::map_chr(pretty_print_rule)
   
   list(
      Predicted_Y_Test_Prob  = penalty_results[['Predicted_Y_Test_Prob']],
      Predicted_Y_Test_Class = penalty_results[['Predicted_Y_Test_Class']],
      count_extracted_rules  = rules_count,
      coef_ranks             = penalty_results[['coef_ranks']] %>%
         dplyr::mutate(
            Predictor = pretty_predictors_names
         )
   )
   
}





