#' Run SVM Algorithm
#' Inputs : 
#'    - Training DataFrame
#'    - Testing DataFrame
#'    - ... (extra argument to be passed to lasso function, e.g. kernel...)
#' Outputs: Predicted_Y_Test_Prob, Predicted_Y_Test_Class
pltr_learner <- function(train_dataframe, test_dataframe, ...) {
   
   # New dataframes
   extended_train_dataframe <- dplyr::tibble() %>% rbind(train_dataframe)
   extended_test_dataframe <- dplyr::tibble() %>% rbind(test_dataframe)
   
   # Get Features Names
   explanatory_variables <- colnames(train_dataframe) %>%
      purrr::keep(function(s) s != "BAD")
   
   # Generate a list of unique couples of features
   features_couples <- explanatory_variables %>%
      combn(m = 2) %>%
      purrr::array_branch(margin = 2)
   
   # Learn two-split tree for each couple
   for (couple in features_couples) {
      
      train_subset <- train_dataframe %>%
         dplyr::select(c("BAD", dplyr::all_of(couple)))
      
      test_subset <- test_dataframe %>%
         dplyr::select(c("BAD", dplyr::all_of(couple)))
      
      two_split_tree <- rpart::rpart(
         BAD ~ .,
         data = train_subset,
         control = list(maxdepth = 2, cp = -1)
      )
      
      party_obj <- partykit::as.party.rpart(two_split_tree)
      
      # Extract rules names
      node_3_rule <- partykit:::.list.rules.party(party_obj)["3"]
      node_4_rule <- partykit:::.list.rules.party(party_obj)["4"]
      node_5_rule <- partykit:::.list.rules.party(party_obj)["6"] %>%
         stringr::str_split(" & ") %>%
         unlist() %>%
         .[1]
      
      # Create dummy variables based on extracted rules
      extended_train_dataframe %<>%
         dplyr::mutate(
            "{node_3_rule}" := (partykit::predict.party(party_obj, type = "node") == 3) %>% as.numeric(),
            "{node_4_rule}" := (partykit::predict.party(party_obj, type = "node") == 4) %>% as.numeric(),
            "{node_5_rule}" := (partykit::predict.party(party_obj, type = "node") > 5) %>% as.numeric()
         )
      
      extended_test_dataframe %<>%
         dplyr::mutate(
            "{node_3_rule}" := (partykit::predict.party(party_obj, 
                                                        newdata = test_subset, 
                                                        type = "node") == 3) %>% as.numeric(),
            "{node_4_rule}" := (partykit::predict.party(party_obj, 
                                                        newdata = test_subset,
                                                        type = "node") == 4) %>% as.numeric(),
            "{node_5_rule}" := (partykit::predict.party(party_obj, 
                                                        newdata = test_subset,
                                                        type = "node") > 5) %>% as.numeric()
         )
      
   }
   
   # Remove duplicated rules
   extended_train_dataframe %<>% .[!duplicated(as.list(.))]
   extended_test_dataframe %<>% dplyr::select(
      dplyr::all_of(colnames(extended_train_dataframe))
   )
   
   # Cross validate LASSO
   
   # Predict on test set
   
}






