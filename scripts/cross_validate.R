library(magrittr)

#' Train a Learning Algorithm & Output the "N x 2-fold" cross-validation Results (Dietterich, 1998)
#' 
#' Inputs : 
#' 
#'    - cv_partitions: List of data partitions - output of partition_data function (partition_data.R)
#'    - learner      : a learning algorithm
#'    - evaluator    : a function to compute evaluation criteria
#'    - shinyProgress: Logical (default TRUE). Showing Shiny Progress or Not
#'    - ...          : extra arguments to be passed to the learner
#' 
#' Outputs:
#'    - dataframe with evaluation metric for each test-fold

cross_validate <- function(cv_partitions, learner, evaluator, shinyProgress = TRUE, ...) {
   
   N <- 1
   
   cv_partitions %>%
      
      purrr::map_df(
         
         function(data_partition) {
            
            if (shinyProgress) {
               
               showModal(
                  modalDialog(
                     paste0("Processing N = ", N, ": Training on Fold 1 and Testing on Fold 2"), 
                     footer = icon("hourglass-start")
                  )
               )
               
            } else {
               
               cat(paste0("Processing N = ", N, ": Training on Fold 1 and Testing on Fold 2\n"))
               
            }
            
            # STEP 1:
            # Train on Fold1 and Test on Fold2
            predictions_test_1 <- learner(
               train_dataframe = data_partition[['Fold1']],
               test_dataframe  = data_partition[['Fold2']],
               ...
            )
            
            eval_metrics_1 <- evaluator(
               y_true            = data_partition[['Fold2']]$BAD %>% as.character() %>% as.numeric(),
               predicted_y_prob  = predictions_test_1[['Predicted_Y_Test_Prob']],
               predicted_y_class = predictions_test_1[['Predicted_Y_Test_Class']]
            )
            
            eval_metrics_1 %<>% dplyr::mutate(
               Iteration = paste0("N = ", N, ", test 1")
            )
            
            if (shinyProgress) {
               
               removeModal()
               
               showModal(
                  modalDialog(
                     paste0("Still Processing N = ", 
                            N, 
                            ", But Now Training on Fold 2 and Testing on Fold 1"), 
                     footer = icon("hourglass-end")
                  )
               )
               
            } else {
               
               cat(paste0("Still Processing N = ", 
                          N, 
                          ", But Now Training on Fold 2 and Testing on Fold 1\n"))
               
            }
            
            # STEP 2:
            # Train on Fold2 and Test on Fold1
            y_test_2 <- learner(
               train_dataframe = data_partition[['Fold2']],
               test_dataframe  = data_partition[['Fold1']],
               ...
            )
            
            eval_metrics_2 <- evaluator(
               y_true            = data_partition[['Fold1']]$BAD %>% as.character() %>% as.numeric(),
               predicted_y_prob  = y_test_2[['Predicted_Y_Test_Prob']],
               predicted_y_class = y_test_2[['Predicted_Y_Test_Class']]
            )
            
            eval_metrics_2 %<>% dplyr::mutate(
               Iteration = paste0("N = ", N, ", test 2")
            )
            
            if (shinyProgress) removeModal()
            
            N <<- N + 1
            
            # rbind
            dplyr::bind_rows(eval_metrics_1, eval_metrics_2) %>%
               dplyr::relocate(Iteration, .before = AUC)
            
         }
         
      )
   
}