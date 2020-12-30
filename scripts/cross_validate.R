library(magrittr)

#' Train a Learning Algorithm & Output the "N x 2-fold" cross-validation Results (Dietterich, 1998)
#' Inputs : 
#'    - cv_partitions: output of partition_data function (partition_data.R)
#'    - learner      : a learning algorithm
#'    - evaluator    : a function to compute evaluation criteria
#'    - ...          : extra arguments to be passed to the learner
#' Outputs:
#'    - dataframe with evaluation metric for each test-fold

cross_validate <- function(cv_partitions, learner, evaluator, ...) {
   
   cv_partitions %>%
      
      purrr::map_df(
         
         function(data_partition) {
            
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
            
            # rbind
            dplyr::bind_rows(eval_metrics_1, eval_metrics_2)
            
         }
         
      )
   
}