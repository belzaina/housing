library(magrittr)


source('scripts/prepare_housing_dataset.R')
source('scripts/partition_data.R')
source('scripts/random_forest.R')
source('scripts/compute_evaluation_criteria.R')


housing_dataframe <- prepare_housing_dataset()

partitions <- partition_data(housing_dataframe, N = 5)

random_forest_results <- partitions %>%
   purrr::map_df(
      
      function(data_partition) {
         
         # STEP 1:
         # Train on Fold1 and Test on Fold2
         predictions_test_1 <- random_forest(
            train_dataframe = data_partition[['Fold1']],
            test_dataframe  = data_partition[['Fold2']]
         )
         eval_metrics_1 <- compute_evaluation_criteria(
            y_true            = data_partition[['Fold2']]$BAD %>% as.character() %>% as.numeric(),
            predicted_y_prob  = predictions_test_1[['Predicted_Y_Test_Prob']],
            predicted_y_class = predictions_test_1[['Predicted_Y_Test_Class']]
         )
         
         # STEP 2:
         # Train on Fold2 and Test on Fold1
         y_test_2 <- random_forest(
            train_dataframe = data_partition[['Fold2']],
            test_dataframe  = data_partition[['Fold1']]
         )
         eval_metrics_2 <- compute_evaluation_criteria(
            y_true            = data_partition[['Fold1']]$BAD %>% as.character() %>% as.numeric(),
            predicted_y_prob  = y_test_2[['Predicted_Y_Test_Prob']],
            predicted_y_class = y_test_2[['Predicted_Y_Test_Class']]
         )
         
         # rbind
         dplyr::bind_rows(eval_metrics_1, eval_metrics_2)
         
      }
      
   )
