library(magrittr)


#' Partition Housing Dataset: N x 2-fold cross-validation of Dietterich (1998)
#' Input : Dataframe, N, Random Seed
#' Output: List of N list. Each sublist contains 2 dataframes (2-fold)
partition_data <- function(housing_dataframe, N = 5, random_seed = 88) {
   
   n_row <- nrow(housing_dataframe)
   
   partitions <- list()
   
   for (i in 1:N) {
      
      set.seed(random_seed + i)
      random_rows <- sample(1:n_row)
      
      partitions[[paste0('N', i)]] <- list(
         'Fold1' = dplyr::slice(housing_dataframe, head(random_rows, n = n_row * 0.5)),
         'Fold2' = dplyr::slice(housing_dataframe, tail(random_rows, n = n_row * 0.5))
      )
      
   }
   
   partitions
   
}