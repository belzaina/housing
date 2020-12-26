library(magrittr)
library(dplyr)


#' Helper function to compute the mode
#' Input : qualitative variable vector
#' Output: the mode
mode_stat <- function(vect) {
   vect %>%
      table() %>%
      which.max() %>%
      names()
}


#' Read and clean the housing dataset
#' Input : path to the housing dataset file
#' Output: dataframe
prepare_housing_dataset <- function(path_xls = './data/hmeq.xls') {
   
   # Read Excel File
   housing_dataset <- readxl::read_excel(path = path_xls, sheet = 'hmeq')
   
   # Qualitative Explanatory Variables: REASON, JOB
   # Replace Missing Values with the Mode
   housing_dataset %<>% tidyr::replace_na(
      list(
         REASON = mode_stat(.$REASON),
         JOB = mode_stat(.$JOB)
      )
   )
   
   # Quantitative Explanatory Variables
   # Replace Missing Values with the Mean
   housing_dataset %<>% dplyr::mutate_if(
      anyNA, 
      ~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)
   )
   
   # Encode Qualitative Explanatory Variables: REASON, JOB
   housing_dataset %<>%
      data.table::as.data.table() %>%
      dplyr::mutate(
         JOB = as.factor(JOB),
         REASON = as.factor(REASON)
      ) %>%
      mltools::one_hot(cols = c("JOB", "REASON")) %>%
      dplyr::as_tibble()
   
   # Return the Clean Dataset
   housing_dataset
   
}


#' Partition Housing Dataset: N x 2-fold cross-validation of Dietterich (1998)
#' Input : Dataframe, N, Random Seed
#' Output: List of N list. Each sublist contains 2 dataframes (2-fold)
partition_data <- function(housing_dataframe, N, random_seed = 88888) {
   
   n_row <- nrow(housing_dataframe)
   
   partitions <- list()
   
   for (i in 1:N) {
      
      set.seed(random_seed + i)
      random_rows <- sample(1:n_row)
      
      partitions[[paste0('N', i)]] <- list(
         'Fold1' = dplyr::slice(housing_dataframe, head(random_rows, n = n_row / 2)),
         'Fold2' = dplyr::slice(housing_dataframe, tail(random_rows, n = n_row / 2))
      )
      
   }
   
   partitions
   
}


#' Performance evaluation:
#' Area under the ROC curve (AUC)
#' Percentage of correctly classied (PCC) cases
#' Brier score (BS)
#' Kolmogorov-Smirnov statistic (KS)
#' Partial Gini Index (PGI)
compute_evaluation_criteria <- function(y_true, y_hat_prob, y_hat_class) {
   
   # AUC
   auc <- cvAUC::AUC(predictions = y_hat_prob, labels = y_true)
   
   # PCC 
   confusion_matrix <- table(True_Labels = y_true, Predicted_Labels = y_hat_class)
   pcc <- sum(diag(confusion_matrix)) / length(y_true)
   
   # BS
   bs <- mean((y_true - y_hat_prob) ^ 2)
   
   # KS
   p1 <- y_hat_prob[y_true == 1]
   p0 <- y_hat_prob[y_true == 0]
   ks <- ks.test(p1, p0)$statistic
   
   dplyr::tibble(
      AUC = auc,
      PCC = pcc,
      BS = bs,
      KS = ks
   )
   
} 











