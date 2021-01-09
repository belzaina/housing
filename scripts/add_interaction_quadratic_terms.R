#' Add Interaction and Quadratic Terms
#' Input: Preprocessed Housing Dataset
#' Output: Preprocessed Housing Dataset with Interaction and Quadratic Terms
add_interaction_quadratic_terms <- function(housing_dataset) {
   
   features_names <- housing_dataset %>%
      dplyr::select(-BAD) %>%
      colnames()
   
   M <- length(features_names)
   
   for (i in 1:(M - 1)) {
      
      for (j in (i + 1):M) {
         
         var_1_name <- features_names[i]
         var_2_name <- features_names[j]
         
         col_name <- paste0(var_1_name, "_", var_2_name)
         
         housing_dataset %<>% dplyr::mutate(
            
            "{col_name}" := (dplyr::pull(housing_dataset, var_1_name) * 
                                dplyr::pull(housing_dataset, var_2_name))
            
         )
         
      }
      
   }
   
   # Remove duplicate variables if any
   housing_dataset %<>% .[!duplicated(as.list(.))]
   
   # Quadratic Terms
   quant_variables <- features_names[1:10]
   for (qv in quant_variables) {
      
      qv_name <- paste0(qv, "^2")
      
      housing_dataset %<>% dplyr::mutate(
         "{qv_name}" := (dplyr::pull(housing_dataset, qv) ^ 2)
      )
      
   }
   
   housing_dataset
   
}