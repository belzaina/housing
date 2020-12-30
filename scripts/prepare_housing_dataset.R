library(magrittr)


#' Helper function to compute the mode
#' Input : qualitative variable vector
#' Output: the mode
mode_stat <- function(vect) {
   vect %>%
      table() %>%
      which.max() %>%
      names()
}


#' Clean the housing dataset
#' Input : raw housing dataframe
#' Output: preprocessed dataframe
prepare_housing_dataset <- function(raw_housing_dataframe) {
   
   # Qualitative Explanatory Variables: REASON, JOB
   # Replace Missing Values with the Mode
   housing_dataset <- raw_housing_dataframe %>% 
      tidyr::replace_na(
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
   housing_dataset %<>% dplyr::mutate(
      JOB    = as.factor(JOB),
      REASON = as.factor(REASON)
   )
   
   # Encode the Dependent Variable as a Factor
   housing_dataset %<>% dplyr::mutate(
      BAD = as.factor(BAD)
   )
   
   # Return the Clean Dataset
   housing_dataset
   
}