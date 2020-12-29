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
   # housing_dataset %<>%
   #    data.table::as.data.table() %>%
   #    dplyr::mutate(
   #       JOB = as.factor(JOB),
   #       REASON = as.factor(REASON)
   #    ) %>%
   #    mltools::one_hot(cols = c("JOB", "REASON")) %>%
   #    dplyr::as_tibble()
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