library(magrittr)
library(dplyr)


# Helper function to compute the mode
mode_stat <- function(vect) {
   vect %>%
      table() %>%
      which.max() %>%
      names()
}


housing_dataset <- readxl::read_excel('./data/hmeq.xls', sheet = 'hmeq')


# Qualitative Explanatory Variables: REASON, JOB
# Replace missing values with the MODE
housing_dataset %<>% tidyr::replace_na(
   list(
      REASON = mode_stat(.$REASON),
      JOB = mode_stat(.$JOB)
   )
)


# Quantitative Explanatory Variables
# Replace missing values with the MEAN
housing_dataset %<>% dplyr::mutate_if(anyNA, ~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))


# Encode Qualitative Variables
housing_dataset %>%
   data.table::as.data.table() %>%
   dplyr::mutate(
      JOB = as.factor(JOB),
      REASON = as.factor(REASON)
   ) %>%
   mltools::one_hot(cols = c("JOB", "REASON")) %>%
   dplyr::as_tibble()














