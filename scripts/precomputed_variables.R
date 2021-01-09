library(magrittr)

source("scripts/prepare_housing_dataset.R")
source("scripts/add_interaction_quadratic_terms.R")


# RAW DATASET
raw_housing_dataset   <- readxl::read_excel('data/hmeq.xls', sheet = 'hmeq')

# CLEAN WITH FEATURES ENCODED AS DUMMIES
clean_housing_dataset <- prepare_housing_dataset(raw_housing_dataset)

# CLEAN WITH FEATURES ENCODED AS FACTORS (FOR EDA)
clean_housing_dataset_eda <- prepare_housing_dataset(raw_housing_dataset, to_dummy = FALSE)

# WITH FEATURES ENCODED AS DUMMIES + INTERACTION AND QUADRATIC TERMS
clean_with_interaction_quadratic <- add_interaction_quadratic_terms(clean_housing_dataset)

predictors_set <- clean_housing_dataset %>% names() %>% tail(n = -1)

predictors_pairs <- get_predictors_pair(predictors_set)

n_rows <- nrow(raw_housing_dataset)

n_cols_raw <- ncol(raw_housing_dataset)

n_cols_clean <- ncol(clean_housing_dataset)

n_cols_iqt <- ncol(clean_with_interaction_quadratic)

missing_values_raw <- round(
   100 * sum(is.na(raw_housing_dataset)) / (n_rows * n_cols_raw), 
   2
)

missing_values_clean <- round(
   100 * sum(is.na(clean_housing_dataset)) / (n_rows * n_cols_clean), 
   2
)

missing_values_iqt <- round(
   100 * sum(is.na(clean_with_interaction_quadratic)) / (n_rows * n_cols_iqt), 
   2
)









