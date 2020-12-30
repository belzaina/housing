library(magrittr)


source("scripts/prepare_housing_dataset.R")
source("scripts/partition_data.R")
source("scripts/compute_evaluation_criteria.R")
source("scripts/random_forest_learner.R")
source("scripts/svm_learner.R")
source("scripts/logistic_learner.R")
source("scripts/cross_validate.R")


partitions <- prepare_housing_dataset() %>%
   partition_data(N = 10)


simulation_1 <- partitions %>%
   cross_validate(random_forest_learner, compute_evaluation_criteria, random_seed = 88)

simulation_2 <- partitions %>%
   cross_validate(v, compute_evaluation_criteria, random_seed = 10)

simulation_3 <- partitions %>%
   cross_validate(random_forest_learner, compute_evaluation_criteria, random_seed = 8888, max_ntree = 500)

simulation_4 <- partitions %>%
   cross_validate(random_forest_learner, compute_evaluation_criteria, max_ntree = 500, random_seed = 1000)

simulation_5 <- partitions %>%
   cross_validate(random_forest_learner, compute_evaluation_criteria, random_seed = 88)

simulation_6 <- partitions %>%
   cross_validate(svm_learner, compute_evaluation_criteria)

simulation_7 <- partitions %>%
   cross_validate(svm_learner, compute_evaluation_criteria, kernel = 'polynomial')

# same as simulation_6
simulation_8 <- partitions %>%
   cross_validate(svm_learner, compute_evaluation_criteria, kernel = 'radial')

simulation_9 <- partitions %>%
   cross_validate(logistic_learner, compute_evaluation_criteria)













