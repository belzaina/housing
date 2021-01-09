library(magrittr)


source("scripts/prepare_housing_dataset.R")
source("scripts/partition_data.R")
source("scripts/compute_evaluation_criteria.R")
source("scripts/random_forest_learner.R")
source("scripts/svm_learner.R")
source("scripts/logistic_learner.R")
source("scripts/penalized_learner.R")
source("scripts/pltr_learner.R")
source("scripts/cross_validate.R")
source("scripts/rules_utilities.R")

raw_housing_dataframe <- readxl::read_excel("./data/hmeq.xls")

partitions <- prepare_housing_dataset(raw_housing_dataframe) %>%
   partition_data(N = 5)

features_pairs <- colnames(partitions$N1$Fold1) %>% tail(n = -1) %>%
   combn(m = 2) %>%
   purrr::array_branch(margin = 2)

simulation_0 <- partitions %>%
   cross_validate(pltr, compute_evaluation_criteria, predictors_pairs = features_pairs)


simulation_1 <- partitions %>%
   cross_validate(random_forest_learner, compute_evaluation_criteria, random_seed = 88)

simulation_2 <- partitions %>%
   cross_validate(random_forest_learner, compute_evaluation_criteria, random_seed = 10)

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

simulation_10 <- partitions %>%
   cross_validate(pltr_learner, compute_evaluation_criteria)


partitions_1 <- prepare_housing_dataset(raw_housing_dataframe) %>%
   partition_data(N = 1)

simulation_11 <- partitions %>%
   cross_validate(pltr_learner, compute_evaluation_criteria)

simulation_12 <- partitions %>%
   cross_validate(pltr_learner, compute_evaluation_criteria)

simulation_13 <- partitions %>%
   cross_validate(pltr_learner, compute_evaluation_criteria)

# LASSO
simulation_14 <- partitions %>%
   cross_validate(penalized_learner, compute_evaluation_criteria, penalty = 1)

# RIDGE
simulation_15 <- partitions %>%
   cross_validate(penalized_learner, compute_evaluation_criteria, penalty = 0)

# LASSO NL
simulation_16 <- partitions %>%
   cross_validate(penalized_learner, compute_evaluation_criteria, penalty = 1)


housing_dataset <- prepare_housing_dataset(raw_housing_dataframe)

fit <- rpart::rpart(BAD ~ .,
                    data = housing_dataset[, c('BAD', 'JOB_Office', 'JOB_ProfExe', 'JOB_Sales')],
                    control = list(maxdepth=2, cp = -1)
)

rpart.plot::rpart.rules(fit, nn = TRUE)
rpart.plot::rpart.plot(fit)

extract_rules(fit, format = FALSE)

party_obj <- partykit::as.party.rpart(fit)
party_obj
rule_3_name <- partykit:::.list.rules.party(party_obj)["3"]
rule_4_name <- partykit:::.list.rules.party(party_obj)["4"]
rule_5_name <- partykit:::.list.rules.party(party_obj)["6"] %>%
   stringr::str_split(" & ") %>%
   unlist() %>%
   .[1]

extended_df <- housing_dataset %>%
   dplyr::mutate(
      "{rule_3_name}" := (partykit::predict.party(party_obj, type = "node") == 3) %>% as.numeric(),
      "{rule_4_name}" := (partykit::predict.party(party_obj, type = "node") == 4) %>% as.numeric(),
      "{rule_5_name}" := (partykit::predict.party(party_obj, type = "node") > 5) %>% as.numeric()
   )

extended_df %>% colnames() %>% 
   paste(collapse = "\n") %>%
   cat()

extended_df %<>% .[!duplicated(as.list(.))]

# cat(paste(rules, collapse = "\n")) %>% .[1]
# 
# rpart.plot::rpart.plot(fit)
# 
# rule <- rpart::path.rpart(fit, nodes = 4, print.it = FALSE) %>%
#    unlist() %>%
#    unname() %>%
#    .[-1] %>%
#    paste(collapse = " & ")
# 
# 
# parse(text = rule)
# 
# housing_dataset %>%
#    head() %>%
#    dplyr::mutate(
#       "{rule}" := as.numeric(eval(parse(text = rule)))
#    )


# partykit::ctree(
#    BAD ~ .,
#    data = housing_dataset %>% dplyr::select(BAD, JOB), 
#    control = list(maxdepth=1)
# )

ridge_cv <- glmnet::cv.glmnet(
   
   x = housing_dataset %>% dplyr::select(-BAD) %>% data.matrix(),
   y = housing_dataset$BAD,
   family = "binomial",
   type.measure = "auc",
   nfolds = 10,
   alpha = 0,
   
)

ridge_cv$lambda.min
best_ridge_coef <- as.numeric(coef(ridge_cv, s = ridge_cv$lambda.min))[-1]

ada_lasso <- glmnet::cv.glmnet(
   x = housing_dataset %>% dplyr::select(-BAD) %>% data.matrix(),
   y = housing_dataset$BAD,
   family = "binomial",
   type.measure = "auc",
   nfolds = 10,
   alpha = 1,
   penalty.factor = 1 / abs(best_ridge_coef)
)


ada_lasso$lambda.min
best_ada_lasso_coef <- coef(ada_lasso, s = ada_lasso$lambda.min)

predicted_class <- predict(
   ada_lasso, 
   newx = housing_dataset %>% dplyr::select(-BAD) %>% data.matrix(), 
   s = "lambda.min", 
   type = "class"
) %>% as.numeric()

predicted_prob <- predict(
   ada_lasso, 
   newx = housing_dataset %>% dplyr::select(-BAD) %>% data.matrix(), 
   s = "lambda.min", 
   type = "response"
) %>% as.numeric()

