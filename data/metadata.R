metadata <- list(
   
   "INTRO_1" = "In this project we implement the Penalized Logistic Tree Regression (PLTR) methodology described by Dumitrescu et al. (2020). According to the authors, PLTR aims to improve the predictive performance of the logistic regression model through new predictors based on short-depth decision trees and a penalised estimation method while preserving the intrinsic interpretability of the scoring model. As a consequence, PLTR combine the advantages of:",
   
   "INTRO_2" = "Logistic regression in term of interpretability;",
   
   "INTRO_3" = "Decision Trees in term of capturing non-linear effects.",
   
   "INTRO_4" = "The algorithm proceeds in two steps:",
   
   "INTRO_5" = "Step 1: Learn new indicator variables that corresponds to threshold effects identified from trees with two splits.",
   
   "INTRO_6" = "Step 2: Use these indicator variables as additional predictors in a penalized logistic regression.",
   
   # "EXTRACTED_RULES" = "The new indicator variables correspond to the following rules:",
   
   "EXTRACTED_RULES" = "One leaf of each of the two branches originating from the root of the tree is retained - Dumitrescu et al. (2020):"
   
)
