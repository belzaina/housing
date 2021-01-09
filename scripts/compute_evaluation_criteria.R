#' 1. Area under the ROC curve (AUC)
#' 2. GINI INDEX
#' 3. F1 Score (harmonic mean of the precision and recall)
#' 4. Percentage of correctly classied (PCC) cases
#' 5. Brier score (BS)
#' 6. Kolmogorov-Smirnov statistic (KS)
compute_evaluation_criteria <- function(y_true, predicted_y_prob, predicted_y_class) {
   
   # AUC
   pred_object <- ROCR::prediction(predicted_y_prob, y_true)
   auc <- ROCR::performance(pred_object, measure = "auc")
   auc <- auc@y.values[[1]]
   
   # GINI
   gini <- (2 * auc) - 1
   
   # PCC 
   confusion_matrix <- table(True_Labels = y_true, Predicted_Labels = predicted_y_class)
   pcc <- sum(diag(confusion_matrix)) / length(y_true)
   
   # F1 Score
   tp <- sum(y_true == 1 & predicted_y_class == 1)
   fp <- sum(y_true == 0 & predicted_y_class == 1)
   fn <- sum(y_true == 1 & predicted_y_class == 0)
   precision <- tp / (tp + fp)
   recall <- tp / (tp + fn)
   f1 <- (2 * precision * recall) / (precision + recall)
   
   # BS
   bs <- mean((y_true - predicted_y_prob) ^ 2)
   
   # KS
   p1 <- predicted_y_prob[y_true == 1]
   p0 <- predicted_y_prob[y_true == 0]
   ks <- ks.test(p1, p0)$statistic
   
   dplyr::tibble(
      AUC  = auc,
      GINI = gini,
      F1   = f1,
      PCC  = pcc,
      BS   = bs,
      KS   = ks
   )
   
} 