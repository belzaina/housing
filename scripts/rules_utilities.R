library(magrittr)


format_rule <- function(s) {
   
   s %<>% stringr::str_replace_all(">=", "GTE")
   s %<>% stringr::str_replace_all("<=", "LTE")
   s %<>% stringr::str_replace_all(">", "GT")
   s %<>% stringr::str_replace_all("<", "LT")
   s %<>% stringr::str_replace_all("&", "AND")
   
   s
   
}

pretty_print_rule <- function(s) {
   
   s %<>% stringr::str_replace_all("_GTE_", " >= ")
   s %<>% stringr::str_replace_all("_LTE_", " <= ")
   s %<>% stringr::str_replace_all("_GT_", " > ")
   s %<>% stringr::str_replace_all("_LT_", " < ")
   s %<>% stringr::str_replace_all("_", " ")
   
   s
   
}


extract_rules <- function(rpart_model, format = TRUE) {
   
   rules <- rpart_model %>%
      rpart.plot::rpart.rules(nn = TRUE) %>% as.data.frame()
   
   n_rules <- nrow(rules)
   
   if (n_rules == 2) terminal_node <- 3
   
   else if (n_rules == 3) terminal_node <- if (2 %in% rules$nn) c(2, 7) else c(5, 3)
   
   else terminal_node <- c(5, 7)
   
   terminal_node %>% lapply(
      
      function(node_number) {
         
         rule_name <- rules[rules$nn == node_number, ] %>% 
            as.character() %>%
            tail(n = -3) %>%
            purrr::keep(function(s) s != "") %>%
            stringr::str_trim() %>%
            stringr::str_c(collapse = "_")
         
         if (format) rule_name %<>% format_rule() %>% make.names()
         
         list(
            node_number = node_number,
            rule_name   = rule_name
         )
         
      }
      
   )
   
}


get_predictors_pair <- function(predictors) {
   
   predictors %>%
      combn(m = 2) %>%
      purrr::array_branch(margin = 2)
   
}
















