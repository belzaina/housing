library(shiny)
library(shinydashboard)
library(ggplot2)
library(magrittr)


source("scripts/rules_utilities.R")
source("scripts/compute_evaluation_criteria.R")
source("scripts/pltr_learner.R")

# Pre-computed Variable to save computation power
source("scripts/precomputed_variables.R")


server <- function(input, output) {
   
   output$n_rows <- renderText(n_rows)
   
   output$n_cols <- renderText(
      if (input$dataset_version == "raw") {
         n_cols_raw
      }  else if (input$dataset_version == "clean") {
         n_cols_clean
      } else {
         n_cols_iqt
      }
   )
   
   output$missing_values <- renderText(
      if (input$dataset_version == "raw") {
         missing_values_raw
      }  else if (input$dataset_version == "clean") {
         missing_values_clean
      } else {
         missing_values_iqt
      }
   )
   
   output$colnames_select_input <- renderUI(
      selectInput("variable_name", 
                  "Please choose a variable:", 
                  c('---', sort(colnames(raw_housing_dataset), decreasing = TRUE)))
   )
   
   output$selected_var <- renderText(
      input$variable_name
   )
   
   output$housing_datatable <- DT::renderDataTable(
      (
         
         if (input$dataset_version == "raw") {
            raw_housing_dataset 
         } else if (input$dataset_version == "clean") {
            clean_housing_dataset
         } else {
            clean_with_interaction_quadratic
         }
         
      ) %>%
         dplyr::mutate_if(is.numeric, round, 2),
      options = list(
         scrollX = TRUE, 
         pageLength = 10
      )
   )
   
   output$summary_stats <- shiny::renderUI({
      
      if ((!is.null(input$variable_name)) && (input$variable_name != '---')) {
         
         summarytools::st_options(
            footnote = NA,
            headings = TRUE,
            plain.ascii = FALSE,
            dfSummary.varnumbers = FALSE
         )
         
         univ_summary <- clean_housing_dataset_eda %>%
            dplyr::select(input$variable_name) %>%
            summarytools::dfSummary()
         
         attr(univ_summary, "data_info")$Data.frame <- paste("Feature:", input$variable_name)
         
         univ_summary %<>% print(method = "render")
         
         bivar_summary <- clean_housing_dataset_eda %>%
            dplyr::select(input$variable_name, BAD) %>%
            dplyr::group_by(BAD) %>%
            summarytools::dfSummary()
         
         bivar_summary[[1]] %<>% head(n = 1)
         bivar_summary[[2]] %<>% head(n = 1)
         
         attr(bivar_summary[[1]], "data_info")$Data.frame <- paste("Feature:", input$variable_name)
         
         bivar_summary %<>% print(method = "render")
         
         if (input$variable_name == "BAD") {
            
            univ_summary
            
         } else {
            
            shiny::tabsetPanel(
               
               shiny::tabPanel(
                  strong("Total"),
                  br(),
                  univ_summary
               ),
               
               shiny::tabPanel(
                  strong("By Target"),
                  br(),
                  bivar_summary
               )
               
            )
            
         }
         
      }
      
   })
   
   output$predictors_set <- renderUI(
      
      selectInput(
         
         "predictors_set",
         
         NULL,
         
         multiple = TRUE,
         
         selected = predictors_set,
         
         choices = predictors_set
         
      )
      
   )
   
   two_split_tree <- reactive({
      
      features <- input$predictors_set
      
      if (!is.null(features)) {
         
         rpart::rpart(
            BAD ~ .,
            data = clean_housing_dataset %>% dplyr::select(dplyr::all_of(c("BAD", features))),
            control = list(maxdepth = 2, cp = -1),
            model = TRUE
         )         
         
      }
      
   })
   
   output$two_split_tree <- renderPlot({
      
      tree <- two_split_tree()
      
      if (!is.null(tree)) rpart.plot::rpart.plot(tree)
      
   })
   
   output$extracted_rules <- renderUI({
      
      tree <- two_split_tree()
      
      if (!is.null(tree)) {
         
         rules_list <- extract_rules(tree, format = FALSE)
         
         rules_list %>% lapply(
            
            function(rule) {
               
               tags$li(rule[["rule_name"]] %>% stringr::str_replace_all("_", " "))
               
            }
            
         )
         
      }
      
   })
   
   model_results <- eventReactive(input$train_model, {
      
      showModal(modalDialog("It takes a few seconds to train a good model...", footer = NULL))
      
      # Prepare Train & Test Sets
      set.seed(input$seed)
      n_train   <- round(n_rows * input$fraction_train)
      i_train   <- sample(1:n_rows, size = n_train)
      train_set <- clean_housing_dataset[i_train, ]
      test_set  <- clean_housing_dataset[-i_train, ]
      
      results <- pltr_learner(train_set, test_set, predictors_pairs)
      
      eval_metrics <- compute_evaluation_criteria(
         test_set$BAD %>% as.character() %>% as.numeric(), 
         results[['Predicted_Y_Test_Prob']], 
         results[['Predicted_Y_Test_Class']]
      )
      
      removeModal()
      
      list(
         
         "eval_metrics" = eval_metrics,
         
         "count_extracted_rules" = results[["count_extracted_rules"]],
         
         "coef_ranks" = results[["coef_ranks"]]
         
      )
      
   })
   
   output$eval_metrics <- renderUI({
      
      eval_metrics <- model_results()[["eval_metrics"]] %>% round(4)
      
      fluidRow(
         
         box(
            
            title = "TEST SET RESULTS",
            
            width = 12,
            
            valueBox(
               
               value    = eval_metrics$AUC,
               subtitle = "Area under the ROC Curve (AUC)",
               icon     = icon("chart-area"),
               width    = 4,
               color    = "blue"
               
            ),
            
            valueBox(
               
               value    = eval_metrics$GINI,
               subtitle = "GINI",
               icon     = icon("goodreads-g"),
               width    = 4,
               color    = "blue"
               
            ),
            
            valueBox(
               
               value    = eval_metrics$PCC,
               subtitle = "Percent of Correct Classifcation (PCC)",
               icon     = icon("product-hunt"),
               width    = 4,
               color    = "blue"
               
            ),
            
            valueBox(
               
               value    = eval_metrics$BS,
               subtitle = "Brier Score (BS)",
               icon     = icon("bold"),
               width    = 4,
               color    = "blue"
               
            ),
            
            valueBox(
               
               value    = eval_metrics$KS,
               subtitle = "Kolmogorov-Smirnov Statistic (KS)",
               icon     = icon("kickstarter-k"),
               width    = 4,
               color    = "blue"
               
            ),
            
            valueBox(
               
               value    = model_results()[["count_extracted_rules"]],
               subtitle = "New Rules Created",
               icon     = icon("columns"),
               width    = 4,
               color    = "blue"
               
            )
            
         )
         
      )
      
   })
   
   output$var_imp <- renderUI({

      coef_ranks <- model_results()[["coef_ranks"]] %>%
         dplyr::mutate_if(is.numeric, round, 4)

      div(
         
         fluidRow(
            
            box(
               
               title = "TOP-10 PREDICTORS",
               
               width = 6,
               
               renderPlot(
                  
                  coef_ranks %>%
                     head(n = 10) %>%
                     dplyr::mutate(
                        
                        Predictor = factor(Predictor, levels = Predictor[order(Coefficient_Magnitude)])
                        
                     ) %>%
                     ggplot(aes(x = Predictor, y = Coefficient_Magnitude)) +
                     geom_bar(stat = "identity", fill = "#f68060", alpha = .6, width = .4) +
                     coord_flip() +
                     xlab("") +
                     ylab("COEFFICIENT MAGNITUDE") +
                     theme_bw()
                  
               )
               
            ),
            
            box(
               
               title = "PREDICTORS IMPORTANCE - SORTED BY COEFFICIENTS MAGNITUDE",
               
               width = 6,
               
               height = "467px",
               
               br(),
               
               DT::renderDataTable(
                  
                  coef_ranks %>% dplyr::select(Predictor, Coefficient),
                  
                  class = "display nowrap",
                  
                  rownames= FALSE,
                  
                  options = list(
                     
                     scrollX = TRUE,
                     pageLength = 5
                     
                  )
                  
               )
               
            )
            
         ),
         
         fluidRow(
            
            column(
               
               width = 12,
               
               align = "center",
               
               h3("TRAIN AGAIN?"),
               
               br()
               
            )
            
         )
         
      )

   })
   
}




















