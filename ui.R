library(shiny)
library(shinydashboard)
library(magrittr)


source("data/metadata.R")


ui <- dashboardPage(
   
   dashboardHeader(title = 'Big Data - M2 ESA'),
   
   dashboardSidebar(
      
      sidebarMenu(
         
         id = "main_menu",
         
         menuItem("INTRODUCTION", tabName = "introduction", icon = icon("door-open")),
         
         menuItem("EXPLORE THE DATASET", tabName = "explore_dataset", icon = icon("chart-bar")),
         
         menuItem("MODELS TESTING", tabName = "models", icon = icon("chart-line")),
         
         menuItem("ROBUSTNESS CHECK", tabName = "cv", icon = icon("microscope")),
         
         menuItem("REPRODUCIBLE RESEARCH", tabName = "reproducible_research", icon = icon("laptop-code")),
         
         menuItem("ABOUT", tabName = "about", icon = icon("at"))
         
      )
      
   ),
   
   dashboardBody(
      
      tags$head(
         
         includeCSS(path = "./www/main.css"),
         
         HTML(
            
            '<link rel="icon" href="logo_title.jpg">'
            
         )
         
      ),
      
      tabItems(
         
         tabItem(
            
            tabName = "introduction",
            
            fluidRow(
               
               h1("PENALISED LOGISTIC TREE REGRESSION"),
               
               align = "center"
               
            ),
            
            br(), br(),
            
            fluidRow(
               
               box(
                  
                  width = 12,
                  
                  tabsetPanel(
                     
                     id = "panel_intro",
                     
                     tabPanel(
                        
                        title = "1. OUTLINE",
                        
                        br(),
                        
                        p(metadata[["INTRO_1"]]),
                        
                        tags$ul(
                           
                           tags$li(metadata[["INTRO_2"]]),
                           
                           tags$li(metadata[["INTRO_3"]])
                           
                        ), br(),
                        
                        p(metadata[["INTRO_4"]]), 
                        
                        tags$ul(
                           
                           tags$li(metadata[["INTRO_5"]], 
                                   actionLink("link_to_demo",strong("(SHOW INTERACTIVE DEMO)"))),
                           
                           tags$li(metadata[["INTRO_6"]])
                           
                        ), br(),
                        
                        p("This application allows you to:"),
                        
                        tags$ul(
                           
                           tags$li(actionLink("link_to_vis", strong("Visualize interactively how PLTR extract new rules"))),
                           
                           tags$li(actionLink("link_to_explore", strong("Explore the housing dataset"))),
                           
                           tags$li(actionLink("link_to_train", strong("Train and test learning algorithms (including an analysis of predictors importance)"))),
                           
                           tags$li(actionLink("link_to_cv", strong("Perform robustness checking using N x 2-fold cross-validation"))),
                           
                           tags$li(actionLink("link_to_tuto", strong("Follow guided tutorials in case you wish to reproduce our results")))
                           
                        )
                        
                     ),
                     
                     tabPanel(
                        
                        title = "2. SHOW ME SOME RULES!",
                        
                        value = "panel_demo",
                        
                        br(),
                        
                        box(
                           
                           width = 3,
                           
                           status = "info",
                           
                           solidHeader = TRUE,
                           
                           title = "PREDICTORS SET",
                           
                           h4("Choose among:"),
                           
                           uiOutput("predictors_set")
                           
                        ),
                        
                        conditionalPanel(
                           
                           condition = "typeof input.predictors_set !== 'undefined' && 
                           input.predictors_set.length == 0",
                           
                           box(
                              
                              width = 9,
                              
                              status = "warning",
                              
                              solidHeader = TRUE,
                              
                              title = span(icon("exclamation-triangle"), HTML("&nbsp"), "WARNING"),
                              
                              "PLEASE SELECT A SET OF PREDICTORS TO CONSIDER IN EACH SPLIT."
                              
                           )
                           
                        ),
                        
                        conditionalPanel(
                           
                           condition = "typeof input.predictors_set !== 'undefined' && 
                           input.predictors_set.length > 0",
                           
                           box(
                              
                              width = 6,
                              
                              status = "info",
                              
                              solidHeader = TRUE,
                              
                              title = "2-SPLIT TREE",
                              
                              plotOutput("two_split_tree")
                              
                           ),
                           
                           box(
                              
                              width = 3,
                              
                              status = "info",
                              
                              solidHeader = TRUE,
                              
                              title = "EXTRACTED RULES",
                              
                              p(metadata[['EXTRACTED_RULES']]),
                              
                              tags$ul(
                                 
                                 uiOutput("extracted_rules")
                                 
                              )
                              
                           )
                           
                        )
                        
                     ),
                     
                     tabPanel(
                        
                        title = "3. MAIN RESULTS",
                        
                        br(),
                        
                        p("Our 5 x 2-fold cross-validation results are as follow: "),
                        
                        br(),
                        
                        fluidRow(
                           
                           column(
                              
                              width = 6,
                              
                              offset = 2,
                              
                              plotOutput("main_results")
                              
                           ),
                           
                           column(
                              
                              width = 4,
                              
                              actionLink("link_to_notebook",strong("(SHOW COMPLETE NOTEBOOK)"))
                              
                           )
                           
                        )
                        
                     ),
                     
                     tabPanel(
                        
                        title = "4. REFERENCE",
                        
                        br(),
                        
                        p("The main reference for this project is:"), br(),
                        
                        em(markdown(metadata[['REFERENCE']])),
                        
                        br(),
                        
                        em(markdown("<sup>[1](#)</sup> EconomiX-CNRS, University of Paris Nanterre, 200 Avenue de la République, 92000 Nanterre, France. E-mail: elena.dumitrescu@parisnanterre.fr")),
                        
                        em(markdown("<sup>[2](#)</sup> Corresponding author, Univ. Orléans, CNRS, LEO (FRE 2014), Rue de Blois, 45067 Orléans. E-mail: sullivan.hue@univ-orleans.fr")),
                        
                        em(markdown("<sup>[3](#)</sup> Univ. Orléans, CNRS, LEO (FRE 2014), Rue de Blois, 45067 Orléans. E-mail: christophe.hurlin@univ-orleans.fr")),
                        
                        em(markdown("<sup>[4](#)</sup> Univ. Orléans, CNRS, LEO (FRE 2014), Rue de Blois, 45067 Orléans. E-mail: sessi.tokpavi@univ-orleans.fr"))
                        
                     )
                     
                  )
                  
               )
               
            )
            
         ),
         
         tabItem(
            
            tabName = "explore_dataset",
            
            fluidRow(
               
               valueBox(
                  
                  value    = textOutput("n_rows"),
                  subtitle = "Number of Rows",
                  icon     = icon("list"),
                  width    = 4,
                  color    = "blue"
                  
               ),
               
               valueBox(
                  
                  value    = textOutput("n_cols"),
                  subtitle = "Number of Variables",
                  icon     = icon("columns"),
                  width    = 4,
                  color    = "blue"
                  
               ),
               
               valueBox(
                  
                  value    = textOutput("missing_values"),
                  subtitle = "Missing Values",
                  icon     = icon("percent"),
                  width    = 4,
                  color    = "blue"
                  
               ),
               
            ),
            
            fluidRow(
               
               box(
                  
                  title       = "Exploratory Data Analysis",
                  status      = "primary",
                  solidHeader = FALSE,
                  width       = 4,
                  
                  uiOutput("colnames_select_input"),
                  
                  conditionalPanel(
                     
                     condition = "input.variable_name === '---'",
                     
                     radioButtons(
                        "dataset_version",
                        "Dataset Version:",
                        c(
                           "Raw Dataset" = "raw", 
                           "Pre-Processed Dataset" = "clean",
                           "Pre-Processed + Interaction & Quadratic Terms" = "iqt"
                        )
                     )
                     
                  ),
                  
               ),
               
               conditionalPanel(
                  
                  condition = "input.variable_name === '---'",
                  
                  box(
                     
                     title       = span(icon("database"), 
                                        HTML('&nbsp;'), 
                                        strong("Housing Dataset")),
                     status      = "primary",
                     solidHeader = TRUE,
                     width       = 8,
                     DT::dataTableOutput("housing_datatable")
                     
                  ),
                  
               ),
               
               conditionalPanel(
                  
                  condition = "input.variable_name !== '---'",
                  
                  box(
                     
                     title       = span(icon("database"), 
                                        HTML('&nbsp;'), 
                                        strong("Housing Dataset")),
                     status      = "primary",
                     solidHeader = TRUE,
                     width       = 8,
                     htmlOutput("summary_stats")
                     
                  ),
                  
               ),
               
            ),
            
         ),
         
         tabItem(
            
            tabName = "models",
            
            fluidRow(
               
               box(
                  
                  width = 12,
                  
                  tabsetPanel(
                     
                     tabPanel(
                        
                        title = "Penalized Logistic Tree Regression",
                        
                        br(),
                        
                        uiOutput("pltr_eval_metrics"),
                        
                        uiOutput("pltr_var_imp"),
                        
                        fluidRow(
                           
                           column(
                              
                              offset = 3,
                              
                              width = 12,
                              
                              box(
                                 
                                 title = "TRAINING PARAMETERS",
                                 
                                 width = 6,
                                 
                                 numericInput("pltr_seed", label = h4("Seed"), value = 8081, min = 1),
                                 
                                 sliderInput("pltr_fraction_train", label = h4("Fraction Used for Training"), 
                                             min = 0.1, max = 0.9, value = 0.5, step = 0.1),
                                 
                                 radioButtons(
                                    "pltr_penalty", 
                                    h4("Penalty"),
                                    c("Adaptive LASSO"      = 2,
                                      "LASSO"               = 1,
                                      "Ridge"               = 0
                                    )
                                 ),
                                 
                                 br(),
                                 
                                 actionButton("pltr_train_button", h4("Train & Test"), width = '100%')
                                 
                              )
                              
                           )
                           
                        )
                        
                     ),
                     
                     tabPanel(
                        
                        title = "Random Forest",
                        
                        br(),
                        
                        uiOutput("rf_eval_metrics"),
                        
                        uiOutput("rf_var_imp"),
                        
                        fluidRow(
                           
                           column(
                              
                              offset = 3,
                              
                              width = 12,
                              
                              box(
                                 
                                 title = "TRAINING PARAMETERS",
                                 
                                 width = 6,
                                 
                                 numericInput("rf_seed", label = h4("Seed"), value = 8081, min = 1),
                                 
                                 sliderInput("rf_fraction_train", label = h4("Fraction Used for Training"), 
                                             min = 0.1, max = 0.9, value = 0.5, step = 0.1),
                                 
                                 radioButtons(
                                    "rf_var_imp_metric", 
                                    h4("Variable Importance Measure"),
                                    c("Mean Decrease in Accuracy" = 1, 
                                      "Mean Decrease in Node Impurity" = 2)
                                 ),
                                 
                                 br(),
                                 
                                 actionButton("rf_train_button", h4("Train & Test"), width = '100%')
                                 
                              )
                              
                           )
                           
                        )
                        
                     ),
                     
                     tabPanel(
                        
                        title = "Support Vector Machine",
                        
                        br(),
                        
                        uiOutput("svm_eval_metrics"),
                        
                        fluidRow(
                           
                           column(
                              
                              offset = 3,
                              
                              width = 12,
                              
                              box(
                                 
                                 title = "TRAINING PARAMETERS",
                                 
                                 width = 6,
                                 
                                 numericInput("svm_seed", label = h4("Seed"), value = 8081, min = 1),
                                 
                                 sliderInput("svm_fraction_train", label = h4("Fraction Used for Training"), 
                                             min = 0.1, max = 0.9, value = 0.5, step = 0.1),
                                 
                                 radioButtons(
                                    "svm_kernel", 
                                    h4("Kernel"),
                                    c("Radial" = "radial", 
                                      "Polynomial" = "polynomial")
                                 ),
                                 
                                 br(),
                                 
                                 actionButton("svm_train_button", h4("Train & Test"), width = '100%')
                                 
                              )
                              
                           )
                           
                        )
                        
                     ),
                     
                     tabPanel(
                        
                        title = "Linear Logistic Regression",
                        
                        br(),
                        
                        uiOutput("llr_eval_metrics"),
                        
                        uiOutput("llr_var_imp"),
                        
                        fluidRow(
                           
                           column(
                              
                              offset = 3,
                              
                              width = 12,
                              
                              box(
                                 
                                 title = "TRAINING PARAMETERS",
                                 
                                 width = 6,
                                 
                                 numericInput("llr_seed", label = h4("Seed"), value = 8081, min = 1),
                                 
                                 sliderInput("llr_fraction_train", label = h4("Fraction Used for Training"), 
                                             min = 0.1, max = 0.9, value = 0.5, step = 0.1),
                                 
                                 radioButtons(
                                    "llr_penalty", 
                                    h4("Penalty"),
                                    c("No Penalty"          = -1, 
                                      "Ridge"               = 0,
                                      "LASSO"               = 1,
                                      "Adaptive LASSO"      = 2)
                                 ),
                                 
                                 br(),
                                 
                                 actionButton("llr_train_button", h4("Train & Test"), width = '100%')
                                 
                              )
                              
                           )
                           
                        )
                        
                     ),
                     
                     tabPanel(
                        
                        title = "Non-Linear Logistic Regression",
                        
                        br(),
                        
                        uiOutput("nllr_eval_metrics"),
                        
                        uiOutput("nllr_var_imp"),
                        
                        fluidRow(
                           
                           column(
                              
                              offset = 3,
                              
                              width = 12,
                              
                              box(
                                 
                                 title = "TRAINING PARAMETERS",
                                 
                                 width = 6,
                                 
                                 numericInput("nllr_seed", label = h4("Seed"), value = 8081, min = 1),
                                 
                                 sliderInput("nllr_fraction_train", label = h4("Fraction Used for Training"), 
                                             min = 0.1, max = 0.9, value = 0.5, step = 0.1),
                                 
                                 radioButtons(
                                    "nllr_penalty", 
                                    h4("Penalty"),
                                    c("Ridge"               = 0,
                                      "LASSO"               = 1,
                                      "Adaptive LASSO"      = 2)
                                 ),
                                 
                                 br(),
                                 
                                 actionButton("nllr_train_button", h4("Train & Test"), width = '100%')
                                 
                              )
                              
                           )
                           
                        )
                        
                     )
                     
                  )
                  
               )
               
            )
            
         ),
         
         tabItem(
            
            tabName = "cv",
            
            fluidRow(
               
               column(
                  
                  width = 12,
                  
                  box(
                     
                     width = 12,
                     
                     tabsetPanel(
                        
                        tabPanel(
                           
                           title = "Penalized Logistic Tree Regression",
                           
                           br(),
                           
                           uiOutput("pltr_cv_eval_metrics"),
                           
                           fluidRow(
                              
                              column(
                                 
                                 width = 6,
                                 
                                 box(
                                    
                                    title = "TRAINING PARAMETERS",
                                    
                                    width = 12,
                                    
                                    numericInput("pltr_cv_seed", label = h4("Seed"), value = 8081, min = 1),
                                    
                                    numericInput("pltr_cv_n", label = h4("N"), value = 5, min = 1, max = 10),
                                    
                                    radioButtons(
                                       "pltr_cv_penalty", 
                                       h4("Penalty"),
                                       c("Adaptive LASSO"      = 2,
                                         "LASSO"               = 1,
                                         "Ridge"               = 0
                                       )
                                    ),
                                    
                                    br(),
                                    
                                    actionButton("pltr_cv_button", h4("DIETTERICH N x 2-FOLD CROSS-VALIDATION"), width = '100%')
                                    
                                 )
                                 
                              ),
                              
                              column(
                                 
                                 width = 6,
                                 
                                 box(
                                    
                                    width = 12,
                                    
                                    height = "471.8px",
                                    
                                    title = "CROSS VALIDATION RESULTS",
                                    
                                    br(),
                                    
                                    DT::dataTableOutput("pltr_cv_dt")
                                    
                                 )
                                 
                              )
                              
                           )
                           
                        ),
                        
                        tabPanel(
                           
                           title = "Random Forest",
                           
                           br(),
                           
                           uiOutput("rf_cv_eval_metrics"),
                           
                           fluidRow(
                              
                              column(
                                 
                                 width = 6,
                                 
                                 box(
                                    
                                    title = "TRAINING PARAMETERS",
                                    
                                    width = 12,
                                    
                                    height = "471.8px",
                                    
                                    numericInput("rf_cv_seed", label = h4("Seed"), value = 8081, min = 1),
                                    
                                    numericInput("rf_cv_n", label = h4("N"), value = 5, min = 1, max = 10),
                                    
                                    br(),
                                    
                                    actionButton("rf_cv_button", h4("DIETTERICH N x 2-FOLD CROSS-VALIDATION"), width = '100%')
                                    
                                 )
                                 
                              ),
                              
                              column(
                                 
                                 width = 6,
                                 
                                 box(
                                    
                                    width = 12,
                                    
                                    height = "471.8px",
                                    
                                    title = "CROSS VALIDATION RESULTS",
                                    
                                    br(),
                                    
                                    DT::dataTableOutput("rf_cv_dt")
                                    
                                 )
                                 
                              )
                              
                           )
                           
                        ),
                        
                        tabPanel(
                           
                           title = "Support Vector Machine",
                           
                           br(),
                           
                           uiOutput("svm_cv_eval_metrics"),
                           
                           fluidRow(
                              
                              column(
                                 
                                 width = 6,
                                 
                                 box(
                                    
                                    title = "TRAINING PARAMETERS",
                                    
                                    width = 12,
                                    
                                    height = "471.8px",
                                    
                                    numericInput("svm_cv_seed", label = h4("Seed"), value = 8081, min = 1),
                                    
                                    numericInput("svm_cv_n", label = h4("N"), value = 5, min = 1, max = 10),
                                    
                                    radioButtons(
                                       "svm_cv_kernel", 
                                       h4("Kernel"),
                                       c("Radial" = "radial", 
                                         "Polynomial" = "polynomial")
                                    ),
                                    
                                    br(),
                                    
                                    actionButton("svm_cv_button", h4("DIETTERICH N x 2-FOLD CROSS-VALIDATION"), width = '100%')
                                    
                                 )
                                 
                              ),
                              
                              column(
                                 
                                 width = 6,
                                 
                                 box(
                                    
                                    width = 12,
                                    
                                    height = "471.8px",
                                    
                                    title = "CROSS VALIDATION RESULTS",
                                    
                                    br(),
                                    
                                    DT::dataTableOutput("svm_cv_dt")
                                    
                                 )
                                 
                              )
                              
                           )
                           
                        ),
                        
                        tabPanel(
                           
                           title = "Linear Logistic Regression",
                           
                           br(),
                           
                           uiOutput("llr_cv_eval_metrics"),
                           
                           fluidRow(
                              
                              column(
                                 
                                 width = 6,
                                 
                                 box(
                                    
                                    title = "TRAINING PARAMETERS",
                                    
                                    width = 12,
                                    
                                    numericInput("llr_cv_seed", label = h4("Seed"), value = 8081, min = 1),
                                    
                                    numericInput("llr_cv_n", label = h4("N"), value = 5, min = 1, max = 10),
                                    
                                    radioButtons(
                                       "llr_cv_penalty", 
                                       h4("Penalty"),
                                       c("No Penalty"          = -1, 
                                         "Ridge"               = 0,
                                         "LASSO"               = 1,
                                         "Adaptive LASSO"      = 2)
                                    ),
                                    
                                    br(),
                                    
                                    actionButton("llr_cv_button", h4("DIETTERICH N x 2-FOLD CROSS-VALIDATION"), width = '100%')
                                    
                                 )
                                 
                              ),
                              
                              column(
                                 
                                 width = 6,
                                 
                                 box(
                                    
                                    width = 12,
                                    
                                    height = "500.8px",
                                    
                                    title = "CROSS VALIDATION RESULTS",
                                    
                                    br(),
                                    
                                    DT::dataTableOutput("llr_cv_dt")
                                    
                                 )
                                 
                              )
                              
                           )
                           
                        ),
                        
                        tabPanel(
                           
                           title = "Non-Linear Logistic Regression",
                           
                           br(),
                           
                           uiOutput("nllr_cv_eval_metrics"),
                           
                           fluidRow(
                              
                              column(
                                 
                                 width = 6,
                                 
                                 box(
                                    
                                    title = "TRAINING PARAMETERS",
                                    
                                    width = 12,
                                    
                                    numericInput("nllr_cv_seed", label = h4("Seed"), value = 8081, min = 1),
                                    
                                    numericInput("nllr_cv_n", label = h4("N"), value = 5, min = 1, max = 10),
                                    
                                    radioButtons(
                                       "nllr_cv_penalty", 
                                       h4("Penalty"),
                                       c("Ridge"               = 0,
                                         "LASSO"               = 1,
                                         "Adaptive LASSO"      = 2)
                                    ),
                                    
                                    br(),
                                    
                                    actionButton("nllr_cv_button", h4("DIETTERICH N x 2-FOLD CROSS-VALIDATION"), width = '100%')
                                    
                                 )
                                 
                              ),
                              
                              column(
                                 
                                 width = 6,
                                 
                                 box(
                                    
                                    width = 12,
                                    
                                    height = "471.8px",
                                    
                                    title = "CROSS VALIDATION RESULTS",
                                    
                                    br(),
                                    
                                    DT::dataTableOutput("nllr_cv_dt")
                                    
                                 )
                                 
                              )
                              
                           )
                           
                        )
                        
                     )
                     
                  )
                  
               )
               
            )
            
         ),
         
         tabItem(
            
            tabName = "reproducible_research",
            
            fluidRow(
               
               column(
                  
                  width = 3,
                  
                  box(
                     
                     width = 12,
                     
                     solidHeader = TRUE, 
                     
                     status = "info",
                     
                     title = "Choose a tutorial:",
                     
                     selectInput(
                        
                        "tutorial",
                        
                        NULL,
                        
                        multiple = FALSE,
                        
                        choices = list(
                           
                           "Robustness Check" = "robustness_check",
                           "PLTR: Predictors Importance" = "pltr_var_imp"
                           
                        )
                        
                     )
                     
                  )
                  
               ),
               
               column(
                  
                  width = 9,
                  
                  conditionalPanel(
                     
                     condition = "input.tutorial == 'robustness_check'",
                     
                     fluidRow(
                        
                        tags$iframe(
                           
                           style = "height:650px; width:100%", 
                           src = "vignettes/robustness_check.pdf"
                           
                        ),
                        
                        align = "center"
                        
                     )
                     
                  ),
                  
                  conditionalPanel(
                     
                     condition = "input.tutorial == 'pltr_var_imp'",
                     
                     fluidRow(
                        
                        tags$iframe(
                           
                           style = "height:650px; width:100%", 
                           src = "vignettes/pltr_var_importance.pdf"
                           
                        ),
                        
                        align = "center"
                        
                     )
                     
                  )
                  
               )
               
            )
            
         ),
         
         tabItem(
            
            tabName = "about",
            
            br(),
            
            fluidRow(
               
               column(
                  
                  width = 12,
                  
                  align = "center",
                  
                  img(src = "Logo-couleur-MasterESA-RVB.jpg")
                  
               )
               
            ),
            
            br(), br(),
            
            fluidRow(
               
               box(
                  
                  width = 6,
                  
                  status = "primary",
                  
                  solidHeader = FALSE,
                  
                  style = "height: 280px;",
                  
                  title = ("About This Project"),
                  
                  markdown(
                     
                     metadata[['ABOUT_PROJECT_1']]
                     
                  ),
                  
                  p(metadata[['ABOUT_PROJECT_2']]),
                  
                  em(markdown(
                     
                     metadata[['ABOUT_PROJECT_3']]
                     
                  ))
                  
               ),
               
               box(
                  
                  title = "Developers",
                  
                  width = 6,
                  
                  status = "primary",
                  
                  solidHeader = FALSE,
                  
                  style = "height: 280px;",
                  
                  br(),
                  
                  fluidRow(
                     
                     column(
                        
                        width = 6, 
                        
                        align = "center",
                        
                        img(
                           
                           class = "img-responsive img-rounded center-block", 
                           
                           src = "zainab_belgada.jpg"
                           
                        ),
                        
                        br(),
                        
                        p(
                           
                           span(icon("envelope"), HTML('&nbsp;'), 
                                "zainab.belgada@etu.univ-orleans.fr"),
                           
                           style = "font-size: 15px;"
                           
                        ),
                        
                        p(
                           
                           a(icon("linkedin", "fa-2x"), HTML('&nbsp;'), HTML('&nbsp;'), href = "https://fr.linkedin.com/in/za%C3%AFnab-belgada-b1175b1ab"),
                           a(icon("github", "fa-2x"), href = "https://github.com/belzaina", style = "color: inherit;")
                           
                        )
                        
                     ),
                     
                     column(
                        
                        width = 6, 
                        
                        align = "center",
                        
                        img(
                           
                           class = "img-responsive img-rounded center-block", 
                           
                           src = "damien_zinsou.jpg"
                           
                        ),
                        
                        br(),
                        
                        p(
                           
                           span(icon("envelope"), HTML('&nbsp;'), 
                                "zinsou.mezonlin@etu.univ-orleans.fr"),
                           
                           style = "font-size: 15px;"
                           
                        ),
                        
                        p(
                           
                           a(icon("linkedin", "fa-2x"), href = "https://www.linkedin.com/in/zinsou-damien-m-7073861b6/")
                           
                        )
                        
                     )
                     
                  )
                  
               )
               
            )
            
         )
         
      )
      
   )
   
)