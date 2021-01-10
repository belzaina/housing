library(shiny)
library(shinydashboard)
library(magrittr)


source("data/metadata.R")


ui <- dashboardPage(
   
   dashboardHeader(title = 'Big Data - M2 ESA'),
   
   dashboardSidebar(
      
      sidebarMenu(
         
         menuItem("INTRODUCTION", tabName = "introduction", icon = icon("door-open")),
         
         menuItem("EXPLORE THE DATASET", tabName = "explore_dataset", icon = icon("chart-bar")),
         
         menuItem("MODELS", tabName = "models", icon = icon("chart-line")),
         
         menuItem("DIETTERICH N x 2-FOLD CV", tabName = "cv", icon = icon("chart-line")),
         
         menuItem("REPRODUCIBLE RESEARCH", tabName = "reproducible_research", icon = icon("laptop-code")),
         
         menuItem("ABOUT", tabName = "about", icon = icon("at"))
         
      )
      
   ),
   
   dashboardBody(
      
      tags$head(
         
         includeCSS(path = "./www/main.css"),
         
         HTML(
            
            '<link rel="icon" href="logo_title.ico">'
            
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
                     
                     tabPanel(
                        
                        title = "1. OUTLINE",
                        
                        br(),
                        
                        p(metadata[["INTRO_1"]]),
                        
                        br(),
                        
                        tags$ul(
                           
                           tags$li(metadata[["INTRO_2"]]),
                           
                           tags$li(metadata[["INTRO_3"]])
                           
                        ),
                        
                        br(),
                        
                        p(metadata[["INTRO_4"]]),
                        
                        br(),
                        
                        tags$ul(
                           
                           tags$li(metadata[["INTRO_5"]]),
                           
                           tags$li(metadata[["INTRO_6"]])
                           
                        )
                        
                     ),
                     
                     tabPanel(
                        
                        title = "2. SHOW ME SOME RULES!",
                        
                        br(),
                        
                        box(
                           
                           width = 3,
                           
                           status = "info",
                           
                           solidHeader = TRUE,
                           
                           title = "PREDICTORS SET",
                           
                           br(),
                           
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
                        
                        uiOutput("eval_metrics"),
                        
                        uiOutput("var_imp"),
                        
                        fluidRow(
                           
                           column(
                              
                              offset = 3,
                              
                              width = 12,
                              
                              box(
                                 
                                 title = "TRAINING PARAMETERS",
                                 
                                 width = 6,
                                 
                                 numericInput("seed", label = h4("Seed"), value = 8081, min = 1),
                                 
                                 sliderInput("fraction_train", label = h4("Fraction Used for Training"), 
                                             min = 0.1, max = 0.9, value = 0.5, step = 0.1),
                                 
                                 br(),
                                 
                                 actionButton("train_model", h4("Train & Test"), width = '100%')
                                 
                              )
                              
                           )
                           
                        )
                        
                     ),
                     
                     tabPanel(
                        
                        title = "Random Forest",
                        
                        br(), br()
                        
                     ),
                     
                     tabPanel(
                        
                        title = "Support Vector Machine",
                        
                        br(), br()
                        
                     ),
                     
                     tabPanel(
                        
                        title = "Linear Logistic Regression",
                        
                        br(), br()
                        
                     ),
                     
                     tabPanel(
                        
                        title = "Non-Linear Logistic Regression",
                        
                        br(), br()
                        
                     )
                     
                  )
                  
               )
               
            )
            
         ),
         
         tabItem(
            
            tabName = "cv",
            
         ),
         
         tabItem(
            
            tabName = "reproducible_research",
            
         ),
         
         tabItem(
            
            tabName = "about",
            
         )
         
      ),
      
   ),
   
)