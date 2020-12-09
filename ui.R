library(shiny)
library(shinydashboard)
library(DT)


ui <- dashboardPage(
   
   dashboardHeader(title = 'Housing'),
   
   dashboardSidebar(disable = TRUE),
   
   dashboardBody(
      
      tabsetPanel(
         
         type = "pills",
         
         tabPanel(
            
            strong('Exploratory Analysis'),
            
            br(),
            
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
                  
                  title       = "Choose a Variable",
                  status      = "primary",
                  solidHeader = FALSE,
                  width       = 4,
                  uiOutput("colnames_select_input")
                  
               ),
               
               conditionalPanel(
                  
                  condition = "input.variable_name == '---'",
                  
                  box(
                     
                     title       = span(icon("database"), 
                                        HTML('&nbsp;'), 
                                        strong("Housing Dataset")),
                     status      = "primary",
                     solidHeader = TRUE,
                     width       = 8,
                     dataTableOutput("housing_datatable")
                     
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
                     htmlOutput("univariate_summary")
                     
                  ),
                  
               ),
               
            ),
            
         ),
         
         tabPanel(
            
            strong('Default Model')
            
         )
         
      )
      
   )
   
)