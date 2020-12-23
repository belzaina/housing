library(readxl)
library(magrittr)


housing_dataset <- readxl::read_excel('data/hmeq.xls', sheet = 'hmeq')

n_rows <- nrow(housing_dataset)

n_cols <- ncol(housing_dataset)

missing_values <- round(
   100 * sum(is.na(housing_dataset)) / (n_rows * n_cols), 
   2
)


server <- function(input, output) {
   
   output$n_rows <- renderText(n_rows)
   output$n_cols <- renderText(n_cols)
   
   output$missing_values <- renderText(missing_values)
   
   output$colnames_select_input <- renderUI(
      selectInput("variable_name", 
                  "Please choose a variable:", 
                  c('---', sort(colnames(housing_dataset), decreasing = TRUE)))
   )
   
   output$housing_datatable <- renderDataTable(
      housing_dataset,
      options = list(scrollX = TRUE)
   )
   
   output$summary_stats <- shiny::renderUI({
      
      if ((!is.null(input$variable_name)) && (input$variable_name != '---')) {
         
         summarytools::st_options(
            footnote = NA,
            headings = TRUE,
            plain.ascii = FALSE,
            dfSummary.varnumbers = FALSE
         )
         
         univ_summary <- housing_dataset %>%
            dplyr::select(input$variable_name) %>%
            summarytools::dfSummary()
         
         attr(univ_summary, "data_info")$Data.frame <- paste("Feature:", input$variable_name)
         
         univ_summary %<>% print(method = "render")
         
         bivar_summary <- housing_dataset %>%
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
   
}