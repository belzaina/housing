library(readxl)


housing_datasets <- readxl::read_excel('hmeq.xls', sheet = 'hmeq')

n_rows <- nrow(housing_datasets)

n_cols <- ncol(housing_datasets)

missing_values <- round(
   100 * sum(is.na(housing_datasets)) / (n_rows * n_cols), 
   2
)


server <- function(input, output) {
   
   output$n_rows <- renderText(n_rows)
   output$n_cols <- renderText(n_cols)
   
   output$missing_values <- renderText(missing_values)
   
   output$colnames_select_input <- renderUI(
      selectInput("variable_name", "Please choose a variable:", c('---', colnames(housing_datasets)))
   )
   
   output$housing_datatable <- renderDataTable(
      housing_datasets,
      options = list(scrollX = TRUE)
   )
   
}