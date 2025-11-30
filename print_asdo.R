# print the analysis specification demo options

# parameters
#   asdo = analysis specification demo options
#   wb = workbook to print the data specified in asdo to. should be an analysis specification
#         template.
print_asdo <- function(
  asdo,
  wb
){
  
  for(
    i in names(asdo)
  ){
    
    writeDataTable(
      wb,
      "demo_options_data", 
      asdo[[i]][["data"]],
      startCol = asdo[[i]][["start_col"]],
      startRow = 1,
      tableName = i
    )  
    
  }
  
}