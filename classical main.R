# main function to run the classical item analysis

# ld = long data list with the elements
#         var = a character value of the variable under investigation
#         level = a character value of the variable under investigation
#         data = a list of of long format data frames of the assessement data
# a_spec = analysis specification file
# dif_vars = character vector of person variables to be investigated for DIF
# wb = openxlsx workbook object for writing stat dataframes to.
# op = output directory

# returns a list of classical item analyses
# and writes the analysis to a standard classical item
# statistics Excel (.xlsx) workbook

classical <- function(
  ld_list,
  a_spec,
  dif_vars = NULL
  # wb,
  # op
){
  
  # assign the data to ld
  ld <- ld_list$data
  
  # create a list for the analysis to go
  
  d01 <- list()
  
  d01$`Test statistics` <- list(
    overview_stat = overview_stats(
      ld,
      a_spec
    ),
    `Cronbach's Alpha` = cronbach_alpha(
      ld,
      a_spec
    )
  )

  temp <- item_stats(
    ld,
    a_spec
  )
  
  # create item level stats
  d01$`Item stats` <- list(
    item_stats = temp$item_stats
  )  
  # create distractor stats
  d01$Distractor <- list(
   distractor = distractor(
     ld,
     a_spec,
     drop_tscore = temp$drop_tscore 
   )
  )  
  
  # cumulative item stats
  d01$`Score table` <- list(
    score_table = cumul(
      ld,
      a_spec
    )
  )  
    
  names(dif_vars) <- dif_vars
  
  # lapply over the DIF variable character vector
  #browser()
  
  # test if null on the ld_list$var and ld_list$level to make
  # that all cases are being analysed, not a selection of cases
  # by a demographic variable.
  if(
    is.null(ld_list$var) & is.null(ld_list$level) & !is.null(dif_vars)
  ){
    
    d02 <- lapply(
      dif_vars,
      function(x){
        
          dif_analysis = dif(
            ld,
            a_spec,
            x
          )
      }
    )
    
    names(d02) <- paste0(
      "DIF_",
      names(d02)
    )
    
    d03 <- append(
      d01,
      d02
    )
      
  }else{
    
    d03 <- d01
    
  }
  
  
  
  # comment out the writing functionality and return the list 
    
  # # for loop over the dataframe list to write the ouput out
  # for(i in names(d01)){
  #   
  #   # write each dataframe to the output workbook
  #   writeData(wb, i, d01[[i]], startCol = 1, startRow = 1)
  #   
  # }
  # 
  # for(i in names(d02)){
  #   
  #   # write each dataframe to the output workbook
  #   addWorksheet(wb, i)
  #   writeData(wb, i, d02[[i]]$DIF, startCol = 1, startRow = 1)
  #   
  # }
  # 
  # # write out the finished workbook to the directory specified by op
  # saveWorkbook(wb, op, overwrite = TRUE)
  
  # return the list of outputs
  return(
    d03
  )
  
}
