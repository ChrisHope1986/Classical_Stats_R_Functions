# rename the output tables just before they are written to
# an excel output file
# mapping_list = list of dataframes with the columns old names
#               and new names.
# df = dataframe with columns to be reanmed
# sh =  character vector of the worksheet to output the dataframe to. 
#       this also identifies the analsysi
# df_name = df_name within spreadsheet being printed
rename_output <- function(
  mapping_list,
  df,
  sh,
  df_name
){
  
  # identify the right lookup data fram in mapping_list, needs to be a regular
  # expression as df has a suffix of the demographic level after it.
  
  fd00 <- if(str_detect(sh, "DIF.*")){
    "DIF"
  }else{
    sh
  }
  
  fd01 <- grep(fd00, names(mapping_list))
  
  # need to find the df_name within the sheet
  
  # set to be false and then set to true only if a mapping list is found
  fd01b <- FALSE
  if(
    length(fd01) > 0
  ){
    fd01b <- df_name %in% names(mapping_list[[fd01]])
  }
    
  # if can't find an mapping table then just return the dataframe
  if(length(fd01) > 0 & fd01b){
  
    # set mapping list to be the dataframe indexed by fd01
    fd02 <- mapping_list[[fd01]][[df_name]]
    
    fd03 <- tibble(
      df_names = names(df)
    ) %>% left_join(
      fd02,
      by = c("df_names" = "old_name")
    ) %>% mutate(
      new_name = ifelse(
        is.na(new_name),
        df_names,
        new_name
      )
    )
    
    fd04 <- df
    names(fd04) <- fd03$new_name
    
  }else{
    fd04 <- df
  }
  
  return(fd04)
  
}