# function to take the long format from the file_formatter_main function and filter it
# by the filter variable by the specified levels. filtering is done by person variables.

#  ld = long format data, a list of 3 elements (person level data,
#       person by question level data and person by item level data).
# filt = filter list with the elements "var" (a character value) of the variable
#         to use in the filtering process
#         level = a character value of the character value to use when filtering the
#         the list by.
# a_spec = analysis specification excel file.

ld_splitter <- function(
  ld,
  filt,
  a_spec
){
  
  if(
    !is.null(filt[["var"]]) & !is.null(filt[["level"]])
  ){
    
    person_uid <- filter(
      a_spec[["var_def"]],
      `variable type` == "person uid"
    ) %>% pull(
      `variable name`
    )
    
    # filter the person level data, then have the necessary person ids to filter the
    # the remaining elements
    
    
    # filter the person level data so only the persons with the desirable
    # characteristics are present.
    fd01 <- filter(
      ld[["person"]],
      !!sym(filt[["var"]]) == filt[["level"]]
    )
    
    # filter each of the other person by question level data
    fd02 <- filter(
      ld[["person_q"]],
      !!sym(person_uid) %in% fd01[[person_uid]]
    )
    
    fd03 <- filter(
      ld[["person_i"]],
      !!sym(person_uid) %in% fd01[[person_uid]]
    )
    
    fd04 <- list(
      person = fd01,
      person_q = fd02,
      person_i = fd03
    )  
  }else{
    fd04 <- ld  
  }
  
  # put the list back together again
  return(
    fd04
  )
  
}