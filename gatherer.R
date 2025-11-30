# gather function
# takes dataframe the unique ids and
# selection of variables to be gathered as arguments

# df = dataframe of wide data to be formatted as long data
# id = character vector of unique ids
# var_selec = character vector of variables to transform into long format data
# core_lookup = dataframe of mapping of common name of the question or item name
#               and derivative variables such as max, facility, omissions and not
#               reached values that are given for items, as an example.
var_gatherer <- function(
  df,
  id,
  var_selec,
  core_lookup
){
  
  # select the var_selec, vector of item or question derivative variables
  # to be transformed from wide to long format.
  # and the id columns also.
  fd01 <- select(
    df, 
    !!!syms(id),
    !!!syms(
      pull(
        core_lookup,
        !!sym(var_selec) 
      )
    )
  )
  
  # create key and value columns from the 
  # var_select coloumn in long_to_wide formating file.
  key <- paste0(var_selec,"_key")
  value <- paste0(var_selec, "_value")
  
  # gather up the data as the key and value pairs
  # keep the ids to the side of the dataframe in order
  # preserve the ids.
  fd02 <- gather(
    fd01,
    !!sym(key),
    !!sym(value),
    -c(!!!syms(id))
  )  

  # lookup the key names and join on the core_name
  
  # first select the variables needed for the lookup
  # the common name is always in the first position in
  # the lookup dataframe. and then select the var_selec
  # variable as well.
  fd03 <- select(
    core_lookup,
    c(
      1,
      !!!syms(
        var_selec
      )
    )
  ) 
  
  # join on the common variable onto the all
  # score data that has been converted to long format
  fd04 <- inner_join(
    rename(
      fd02,
      !!sym(var_selec) := !!sym(key)
    ),
    fd03,
    by = var_selec
  )
  
  # remove the key variable from the dataframe 
  # can now remove the derivative variable names from the
  # dataframe and keep just the common name and the derivative name
  # values.
  fd05 <- select(
    fd04,
    -c(!!sym(var_selec))
  ) %>% select(
    !!!syms(id),
    names(core_lookup)[1],
    !!sym(value)
  )

  return(fd05)
  
}