# create a list of elements from the split classical analysis

# org_split_ls = list of organised data outputted by organise_split
# rows_split_ls = row numbers to print elements of org_split_ls to
#                 in excel worksheets.

elems_split_classical <- function(
  org_split_ls,
  rows_split_ls
){
  
  # split out the list of elements,
  
  # set factor as the names of the data as the exist in the data 
  # frames
  
  # paste the list_el_names together with the type values to
  # get unique ids
  fd01 <- lapply(
    rows_split_ls,
    function(x){
      
      fd01 <- split(
        x,
        f = factor(
          x$list_el_name,
          levels = unique(x$list_el_name)
        )
      )
    }
  )
  
  fd01b <- lapply(
    fd01,
    function(x){
      
      lapply(
        x,
        function(y){
          
          split(
            y,
            f = factor(
              y$type,
              c("header", "data")
            )
          )
          
        }
      )
      
    }
  )
  
  fd02 <- names(fd01b)
  names(fd02) <- fd02
  
  # nested lapply to get the data type to be printed
  fd03 <- lapply(
    fd02,
    function(x){
      
      # lapply again over each demographic group's stats
      
      ffd01 <- names(fd01b[[x]])
      names(ffd01) <- ffd01
      
      lapply(
        ffd01,
        function(y){
          
          list(
            header = list(
              type = "header", 
              data = fd01b[[x]][[y]][["header"]][["list_el_name"]],  
              row = fd01b[[x]][[y]][["header"]][["start"]]
            ),
            data = list(
              type = "data", 
              data = org_split_ls[[x]][[y]],  
              row = fd01b[[x]][[y]][["data"]][["start"]]
            )
          )
          
        }
      )
      
    }
  )
  
  return(fd03)
  
}