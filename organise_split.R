# function to organise the split data so that the analysis outer demographic layer becomes the inside layer
# and the inner levels of analysis type become the outer layer

# takes the output from the run_split_classical as its input

#   parameters
#     rsc_ls = run split stats list. a list object outputted from the run_split_classical function.

# returns a reorganised list of data

organise_split <- function(
  rsc_ls
){
  
  # lapply over each of 
  
  # get unique demographic variable names within each demographic
  # variable
  
  # could just use a for loop?
  
  fd01 <- list()
  
  # lapply over booklets
  for(x in names(rsc_ls)){
      
      # lapply over demographic variables
      for(y in names(rsc_ls[[x]])){
          
          # lapply over demographic variable levels
          for(z in names(rsc_ls[[x]][[y]])){
            
            # loop over sheet names
            for(a in names(rsc_ls[[x]][[y]][[z]]$classical)){
              
              #browser()
              
              fd01[[x]][[y]][[a]][[z]] = rsc_ls[[x]][[y]][[z]]$classical[[a]]
              
            } # next a - sheet names
              
          } # end z - lapply over demographic varialbe levels

      }# end y - lapply demographic variable
      
  }# end x lapply - booklet
  
  return(fd01)
  
}