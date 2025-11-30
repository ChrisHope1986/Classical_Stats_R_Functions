# calculate weighted variance
#   x = values to calculate variance of
#   weight = values to weight x by
weighted_var <- function(
  x,
  weight
){
  
  # find the weighted mean
  fd01 <-  sum(x * weight)/sum(weight)
  
  # find sum of squares
  fd02 <- sum((x - fd01)^2 * weight)
  
  # get variance
  fd03 <- fd02/sum(weight)
  
  return(fd03)
  
}