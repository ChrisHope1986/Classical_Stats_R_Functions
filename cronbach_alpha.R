# compute cronbach alpha according to the frisbie paper (1988)
  # ld = list of long data

  # return value is a data frame of one value for cronbach's alpha.

cronbach_alpha <- function(
  ld,
  a_spec
){
  
  # find number of items k
  fd01 <- unique(
    ld$person_i$item
  ) %>% length()
  
  # get the person weight variable
  fd01b <- filter(
    a_spec$var_def,
    `variable type` == "weight" 
  ) %>% pull(
    `variable name`
  )
  
  # get the person id variable
  fd01c <- filter(
    a_spec$var_def,
    `variable type` == "person uid" 
  ) %>% pull(
    `variable name`
  )
  
  # get total score values
  fd01c2 <- filter(
    a_spec$var_def,
    `variable type` == "total score" 
  ) %>% pull(
    `variable name`
  )
  
  
  # get join on the person weights onto the item level data
  fd01d <- select(
    ld$person,
    !!!syms(
      c(
        fd01c,
        fd01b
      )
    )
  )
  
  fd01e <- inner_join(
    ld$person_i,
    fd01d,
    by = fd01c
  )

  # find sum of variance on items
  #   find variance on each item score
  #   group by item and find variance
  #   then sum
  fd02 <- group_by(
    fd01e,
    item
  ) %>% summarise(
    var = weighted_var(item_score_value, !!sym(fd01b)),
    .groups = "drop"
  ) %>% ungroup(
  ) %>% pull(var) %>% sum()
  
  # find variance on total scores
  fd03 <- weighted_var(ld$person[[fd01c2]], ld$person[[fd01b]])
  
  fd04 <- (fd01/(fd01 - 1)) * (1 - (fd02/fd03))
  
  return(
    tibble(
      "Cronbach's Alpha" = fd04
    )
  )
  
}