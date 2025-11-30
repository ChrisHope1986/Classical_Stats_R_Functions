# create the cummulative stats
# ld = long format data
cumul <- function(
  ld,
  a_spec
){
  
  # find the weighting variable
  weight <- filter(
    a_spec[["var_def"]],
    `variable type` == "weight"
  ) %>% pull(
    `variable name`
  )
  
  t_score <- filter(
    a_spec[["var_def"]],
    `variable type` == "total score"
  ) %>% pull(
    `variable name`
  )
  
  # find the max score possible on the assessment 
  fd01 <- select(
    ld$person_i,
    item,
    item_max_value
  ) %>% unique()
  
  # create a vector of 0 to the max possible score
  # turn this into a tibble
  fd02 <- tibble(
    score_range = as.numeric(0:sum(fd01[["item_max_value"]]))
  )

  # create a frequency summary
  fd03 <- group_by(
    ld$person,
    !!sym(t_score)
  ) %>% summarise(
    frequency = sum(!!sym(weight))
  ) %>% ungroup()
  
  # left join onto the possible score range
  fd04 <- left_join(
    fd02,
    fd03,
    by = c("score_range" = t_score)
  ) %>% mutate(
    frequency = ifelse(
      is.na(frequency),
      0,
      frequency
    ),
    percent = (frequency/sum(frequency)) * 100,
    cumulative_percent = cumsum(percent)
  )
  
  return(fd04)
  
}