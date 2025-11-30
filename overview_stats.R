# requires packages spatstat and radiant.data 
# for weighted.median and weighted.sd

# return overview assessment of mean total score,
# median total score, the number of persons who have
# taken the assessment etc.

# ld = list of person, item, question long data
# a_spec = analysis specification which is a list of
#           variable definition and the question to item
#           mapping.

overview_stats <- function(
  ld,
  a_spec
){
  
  # find out what the total score variable name is from
  # the variable definition dataframe
  t_score <- filter(
    a_spec[["var_def"]],
    `variable type` == "total score"
  ) %>% pull(
    `variable name`
  )
  
  weight <- filter(
    a_spec[["var_def"]],
    `variable type` == "weight"
  ) %>% pull(
    `variable name`
  )
  
  person_id <- filter(
    a_spec[["var_def"]],
    `variable type` == "person uid"
  ) %>% pull(
    `variable name`
  )
  
  
  # use the person level data to get the overview
  # stats
  fd01 <- bind_rows(
    tibble(
      stat_name = "Total Cases",
      stat_val = sum(ld$person[weight])
    ), tibble(
      stat_name = "Maximum Possible Score",
      stat_val = select(
        ld$person_i,
        item,
        item_max_value
      ) %>% unique(
      ) %>% pull(
        item_max_value
      ) %>% sum()
    ),
    tibble(
      stat_name = "Maximum Attained Score",
      stat_val = pull(
        ld$person,
        tscore
      ) %>% max(
      )
    ),
    tibble(
      stat_name = "Mean Score",
      stat_val = weighted.mean(
        ld$person[[t_score]],
        ld$person[[weight]]
      )
    ), tibble(
      stat_name = "Median Score",
      stat_val = weighted.mean(
        ld$person[[t_score]],
        ld$person[[weight]]
      ) 
    ), tibble(
      stat_name = "Score Standard Deviation",
      stat_val = radiant.data::weighted.sd(
        ld$person[[t_score]],
        ld$person[[weight]]
      )
    )
  )
    
  return(
    fd01
  )
  
}