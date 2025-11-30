# ld = list of long data
# spec = data specification file, for the long to wide formating
item_stats <- function(
  ld,
  a_spec
){
  
  person_id <- filter(
    a_spec[["var_def"]],
    `variable type` == "person uid"
  ) %>% pull(
    `variable name`
  )
  
  weight <- filter(
    a_spec[["var_def"]],
    `variable type` == "weight"
  ) %>% pull(
    `variable name`
  )
  
  # need to compute total score with item removed from total
  # for each person
  
  fd01 <- select(
    ld$person_i,
    !!sym(person_id),
    item
  )
  
  
  # inner join the item dataset by item

  # need to rename the columns
  # so that one columsn is the dropped column
  
  # joining on just the pupil id will mean that each item is joined onto
  # each other item
  
  # then can calculate the dropped t score by
  
  fd01b <- select(
    ld$person_i,
    !!sym(person_id),
    item,
    item_score_value
  ) %>% rename(
    drop_item = item
  )
  
  fd01c <- inner_join(
    fd01,
    fd01b,
    by = person_id
  )
  
  fd01d <- filter(
    fd01c,
    !item == drop_item
  )
  
  fd03 <- group_by(
    fd01d,
    !!sym(person_id),
    item
  ) %>% summarise(
    drop_tscore = sum(item_score_value),
    .groups = "keep"
  ) %>% arrange(
    !!sym(person_id),
    item
  )
  
  # add on the dropped t_score onto the person by item 
  # level dataset.
  
  # remove attributes from the
  
  attributes(
    ld$person_i[[person_id]]
  ) <- NULL
  
  fd04 <- inner_join(
    ld$person_i,
    fd03,
    by = c(person_id, "item")
  ) %>% arrange(
    !!sym(person_id),
    order_index
  )
  
  # join on the weightings onto the item stats
  fd04b <- inner_join(
    fd04,
    ld$person,
    by = person_id
  )
  
  
  # remove precursor datasets to save memory
  rm(
    fd01, 
    fd01b,
    fd01c,
    fd01d,
    fd03
  )
  
  # look at the correlation between drop_tscore and
  # item responses
  
  # the function shouldn't need to handle
  # missing data as NA item_score_values should be
  # scored as 0.
  fd05 <- group_by(
    fd04b,
    item
  ) %>% summarise(
    discrimination = weights::wtd.cors(
      item_score_value,
      drop_tscore,
      weight = !!sym(weight)
    ) %>% as.numeric()
  )
  
  # calculate other item stats here.
  
  # first join on person_by_item level data and the
  # person level data so person total score is joined on
  
  attributes(
    ld$person[[person_id]]
  ) <- NULL
  
  fd06 <- inner_join(
    ld$person_i,
    ld$person,
    by = person_id
  ) %>% mutate(
    item_facility_value = item_score_value/item_max_value
  ) %>% group_by(
    item
  ) %>% summarise(
    mean_score = weighted.mean(
      item_score_value,
      w = !!sym(weight)
    ),
    max_score = unique(
      item_max_value
    ),
    facility = weighted.mean(
      item_facility_value,
      w = !!sym(weight),
      na.rm = T
    ),
    omitted = weighted.mean(
      item_omitted_value,
      w = !!sym(weight)
    ),
    not_reached = weighted.mean(
      item_nr_value,
      w = !!sym(weight)
    )
  )
  
  # inner join on the discrimination values
  # to the rest of the item stats
  fd07 <- inner_join(
    fd06,
    fd05,
    by = "item"
  )
  
  # create percentage of the different item response
  # score 1, score 2 etc.
  fd08 <- group_by(
    inner_join(
      ld$person_i,
      ld$person,
      by = person_id
    ),
    item,
    item_score_value
  ) %>% summarise(
    n = sum(!!sym(weight)),
    .groups = "drop_last"
  ) %>% mutate(
    perc = n/sum(n)
  ) %>% ungroup(
  ) %>% arrange(
    item,
    item_score_value
  ) %>% mutate(
    item_score_value = factor(
      item_score_value,
      levels = unique(
        item_score_value
      ) %>% sort()
    )
  )
  
  # spread the data to get the different item
  # scores as columns
  # add rename the columns so they read in a meaningful
  # way in the final data file.
  fd09 <- spread(
    select(
      fd08,
      -n
    ) %>% mutate(
      item_score_value = paste(
        "Percent",
        item_score_value,
        "Mark/s"
      )
    ),
    item_score_value,
    perc
  )
  
  # join on the item response percentages onto
  # the rest of the item stats
  fd10 <- inner_join(
    fd07,
    fd09,
    by = "item"
  ) %>% inner_join(
      unique(
        select(
          ld$person_i,
          item,
          order_index
        )
      ),
      by = "item"
  ) %>% arrange(
    order_index
  ) %>% select(
    -order_index
  )
  # return the d04 dataset for later distractor analysis
  return(
    list(
      drop_tscore = fd04,
      item_stats = fd10
    )
  )
  
}