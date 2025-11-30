# distractor analysis
# ld = the long format data
# qi_map = the question to item mapping dataframe
#           from the analysis specification file
# ltw_spec = specification for turning wide format data from
#             wide format to a list of long format data

distractor <- function(
  ld,
  a_spec,
  drop_tscore
){
  
  # find if the correlations between distractors and total scores have the item total
  # scores removed

  
  # need to link the questions to their items and then
  # join the dropped tscores to each question
  
  # remove attributes from the person_by_question dataset
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
  
  
  attributes(
    ld$person_q[[person_id]]
  ) <- NULL
  
  fd01 <- inner_join(
    ld$person_q,
    a_spec$qi_mapping,
    by = c("question")
  )
  
  # inner join the dropped scores onto the
  # question response data
  fd02 <- inner_join(
    fd01,
    drop_tscore,
    by = c(
      "items" = "item", 
      person_id
    )
  )
  
  # join on just the weight variable
  fd02b <- inner_join(
    fd02,
    select(
     ld$person,
     !!sym(person_id),
     !!sym(weight)
    ),
    by = person_id
  )
  
  
  rm(fd01)
  
  # split out the data by question and question response
  # then lapply over the data to recode cases depending on whether they provided the 
  # question response or not
  
  # need to get the complete set of question responses becuase the final dataset is a
  # dataframe with question responses running along the top of the dataframe as columns.
  fd03 <- select(
    fd02b,
    question,
    question_response_value
  ) %>% unique()
  
  # NA values can't be used to split out the data in base R "split"
  # so the tidyverse solution is used here
  fd04 <- group_by(
    fd03,
    question,
    question_response_value
  ) %>% group_split(
  )
  
  # can now lapply over the data to get the binary coding of the
  # data into 1 for whether each response was provided or not in order
  # to find the point biserial correlation for each question response.
  # need to test if the question values is na or not and as they binary coding
  # needs to be done differntly depending on whehter the question response is 
  # NA or not NA.
  fd05 <- lapply(
    fd04,
    function(x){
      
      # filter the data by question and then create a 
      # flag to indicate if the response is of the type in the list
      ffd01 <- filter(
        fd02b,
        question == x[["question"]]
      )
      
      if(!is.na(x[["question_response_value"]])){
        
        ffd02 <- mutate(
          ffd01,
          qrv_flag = if_else(
            question_response_value == x[["question_response_value"]],
            1,
            0,
            missing = 0
          )
        )
        
      }else{
          
        ffd02 <- mutate(
          ffd01,
          qrv_flag = if_else(
            is.na(question_response_value),
            1,
            0
          )
        )
    
      } # end of if_else statement
      
      # run correlation between flag and the dropped total score
      ffd03 <- weights::wtd.cors(
        ffd02$qrv_flag,
        ffd02$drop_tscore,
        weight = ffd02[[weight]]
      )
      
      return(
        tibble(
          question = x[["question"]],
          question_response_value = x[["question_response_value"]],
          cor = ffd03,
          n = sum(ffd02$qrv_flag * ffd02[[weight]]),
          mean_score = filter(
            ffd02,
            qrv_flag == 1
          ) %>% summarise(
            mean_ds = weighted.mean(
              drop_tscore,
              w = !!sym(weight)
            )
          ) %>% pull(
            mean_ds
          )
        )
      )
      
    }
  ) %>% bind_rows()   # the create the correlation for that value
  
  # reshape the data to get it into the right format
  # NA response cannot be handled easily so these are renamned
  # blank to make it easier and consistent with the legacy fast items
  # output.
  fd06 <- gather(
    fd05,
    stat,
    stat_value,
    c(
      cor, n, mean_score
    )
  ) %>% arrange(
    question,
    question_response_value,
    stat
  ) %>% mutate(
    question_response_value = ifelse(
      is.na(question_response_value),
      "blank",
      question_response_value
    )
  )
  
  # as the final outputted dataset is a dataframe
  # questions that do not have responses that are
  # present for other items will need to have 
  # values of NAs for those missing responses.
  
  # need to get the set of unique question_response_values
  fd07 <- select(fd06, question_response_value) %>%
    unique() %>%
    pull(question_response_value)
  
  names(fd07) <- fd07
  
  # and get the set of unique question ids, and name them
  # so they can be properly referenced in the list
  fd08 <- select(fd06, question) %>% 
      unique() %>%
      pull(question)
  
  names(fd08) <- fd08
  
  # lapply over the questions in the data
  fd09 <- lapply(
    fd08,
    function(x){
      
      # lapply over each unique response in the data
      lapply(
        fd07,
        function(y){
          
          # filter the data by question (x) and response (y)
          # return the filtered dataset if there is a match
          # if not then create an NA dataset of statistics
          ffd01 <- filter(
            fd06,
            question == x,
            question_response_value == y
          )
          
          if(nrow(ffd01) > 0){
            
            # the stat_value column is renamed
            # to be the question response value, so
            # the lists can eventually be inner joined
            # together for each question.
            ffd02 <- select(
              ffd01,
              stat,
              stat_value
            ) %>% rename(
              !!sym(y) := stat_value
            )
          }else{
            ffd02 <- tibble(
               stat = c(
                 "cor",
                 "mean_score",
                 "n"
               ),
               !!sym(y) := c(
                 rep(NA, 3)
               )
            )
          }
          
        } # end y lapply function
      ) # end y lapply loop 
      
    } # end x lapply function
  ) # end x lapply loop
  
  # inner join the lists together within each question
  fd10 <- lapply(
    fd09,
    function(x){
      reduce(
        x,
        inner_join,
        by = "stat"
      )
    }
  )
  
  # create an overall dataframe from the lists
  fd11 <- map_dfr(
    fd10,
    ~.,
    .id = "item"
  ) %>% inner_join(
    unique(
      select(
        ld$person_q,
        question,
        order_index
      )
    ),
    by = c("item" = "question")
  ) %>% mutate(
    stat = factor(stat, levels =c("n", "cor", "mean_score"))
  ) %>% arrange(
    order_index,
    stat
  ) %>% select(
    -order_index
  ) %>% mutate(
    stat = as.character(stat)
  )
  
  return(fd11)
  
}