# main funciton to transform the format of the wide format data file
# to a list of long format dataframes.

# direc = directory to the wide to long excel specification file

file_formatter_main <- function(
  direc
){
  require(readxl)
  require(tidyverse)
  require(haven)
  
  # parameters
  # directory to the wide to long format excel specification file
  d01 <- direc

  # set an empty list for the different sheets of information from the
  # excel specification file to be stored.
  d02 <- list()
  
  # set the config sheet
  d02$config <- read_xlsx(
    d01,
    "config"
  )
  
  # read in the person level
  # variables as a dataframe first
  # where the person level variables are
  # column names running along the top of 
  # a dataframe.
  d02$person <- read_xlsx(
    d01,
    "person level"
  )
  
  # set the person entry then to be a tibble
  # with one column called var, which is the
  # column names of the originally read in
  # dataframe
  d02$person <- tibble(
    var = names(
      d02$person
    )
  )
  
  # person q is the person by question
  # level data
  d02$person_q <- read_xlsx(
    d01,
    "person by question level"
  )
  
  d02$person_q = dup_col(
    d02$person_q,
    "question",
    "question_response"
  )
  
  
  # person_i is the person by item level specification
  d02$person_i <- read_xlsx(
    d01,
    "person by item level"
  )
  
  d02$person_i = dup_col(
    d02$person_i,
    "item",
    "item_score"
  )
  
  
  # read in the spss wide format data based on the input directory
  d03 <- read_sav(
    str_replace_all(d02$config$input_data, "\\\\", "/")
  )
  
  # seelct just the person identifier and 
  # the person level variables from d03 for the person level data frame.
  d04 <- select(
    d03,
    !!!syms(
      c(
        d02$config$unique_person_identifier,
        pull(d02$person, var) 
      )
    )  
  )
  
  # create the person by question level
  # dataset.
  d05 <- gatherer_main(
    d03,
    d02,
    "person_q"
  )
  
  # create the person by item level dataset
  d06 <- gatherer_main(
    d03,
    d02,
    "person_i"
  )
  
  # create list
  d07 <- list(
    person = d04,
    person_q = d05,
    person_i = d06  
  )
  
  d08 <- booklet_info(
    d07,
    direc
  )
  
  # select items from the d03 dataset to get the person level data
  return(
    d08
  )  
}








