# add booklet info onto long format data

# arguments
#   ld = longformat data
#   direc = directory to wide to long format mapping
booklet_info <- function(
  ld,
  direc
){
  
  # read in the question to booklet mapping sheet
  # fd01 <- read_xlsx(
  #   direc,
  #   "question_booklet_mapping"
  # )
  
  # read in the item to booklet mapping sheet
  fd02 <- read_xlsx(
    direc,
    "item_booklet_mapping"
  )
  
  # get the config file
  fd02b <- read_xlsx(
    direc,
    "config"
  )
  
  
  # return the long format data with the booklet info joined on.
  fd03 <- ld
  
  # fd03$person_q <- inner_join(
  #   fd03$person_q,
  #   fd01,
  #   by = "question"
  # ) %>% arrange(
  #   !!sym(fd02b$unique_person_identifier),
  #   question
  # )
  
  
  fd03$person_i <- inner_join(
    fd03$person_i,
    fd02,
    by = "item"
  ) %>% arrange(
    !!sym(fd02b$unique_person_identifier),
    item
  )
  
  return(fd03)
  
}