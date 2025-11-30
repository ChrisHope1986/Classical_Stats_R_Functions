# get the row numbers of the different elements
# that are to be outputted

#   parameters
#     rsc_ls = list of classical analyis split by demographics
#               outputted by the organise split function
#   sel_fm_banding_fill = selected formatting options for the dataframe bands when
#                         printed to the excel file.
#   sel_fm_col_name = selected formatting options for the column names when printed
#                     to the excel file

rows_split_classical <- function(
  rsc_ls,
  sel_fm_banding_fill,
  sel_fm_col_name
){
  
  # get teh spec for the data list
  
  # lapply over the booklets
  fd02 <- lapply(
    rsc_ls,
    function(x){
  
    # get a dataframe for each element
    # of the number cummulative sum 
    # of rows for each dataframe
    
    # need to loop over the data to get at the
    # sheet level
    
    # lapply over the demographic variables
    lapply(
        x,
        function(y){
          
          fd01 <- names(y)
          names(fd01) <- fd01
          
           # lapply over the sheet names
          lapply(
            fd01,
            function(z){
              
              
              # browser()
              
              # get the rows of data
              
              # add the rows data as a list to the end of the
              # demo level 
              
              
              ffd01 <- map_dfr(
                y[[z]],
                ~map_dfr(
                  .,
                  ~tibble(
                    type = "data",
                    nrow = nrow(.),
                    ncol = ncol(.)
                  ),
                  .id = "df_name"
                ),
                .id = "demo_level"
              ) %>% mutate(
                row_index = 1:nrow(.)
              )
              
              ffd02 <- mutate(
                ffd01,
                type = "header",
                nrow = 1
              )
              
              ffd03 <- bind_rows(
                ffd01,
                ffd02
              )
              
              ffd04 <- arrange(
                ffd03,
                row_index,
                factor(
                  type,
                  level = c("header", "data")
                )
              )
              
              ffd05 <- group_by(
                ffd04,
                demo_level
              ) %>% mutate(
                nrow_2 = ifelse(
                  type == "data",
                  nrow + 2,
                  nrow
                ),
                cummsum = cumsum(
                  nrow_2
                ),
                start = (lag(cummsum)) + 1,
                start = ifelse(is.na(start), 1, start)
              ) %>% ungroup()
               
              ## get the lag of the 
              # max cols of the data
              ffd06 <- group_by(
                ffd05,
                demo_level
              ) %>% summarise(
                row_index = max(row_index),
                max_col_count = max(ncol) + 1
              ) %>% ungroup(
              ) %>% arrange(
                row_index
              ) %>% mutate(
                cummsum_col= cumsum(max_col_count),
                lag_col_count = lag(
                  cummsum_col
                ) + 1
              ) %>% select(
                demo_level,
                lag_col_count
              ) %>% mutate(
                start_col = ifelse(
                  is.na(lag_col_count),
                  1,
                  lag_col_count
                )
              )
              
              ffd07 <- inner_join(
                ffd05,
                select(
                  ffd06,
                  demo_level,
                  start_col
                ),
                by = "demo_level"
              ) %>% arrange(
                row_index,
                factor(
                  type,
                  level = c("header", "data")
                )
              )
              
              # z2
              # within each sheet loop over each demographic variable level
              
              # z3
              # within each demographic variable level loop over each dataframe
              
              # run function to select the correct text properties list
              
              # and then build out the formatting dataframe
              
              # pass in the data
              
              # 
              ffd07b <- run_fsc(
                y[z],
                sel_fm_banding_fill,
                sel_fm_col_name
              )
              
              
              fd08 <- list(
                data = y[[z]],
                nrow_data = ffd07,
                format_spec = ffd07b
              )
              
              return(fd08)
              
            }  
          ) # y # end lapply over sheet names                
        
        }    
      )# x end lapply over booklets
        
    }) # end lapply
  
  # have the names of the fd01 list elements and the number of rows in the
  # dataframes
  
  
  
  
  
  
#   # create a second data frame for just the titles, each of the rows here will equal only 1
#   
#   # lapply over booklet level
#   fd03 <- lapply(
#     fd02,
#     function(x){
#       
#       # lapply over demographic variable
#       # level
#       lapply(
#         x,
#         function(y){
#         
#           lapply(
#             y,
#             function(z){
#               
#               mutate(
#                 z,
#                 type = "header",
#                 nrow = 1
#               )        
#               
#             }
#           )# end z lapply
#           
#       }
#     )# end y lapply
#   }
# ) # end x lapply
#     
#       
#   fd04 <- names(fd02)
#   names(fd04) <- fd04
#   
#   fd05 <- lapply(
#     fd04,
#     function(x){
#       
#       # browser()
#       # rbind the data nrow data and the header data together
#       ffd01 <- bind_rows(
#         fd02[[x]],
#         fd03[[x]]
#       )
#       
#       # sort the rows by list_el_name and then by factor of the type (levels = "header", "data)
#       ffd02 <- arrange(
#         ffd01,
#         list_el_name,
#         factor(type, levels = c("header", "data"))
#       )
#       
#       # create the cummsum
#       
#       # nrow 2 is the data nrow + 1 to seperate the titles
#       ffd03 <- mutate(
#         ffd02,
#         nrow_2 = ifelse(
#           type == "data",
#           nrow + 1,
#           nrow
#         ),
#         cummsum = cumsum(
#           nrow_2
#         ),
#         start = (lag(cummsum)) + 1,
#         start = ifelse(is.na(start), 1, start)
#       )
#       
#       
#     }
#   )

  # turn the co-ordinates list into a list of objects to
  # print out onto a worksheet
  
  return(fd02)
  
}