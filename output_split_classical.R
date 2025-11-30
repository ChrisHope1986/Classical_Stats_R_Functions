# function to output split classical analysis to excel

#    wb = directory to a workbook
#    d = data object, created by the function elems_split_classical
#         contains the data to be printed, the coordinate dataframe
#         for printing the data and the formatting dataframe.
#     op = character values to output the final workbook to
#     ld = list of 3 dataframes of long format assessment data.
#           person = person demographic data. person_i = person by item data
#           person_q = person by question data.
# header_names = dataframe of the header names to replace the R names with the final output
#               names for the headers.
# header_fm = header_format dataframe with the columns prop_type and prop_val

output_split_classical <- function(
  wb,
  d,
  op,
  ld,
  mapping_list,
  header_names,
  header_fm
){
  
  # loop over folders
  fd01 <- names(d)
  for(i in fd01){
    
    # create a new directory for each booklet
    dir.create(
      paste0(
        op,
        "/",
        i
      )
    )
    
    # loop over demo variable to create different workbooks for each one
    fd02 <- names(d[[i]])
    for(j in fd02){
    
      #browser()
      
      f_env <- new.env()
      f_env$wb <- loadWorkbook(
        wb
      )
        
      # create a new evironment to assign the inputted wb to
      # then the different output types of header and data
      
      # loop over the sheets - different analysis types in the workbook
      fd03 <- names(d[[i]][[j]])
      for(k in fd03){
        
        # output the score plot data frames to the
        # 
        if(k == "Score table"){
          
          # combine the data together into 
          # a master dataframe
          
          temp_score_data_01 <- map_dfr(
            d[[i]][[j]][[k]]$data,
            ~.[["score_table"]],
            .id = "group"
          )
          
          
          # j is the demo variable
          
          # get the attributes from the ld person data
          
          # used an ifelse statement to rename the demographic variable
          # values
          
          # catch na conditions by replacing with the values
          
          
          temp_score_data_02 <- attributes(ld$person[[j]])[["labels"]]
          
          temp_score_data_03  <- if(
            is.null(temp_score_data_02)
          ){
            temp_score_data_01
          }else{
            mutate(
              temp_score_data_01,
              factor_match = match(
                group,
                temp_score_data_02
              ),
              group = ifelse(
                is.na(factor_match),
                group,
                names(temp_score_data_02[factor_match])
              )
            ) %>% select(
              -factor_match
            )
          }
           
          temp_score_data_04 <- rename(
            temp_score_data_03,
            score = score_range,
            percentage = percent
          ) %>% select(
            group,
            score,
            percentage
          )
          
          writeData(
            f_env$wb,
            "Score plot",
            temp_score_data_04,
            1,
            1
          )
          
        }
        
        
        # create a DIF  worksheet
        # create a new worksheet if the
        # the analysis type is DIF.
        if(
          str_detect(
            k,
            "^DIF_.*"
          )
        ){
         # browser()
          
          addWorksheet(f_env$wb, k)
        }
        
        # apply over the rows of the data pecification worksheet.
        # write data to worksheet k
        
        data_printer(
          d[[i]][[j]][[k]][["nrow_data"]],
          d[[i]][[j]][[k]][["data"]],
          f_env$wb,
          k,
          ld,
          j,
          mapping_list,
          d[[i]][[j]][[k]][["format_spec"]],
          header_names,
          header_fm
        )
        
        # # loop over the different variable levels outputted to different rows in the
        # # worksheet
        # fd04 <- names(d[[i]][[j]][[k]])
        # for(l in fd04){
        #   
        #   
        #   # print the variable levels
        #   
        #   
        #   
        #   # print the variable level to the workbook
        #   writeDatas(
        #     wb,
        #     
        #   )
        #   
        #   # loop over the list of dataframes at a specific variable level
        #   fd05 <- names(d[[i]][[j]][[k]][[l]])
        #   # m is a token for a dataframe
        #   for(m in fd05){
        #       
        #       # check if the analysis type is DIF, if it is then
        #       # create a new worksheet.
        #       if(
        #         
        #       ){
        #         
        #         # create a new workbook.
        #         
        #         # create a function to handle the 
        #         # apply over the nrow data frame to output
        #         # headers or dataframes
        #         
        #         
        #       }
        # 
        #   } # loop over the next m
        #     
        # } # end the l loop over datasets in the sheet
        
      } # end k loop over worksheets
      
      # save out the workbooks
      saveWorkbook(f_env$wb,  paste0(
        op,
        "/",
        i,
        "/",
        i,
        " - ",
        j,
        ".xlsm"
      ), overwrite = TRUE)
      
    } # end j loop over different workbooks
  
  } # end i loop over booklets
  
  #return(wb)
  
}