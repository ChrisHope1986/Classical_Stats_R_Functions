# data printer function to apply over a dataframe and
# print data based on its specification to
# an excel spreadsheet.

# spec_df = dataframe. the dataframe to be used to provide the specs to do the print
# data_ls = list of dataframes to be printed that can be indexed by spec_df
# wb = openxlx workbook. the workboook to be printed to
# sh = character value. character value of the worksheet to print to. 
# ld = list of 3 data frames of the assessment data in long format.
#       consists of the dataframes person, person_i and person_q.
# header_names = dataframe of the header names to replace the R names with the final output
#               names for the headers.
# header_fm = header_format dataframe with the columns prop_type and prop_val
data_printer <- function(
  spec_df,
  data_df,
  wb,
  sh,
  ld,
  demo_var,
  mapping_list,
  format_df,
  header_names,
  header_fm
){
  
  apply(
    spec_df,
    1,
    function(x){
      
      #browser()
      
      # determine if the data is a header or not
      if(x["type"] == "header"){
        
        fd01 <- attributes(ld$person[[demo_var]])[["labels"]]
        
        if(!is.null(fd01)){
          fd02 <- names(fd01[match(x["demo_level"], fd01)])  
        }else(
          fd02 <- NULL
        )
        
        writeData(
          wb,
          sh,
          paste0(
            fd02,
            "[",
            x["demo_level"],
            "]",
            "-",
            filter(
              header_names, 
              original_headers == x["df_name"]
            ) %>% pull(
              new_headers
            )
          ),
          startCol = x["start_col"],
          startRow = x["start"],
        )
        
        # format the header
        fm_obj_applier(
          mutate(
            header_fm,
            prop_val = ifelse(
              prop_type %in% c(
                "fontSize",
                "textRotation",
                "indent" 
              ),
              prop_val,
              paste0("\"", prop_val, "\"")
            )
          ),
          wb,
          sh,
          x["start"],
          x["start_col"]
        )
        
      }else if(x["type"] == "data"){
        
        fd01 <- data_df[[x["demo_level"]]][[x["df_name"]]]
        
        fd02 <- rename_output(
          mapping_list,
          fd01,
          sh,
          x["df_name"]
        )
        
        writeData(
          wb,
          sh,
          fd02,
          startCol = x["start_col"],
          startRow = x["start"],
        )
        
        fd03 <- format_df[[x["demo_level"]]][[x["df_name"]]]
        
        # split up the formating dataframe
        split_fm_df(
          fd03,
          wb,
          start_row = as.numeric(x["start"]),
          start_col =  as.numeric(x["start_col"]),
          sh
        )
        
        
      }
      
      
    }
  )
  
  
}