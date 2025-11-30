# dif analyis
# ld = list of demographic variables
# a_spec = specification for turning data from wide to long format
# demo_var = demographic variable to run DIF analysis on
dif <- function(
  ld,
  a_spec,
  demo_var
){
  
  require(tidyverse)
  require(ordinal)
  
  # use the person data frame to get the demographic identifier
  
  # need to join on the person level dataframe onto the item level
  # dataframe
  
  fd01 <- inner_join(
    ld$person_i,
    ld$person,
    by = filter(
      a_spec$var_def, `variable type` == "person uid" 
    ) %>% pull(
      `variable name`
    )
  )
  
  # get the person weight variable
  fd01b <- filter(
    a_spec$var_def, `variable type` == "weight" 
  ) %>% pull(
    `variable name`
  )

  # set the person variable as a factor to allow the model to run
  fd02 <- mutate(
    fd01,
    !!sym(demo_var) := as.factor(
        as_factor(!!sym(demo_var))
    )
  )
  
  # could create a lookup table to see which level of the deomgraphic
  # variable is highrer than the other
    
  # split out the dataframe by item and run logistic regression on
  # each one
  fd03 <- split(
    fd02,
    fd02$item
  )
  
  fd04 <- lapply(
    fd03,
    function(x){
      
      # run logistic regression model on each data frame
      ffd01 <- ordinal::clm(
        formula = as.formula(
          paste(
              "as.factor(item_score_value) ~ tscore + ",
              demo_var
            )
        ),
        data = x,
        weights = x[[fd01b]]
      )
      
      # find the coefficients and their standard errors
      ffd02 <- summary(
        ffd01
      )
      
      # get the mean item_score value by the demographic variable
      ffd03 <- group_by(
        x,
        !!sym(demo_var)
      ) %>% summarise(
        mean_score = weighted.mean(
          x = item_score_value,
          w = !!sym(fd01b)
        ),
        n = sum(!!sym(fd01b))
      ) %>% ungroup()
      
      # use the valence of the demographic coefficient to find
      # which group is favoured by the DIF
      # use the levels of the demographic variable factor to get the name of 
      # the demographic variable
      
      
      # get the vector of the demographic variable values
      ffd04 <- x[[demo_var]]
      
      # get the name of the demo_graphic variable coeffients as
      # they exist in the logistic regression output
      
      ffd05a <-levels(ffd04)[1]
      
      ffd05b <- levels(ffd04)[length(levels(ffd04))]
      
      # get the coefficient for the demographic variable      
      ffd06 <- ffd02$coefficients[
        paste0(demo_var, ffd05b),
        "Estimate"
      ]
      
      # find which demographic group is favoured by
      # DIF
      ffd07 <- case_when(
        ffd06 == 0 ~ "neither group",
        ffd06 < 0 ~ ffd05a,
        ffd06 > 0 ~ ffd05b
      )
      
      # create dataframe of the logistic regression outputs
      
      # use levels of demographic variable to create the
      # names for the dataframe and to refer to specific coefficient values
      # 
      
      ffd08 <- tibble(
        # mean score for first level of demographic factor
        !!sym(ffd05a) :=  filter(
          ffd03, !!sym(demo_var) == ffd05a
        ) %>% pull(
          mean_score
        ),
        # mean score for second level of demographic factor
        !!sym(ffd05b) :=  filter(
          ffd03, !!sym(demo_var) == ffd05b
        ) %>% pull(
          mean_score
        ),
        # coefficient for demographic factor
        coefficient = ffd06,
        std_error =  ffd02$coefficients[
          paste0(demo_var, ffd05b),
          "Std. Error"
        ],
        # significance of coefficient
        signif = ffd02$coefficients[
          paste0(demo_var, ffd05b),
          "Pr(>|z|)"
        ],
        # favours 
        favours = ifelse(signif < 0.05, ffd07, NA),
        #  severity, the cases need to descend from most sevre to least sevre
        #  in order so that the cases are tripped in the right order,
        # for example, so that the medium case isn't returned when
        # the case has large severity.
        severity = case_when(
          ((1 - pnorm((abs(coefficient) - 0.426)/std_error)) < 0.025) & (abs(coefficient) >= 0.638) ~ "Large",
          signif < 0.05 & abs(coefficient) >= 0.426 ~ "Medium",
          signif < 0.05 & abs(coefficient) < 0.426 ~ "Negligible",
          TRUE ~ NA_character_
        ),
        !!sym(
          paste0(
            "N(",
            ffd05a,
            ")"
          )
        ) :=  filter(
          ffd03, !!sym(demo_var) == ffd05a
        ) %>% pull(
          n
        ),
        # mean score for second level of demographic factor
        !!sym(
          paste0(
            "N(",
            ffd05b,
            ")"
          )
        ) :=  filter(
          ffd03, !!sym(demo_var) == ffd05b
        ) %>% pull(
          n
        )
        
      )
      
    }
  )
  
  # classical DIF on item variables
  fd05 <- bind_rows(
    fd04,
    .id = "item"
  )
  
  # need to also get out the mean score by the independent variable
  # levels as well
  fd06 <- group_by(
    ld$person,
    !!sym(demo_var) := as.factor(as_factor(!!sym(demo_var)))
  ) %>% summarise(
    mean_tscore = weighted.mean(
      x = tscore,
      w = !!sym(fd01b)
      )
  )
  
  return(
    list(
      mean_tscore = fd06,
      DIF = fd05  
    )
  )

}