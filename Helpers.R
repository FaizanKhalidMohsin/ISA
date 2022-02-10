
fix_gender_col <- function(x) {
  case_when(str_detect(x, 'No one') ~ 'No One'
            , x == "Female;Male" ~ "Both"
            , TRUE ~ x)
}


sep_col <- function(dfr, colName = "PersonalEngagement") {
  
  new_columns_as_dataframe = dfr %>%
    pull(colName) %>%
    str_split(pattern = ";", simplify = TRUE) %>%
    as.data.frame() %>%
    mutate(ID = row_number()) %>%
    pivot_longer(starts_with("V")) %>%
    filter(value != "" | is.na(value)) %>%
    pivot_wider(id_cols = ID, names_from = value) %>% 
    select(-c("ID", "NA")) %>% 
    # Using replace() & replace_na() instead of ifelse() or if_else() due to speed.
    mutate(across(everything(), ~replace(., !is.na(.), 1))) %>%
    mutate(across(everything(), .fns = ~replace_na(.,0))) 
  
  # Using Tyler the Great's naming convention.
  sepColumnNames = new_columns_as_dataframe %>% 
    colnames() %>%  str_to_title() %>% str_replace_all(pattern = " ", replacement = "_") %>% 
    paste(str_to_upper(colName), ., sep = "_")
  
  colnames(new_columns_as_dataframe) = sepColumnNames
  
  dfr = bind_cols(dfr, new_columns_as_dataframe)
  dfr
}



# 
# sep_col <- function(dfr, colName = "PersonalEngagement") {
#   
#   new_columns_as_dataframe = dfr %>%
#     pull(colName) %>%
#     str_split(pattern = ";", simplify = TRUE) %>%
#     as.data.frame() %>%
#     mutate(ID = row_number()) %>%
#     pivot_longer(starts_with("V")) %>%
#     filter(value != "" | is.na(value)) %>%
#     pivot_wider(id_cols = ID, names_from = value) %>% 
#     select(-c("ID", "NA"))
#   
#   sepColumnNames = new_columns_as_dataframe %>% colnames() %>%  str_to_title() %>% str_replace_all(pattern = " ", replacement = "")
#   colnames(new_columns_as_dataframe) = sepColumnNames
#   
#   dfr = bind_cols(dfr, new_columns_as_dataframe)
#   dfr
# }



pretty_strings <- function(myString) {
  # myString = dd$Field # Testing
  pretty_new_lines = function(x) gsub("([^ ]+ [^ ]+) ", "\\1\n", x)
  
  blankCount = str_count(myString, pattern = " ")
  
  for(i in 1:length(blankCount)){
    thisBlankCount = blankCount[i]
    
    # If only one space, replace with \n
    if (thisBlankCount == 1) {
      myString[i] = str_replace(myString[i], " ", "\n")
      
    } else if (thisBlankCount == 2) { # if 2 spaces, put beside longest word
      myString[i] = pretty_new_lines(myString[i])
      
    } else if (thisBlankCount == 3) { # If 3 spaces, put after 2nd one
      myString[i] = pretty_new_lines(myString[i])
      
    } else if (thisBlankCount > 3) { # If 4 or more, put every 2nd space
      myString[i] = pretty_new_lines(myString[i])
    }
  }
  
  sub("–", "-", myString)
}
