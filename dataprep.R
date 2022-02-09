# Dataprep



library(tidyverse)

countryLookup = read_csv("ISA – WIDSR – Country Lookup.csv", n_max = 17, na = c("N/A")) %>%
  select(Country:GGI)

ourNamesInd = read_csv("ColumnNameLookupInd.csv")

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
    mutate(across(everything(), ~replace(., !is.na(.), 1))) %>% # using replace() & replace_na() instead of ifelse() or if_else() due to speed.
    mutate(across(everything(), .fns = ~replace_na(.,0))) 
  
  
  sepColumnNames = new_columns_as_dataframe %>% colnames() %>%  str_to_title() %>% str_replace_all(pattern = " ", replacement = "") %>% paste(colName, ., sep = "_")
  colnames(new_columns_as_dataframe) = sepColumnNames
  
  dfr = bind_cols(dfr, new_columns_as_dataframe)
  dfr
}


read_csv("Individual Survey.csv", col_names = ourNamesInd$InternalName, skip=3, na = c(""," ", "N / A", "N/A")) %>%
  
  mutate( Country = str_to_title(Country)
          
          , NumPubs_PatentApps = as.numeric(NumPubs_PatentApps) # correct data type
          , NumPubs_Patents = as.numeric(NumPubs_Patents)
          
          , Country = case_when(  str_detect(Country, 'Alice|Botsawana')       ~ 'Botswana'
                                 , str_detect(Country, 'Trinidad|Tobago')       ~ "Trinidad &\nTobago" 
                                 , str_detect(Country, 'Kitts|Nevis')           ~ "St. Kitts &\nNevis"        
                                 , str_detect(Country, 'Maldives')              ~ "Maldives"
                                 , str_detect(Country, 'Fiji')                  ~ "Fiji Islands"
                                 , str_detect(Country, 'Mauritus')              ~ "Mauritius"
                                 , str_detect(Country, 'Mocambique|Moçambique|Tabitha') ~ 'Mozambique'
                                 , TRUE ~ Country)
          
          , Helped_Research       = fix_gender_col(Helped_Research)
          , Helped_Grant          = fix_gender_col(Helped_Grant)
          , Helped_Papers         = fix_gender_col(Helped_Papers)
          , Helped_Position       = fix_gender_col(Helped_Position)
          , Helped_Programme      = fix_gender_col(Helped_Programme)
          , Helped_Apprenticeship = fix_gender_col(Helped_Apprenticeship)
          , Helped_Employment     = fix_gender_col(Helped_Employment)
          , Helped_Study          = fix_gender_col(Helped_Study)
          , Helped_SupportEducation = fix_gender_col(Helped_SupportEducation)
          
          , CareerBreakReason              = case_when(CareerBreakReason == "Care for family member (other than children)" ~ "Care for family member \n(other than children)"
                                                       , CareerBreakReason == "parental leave and health" ~ "parental leave \nand health"
                                                       , TRUE ~ CareerBreakReason)
          
          , iPartnerWorkingForMoney        = case_when(str_detect(iPartnerWorkingForMoney, 'full') ~ 'Yes, full-time'
                                                       , str_detect(iPartnerWorkingForMoney, 'part') ~ 'Yes, part-time'
                                                       , TRUE ~ iPartnerWorkingForMoney)
          
          , GendersTreatedEquallyAtWork    = case_when(str_detect(GendersTreatedEquallyAtWork, 'sometimes') ~ 'Sometimes'
                                                       , str_detect(GendersTreatedEquallyAtWork, 'No') ~ 'Consistent \nInequities'
                                                       , str_detect(GendersTreatedEquallyAtWork, 'Yes') ~ 'Always \nEquitable')
          
          , EthnicityTreatedEquallyAtWork  = case_when(str_detect(EthnicityTreatedEquallyAtWork, 'sometimes') ~ 'Sometimes'
                                                       , str_detect(EthnicityTreatedEquallyAtWork, 'No') ~ 'Consistent \nInequities'
                                                       , str_detect(EthnicityTreatedEquallyAtWork, 'Yes') ~ 'Always \nEquitable')
          
          , AwareGenderPayGap              = case_when(str_detect(AwareGenderPayGap, 'Yes') ~ 'Yes, men make more'
                                                       , TRUE ~ AwareGenderPayGap)
          
          , iGendersTreatedEquallyAtSchool = case_when(str_detect(iGendersTreatedEquallyAtSchool, 'sometimes') ~ 'Sometimes'
                                                       , str_detect(iGendersTreatedEquallyAtSchool, 'No') ~ 'Consistent \nInequities'
                                                       , str_detect(iGendersTreatedEquallyAtSchool, 'Yes') ~ 'Always \nEquitable')
          
          , iWomenMinorityOverlooked_Ed    = case_when(str_detect(iWomenMinorityOverlooked_Ed, 'No') ~  'Equitable \nTreatment'
                                                       , str_detect(iWomenMinorityOverlooked_Ed, 'Yes') ~  'Inequitable \nTreatment')
          
          , AwareGenderPayGap_Ed           = case_when(str_detect(AwareGenderPayGap_Ed, 'Yes') ~ 'Yes, men make more' 
                                                       , TRUE ~ AwareGenderPayGap_Ed)
          
          , NumPubs_Articles               = if_else(NumPubs_Articles == 0.1, 0, NumPubs_Articles)
          , NumPubs_Books                  = if_else(NumPubs_Books == 0.1, 0, NumPubs_Books)
          , NumPubs_Chapters               = if_else(NumPubs_Chapters == 0.1, 0, NumPubs_Chapters)
          , NumPubs_PatentApps             = if_else(NumPubs_PatentApps == 0.1, 0, NumPubs_PatentApps)
          , NumPubs_Patents                = if_else(NumPubs_Patents == 0.1, 0, NumPubs_Patents)
          
          ## Try to replace the above NumPubs_* with one line code as below. 
          ## %>% mutate_at(vars(contains("NumPubs")), list(~case_when(.==0.1 ~ 0, TRUE ~ .x ) # Right now this .x does not work.
          
  ) %>% 
  ## All variables to de-coalesce and count
  sep_col() %>%
  sep_col(colName = "ReasonsForNotEnoughOppsConferences") %>%
  sep_col(colName = "NatureWorkDiscrim") %>%
  sep_col(colName = "BasisWorkDiscrim") %>%
  sep_col(colName = "TrainingSubsequentActivity") %>%
  sep_col(colName = "CareerProgressSupport") %>%
  sep_col(colName = "ProgrammeNotCompleted_WhyEnrol") %>%
  sep_col(colName = "ProgrammeNotCompleted_WhyNotComplete") %>%
  sep_col(colName = "InterruptionReasons") %>%
  sep_col(colName = "PositionsHeld") %>%
  sep_col(colName = "ProgrammeNotCompleted_WhyEnrol_Ed") %>%
  sep_col(colName = "ProgrammeNotCompleted_WhyNotComplete_Ed") %>%
  sep_col(colName = "DifficultFulfillResp_Reasons") %>%
  sep_col(colName = "ReasonsTurnDownTravel_Ed") %>%
  sep_col(colName = "NatureWorkDiscrim_Ed") %>%
  sep_col(colName = "BasisWorkDiscrim_Ed") %>%

  # # select(-ends_with("_Ed")) %>%
  # filter(Consent == "I consent") %>%
  # filter(iStudyOrEmployed == "Yes") %>%
  # select(-c(Timestamp, Consent)) %>%
  # left_join(countryLookup, by = "Country") %>%
  saveRDS("ISA_Raw_Ind.rds")

dd = readRDS("ISA_Raw_Ind.rds")
colnames(dd)


############################## Testing.  ##########################################

n <- ncol(dd) 
n
dd[, (n - 10):n]

dd %>% select(-c(starts_with("Helped_")
                 , starts_with("EmploymentSatisfaction_")
                 , starts_with("OrgHavePolicies_")
                 , starts_with("Funding_")
                 , starts_with("InstitutionPolicies_")
                 )) %>% 
  saveRDS("ISA_Ind.rds")


# Replace NAs in a data frame
df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"))
df %>% replace_na(list(x = 0, y = "unknown"))

# Replace NAs in a vector
df %>% dplyr::mutate(x = replace_na(x, 0))
# OR
df$x %>% replace_na(0)
df$y %>% replace_na("unknown")

# Replace NULLs in a list: NULLs are the list-col equivalent of NAs
df_list <- tibble(z = list(1:5, NULL, 10:20))
df_list %>% replace_na(list(z = list(5)))




colName = "NatureWorkDiscrim"
new_columns_as_dataframe = dd %>%
  pull(colName) %>%
  str_split(pattern = ";", simplify = TRUE) %>%
  as.data.frame() %>%
  mutate(ID = row_number()) %>%
  pivot_longer(starts_with("V")) %>%
  filter(value != "" | is.na(value)) %>%
  pivot_wider(id_cols = ID, names_from = value) %>% 
  select(-c("ID", "NA")) %>% 
  mutate(across(everything(), ~replace(., !is.na(.), 1))) %>% 
  mutate(across(everything(), .fns = ~replace_na(.,0))) 


sepColumnNames = new_columns_as_dataframe %>% colnames() %>%  str_to_title() %>% str_replace_all(pattern = " ", replacement = "") %>% paste(colName, ., sep = "_")
colnames(new_columns_as_dataframe) = sepColumnNames

dfr = bind_cols(dfr, new_columns_as_dataframe)
dfr

read_csv("Institutional Survey.csv") %>%
  rename(Country = `Please enter the name of your country.`) %>%
  mutate(
    Country = str_to_title(Country)
  ) %>%
  saveRDS("ISA_Raw_Inst.rds")

read_csv("National Survey.csv") %>%
  rename(Country = `Please enter the name of your country.`) %>%
  mutate(
    Country = str_to_title(Country)
  ) %>%
  saveRDS("ISA_Raw_NFP.rds")



# "Thapelo Jacobs". I think I've found this person, and that the response is from Botswana.

# dd %>% filter(!is.na(Email)) %>% select(Country, Institution, Email) %>% write_csv("EmailList.csv")
# dd = read_csv("Institutional Survey.csv")
# data.frame(InternalName = NA, iBinary = NA, ExternalName = names(dd)) %>% write_csv("ColumnNameLookupInst.csv")

pretty_strings <- function(string) {
  
  pretty_new_lines = function(x) gsub("([^ ]+ [^ ]+) ", "\\1\n", x)
  
  blankCount = str_count(string, pattern = " ")
  
  # If only one space, replace with \n
  if (blankCount == 1) {
    string = str_replace(string, " ", "\n")
    
  } else if (blankCount == 2) { # if 2 spaces, put beside longest word
    string = pretty_new_lines(string)
    
  } else if (blankCount == 3) { # If 3 spaces, put after 2nd one
    string = pretty_new_lines(string)
    
  } else if (blankCount > 3) { # If 4 or more, put every 2nd space
    string = pretty_new_lines(string)
    
  }
  return(string)
}



table_plus <- function(n) {
  print(colnames(dd)[n-1])
  print(table(dd[[n-1]]))
  print(colnames(dd)[n-1])
}

table_plus(3)


#FFFFFF	RGB(255, 255, 255)	0.77042
#C09060	RGB(192, 144, 96)	0.05146
#183060	RGB(24, 48, 96)	0.04979
#9090A8	RGB(144, 144, 168)	0.02931
#D8D8D8	RGB(216, 216, 216)	0.01965
#F0F0F0	RGB(240, 240, 240)	0.01500
#787878	RGB(120, 120, 120)	0.01139
#C0C0C0	RGB(192, 192, 192)	0.01063
#A8A8A8	RGB(168, 168, 168)	0.00979
#606060	RGB(96, 96, 96)	0.00792
#D8C0A8	RGB(216, 192, 168)	0.00736
#C0A878	RGB(192, 168, 120)	0.00403
#1860A8	RGB(24, 96, 168)	0.00313
#60C0D8	RGB(96, 192, 216)	0.00313
#484848	RGB(72, 72, 72)	0.00236
#4878A8	RGB(72, 120, 168)	0.00090
#304878	RGB(48, 72, 120)	0.00083
#486078	RGB(72, 96, 120)	0.00063
#1878C0	RGB(24, 120, 192)	0.00042
#303030	RGB(48, 48, 48)	0.00042