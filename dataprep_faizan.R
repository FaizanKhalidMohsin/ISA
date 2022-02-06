library(tidyverse)

ourNamesInd = read_csv("ColumnNameLookupInd.csv")

dd_raw = read_csv("Individual Survey.csv", col_names = ourNamesInd$InternalName, skip=3, na = c(""," ", "N / A"))

dd = dd_raw %>% 
  mutate(NumPubs_PatentApps = as.numeric(NumPubs_PatentApps) # correct data type
         , NumPubs_Patents = as.numeric(NumPubs_Patents)
         ) %>% 
  mutate( Country = str_to_title(Country)
        , Country                        = case_when(  str_detect(Country, 'Alice|Botsawana')       ~ 'Botswana'
                                                     , str_detect(Country, 'Trinidad|Tobago')       ~ "Trinidad &\nTobago" 
                                                     , str_detect(Country, 'Kitts|Nevis')           ~ "St. Kitts &\nNevis"        
                                                     , str_detect(Country, 'Maldives')              ~ "Maldives"
                                                     , str_detect(Country, 'Fiji')                  ~ "Fiji Islands"
                                                     , str_detect(Country, 'Mauritus')              ~ "Mauritius"
                                                     , str_detect(Country, 'Mocambique|Moçambique|Tabitha') ~ 'Mozambique'
                                                     , TRUE ~ Country)
        
        , Helped_Grant                   = case_when(Helped_Grant == "Female;Male" ~ "Both"
                                                     , TRUE ~ Helped_Grant)    
        
        , Helped_Research                = case_when(str_detect(Helped_Research, 'No one') ~ 'No One'
                                                     , Helped_Research == "Female;Male" ~ "Both"
                                                     , TRUE ~ Helped_Research)
        
        , Helped_Papers                  = case_when(str_detect(Helped_Papers, 'No one') ~ 'No One'
                                                     , Helped_Papers == "Female;Male" ~ "Both"
                                                     , TRUE ~ Helped_Papers)
        
        , Helped_Position                = case_when(str_detect(Helped_Position, 'No one') ~ 'No One'
                                                     , Helped_Position == "Female;Male" ~ "Both"
                                                     , TRUE ~ Helped_Position)
        
        , Helped_Programme               = case_when(str_detect(Helped_Programme, 'No one') ~ 'No One'
                                                     , Helped_Programme == "Female;Male" ~ "Both"
                                                     , TRUE ~ Helped_Programme)
        
        , Helped_Apprenticeshi           = case_when(str_detect(Helped_Apprenticeship, 'No one') ~ 'No One'
                                                     , Helped_Apprenticeship == "Female;Male" ~ "Both"
                                                     , TRUE ~ Helped_Apprenticeship)
        
        , Helped_Employment              = case_when(str_detect(Helped_Employment, 'No one') ~ 'No One'
                                                     , Helped_Employment == "Female;Male" ~ "Both"
                                                     , TRUE ~ Helped_Employment)
        
        , Helped_Study                   = case_when(str_detect(Helped_Study, 'No one') ~ 'No One'
                                                     , Helped_Study == "Female;Male" ~ "Both"
                                                     , TRUE ~ Helped_Study)
        
        , Helped_SupportEducation        = case_when(str_detect(Helped_SupportEducation, 'No one') ~ 'No One'
                                                     , Helped_SupportEducation == "Female;Male" ~ "Both"
                                                     , TRUE ~ Helped_SupportEducation)
        
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
                                                        
        
        ## All variables to de-coalesce and count
        , EconomicOpportunity = case_when(str_detect(PersonalEngagement, pattern = "Economic opportunity") ~ 1
                                        , is.na(PersonalEngagement) ~ NA_real_
                                        , TRUE ~ 0)
        
        , Conservation = case_when(str_detect(PersonalEngagement, pattern = "Conservation") ~ 1
                                   , is.na(PersonalEngagement) ~ NA_real_
                                   , TRUE ~ 0)
        
        , CapacityDevelopment = case_when(str_detect(PersonalEngagement, pattern = "Capacity development") ~ 1
                                          , is.na(PersonalEngagement) ~ NA_real_
                                          , TRUE ~ 0)
        
        # All variables to Coalesce
        , AwareGenderPayGap = coalesce(AwareGenderPayGap, AwareGenderPayGap_Ed)

  ) %>% 
  # select(-ends_with("_Ed")) %>%
  saveRDS("ISA_Raw_Ind.rds")        

dd = readRDS("ISA_Raw_Ind.rds")

dd %>% select(NewPositionReason) %>% distinct()  
dd %>% select(NewPositionReason) %>% table()


# Count total number of responses per column
dd %>% summarise(across(everything(), ~ sum(!is.na(.)))) %>%  t() %>% write.csv(file = "n_per_column.csv")

dd %>% select(EconomicOpportunity:CapacityDevelopment)

toto = dd %>% 
  select(PersonalEngagement) %>% 
  mutate(EconomicOpportunity = case_when(str_detect(PersonalEngagement, pattern = "Economic opportunity") ~ 1
                                      , is.na(PersonalEngagement) ~ NA_real_
                                      , TRUE ~ 0)
         
         , Conservation = case_when(str_detect(PersonalEngagement, pattern = "Conservation") ~ 1
                                           , is.na(PersonalEngagement) ~ NA_real_
                                           , TRUE ~ 0)
         
         , CapacityDevelopment = case_when(str_detect(PersonalEngagement, pattern = "Capacity development") ~ 1
                                           , is.na(PersonalEngagement) ~ NA_real_
                                           , TRUE ~ 0)
  )




pretty_strings <- function(string) {
  
  blankCount = str_count(string, pattern = " ")
  
  # If only one space, replace with \n
  if(blankCount == 1){
    str_replace(string, " ", "\n")
  } else if (blankCount == 2) { # if 2 spaces, put beside longest word
    
  } else if (blankCount == 3) { # If 3 spaces, put after 2nd one
    
  } else if (blankCount > 3) { # If 4 or more, put every 2nd space
    
  }
  
}


table_plus <- function(n) {
  print(colnames(dd)[n-1])
  print(table(dd[[n-1]]))
  print(colnames(dd)[n-1])
}

table_plus(3)

 
# read_csv("Institutional Survey.csv") %>%
#   rename(Country = `Please enter the name of your country.`) %>%
#   mutate(
#     Country = str_to_title(Country)
#   ) %>%
#   saveRDS("ISA_Raw_Inst.rds")


# ## Ignore
# 
# dd = read_csv("Institutional Survey.csv")
# data.frame(InternalName = NA, iBinary = NA, ExternalName = names(dd)) %>% write_csv("ColumnNameLookupInst.csv")
# 
# table(dd$Country)
# 
# 
# LEAST DEVELOPED COUNTRIES (LDCs) (24 countries)
# SIDS
# Region
# Lower Income
# Lower Middle Income
# Upper Middle Income
# Africa
# 
# Comoros
# 
# Asia
# 
# Timor-Leste
# 
# Pacific
# 
# Vanuatu
# Tuvalu
# 
# Kiribati
# 
# 
# Sao Tome & Principe
# 
# Latin America and the Caribbean
# Haiti
# 
# 
# Landlocked
# Region
# Lower Income
# Lower Middle Income
# Upper Middle Income
# Africa
# Malawi
# Lesotho
# 
# Uganda
# Chad
# Zambia
# Burkina Faso
# Niger
# Asia
# Nepal
# Lao Peoples Democratic Republic
# Africa
# Mozambique
# Djibouti
# Madagascar
# Mauritania
# Guinea-Bissau
# Benin
# Democratic Republic of the Congo
# Asia
# Bangladesh
# Myanmar
# 
# data.frame(
#   Country = c("")
#   , Region = c("Asia-Pacific")
#   , Type = c("LDC", "LLDC", "SIDS")
#   , Income = c("Lower", "Lower Middle", "Upper Middle")
# )
# 
# 
# Zimbabwe
# Botswana
# 
# Asia- Pacific
# Mongolia
# Azerbaijan
# 
# Moldova
# Kazakhstan
# 
# Latin America and the Caribbean
# Bolivia
# Paraguay
# 
# 
# SMALL ISLAND DEVELOPING STATES (17 countries)
# Region
# Lower Middle Income
# Upper Middle Income
# High Income
# Africa
# Cabo Verde
# 
# Mauritius
# 
# 
# Seychelles
# Asia- Pacific
# Papua New Guinea
# Maldives
# Nauru
# 
# Fiji
# Singapore
# 
# Cook Island
# Tonga
# 
# Middle East
# Bahrain
# 
# Latin America and the Caribbean
# Jamaica
# Trinidad and Tobago
# Suriname
# St. Kitts and Nevis
# Belize
# Cuba
# 
