library(tidyverse)

countryLookup = read_csv("ISA – WIDSR – Country Lookup.csv", n_max = 17, na = c("N/A")) %>%
  select(Country:GGI)

ourNamesInd = read_csv("ColumnNameLookupInd.csv")

read_csv("Individual Survey.csv", col_names = ourNamesInd$InternalName, skip=3, na = c(""," ", "N / A")) %>%
  
  mutate( Country = str_to_title(Country)
          
          , NumPubs_PatentApps = as.numeric(NumPubs_PatentApps) # correct data type
          , NumPubs_Patents = as.numeric(NumPubs_Patents)
          
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
          
          ## Try to replace NumPubs with one line code below. 
          ## %>% mutate_at(vars(contains("NumPubs")), list(~case_when(.==0.1 ~ 0  
          ##                                                          , TRUE ~ .x ) # Right now this .x does not work.
          
          ## Coalesce all _Ed variables
          , AwareGenderPayGap = coalesce(AwareGenderPayGap, AwareGenderPayGap_Ed)
          
  ) %>% 
  # select(-ends_with("_Ed")) %>%
  filter(Consent == "I consent") %>%
  select(-c(Timestamp, Consent)) %>%
  left_join(countryLookup, by = "Country") %>%
  saveRDS("ISA_Raw_Ind.rds")

dd = readRDS("ISA_Raw_Ind.rds")

read_csv("Institutional Survey.csv") %>%
  rename(Country = `Please enter the name of your country.`) %>%
  mutate(
    Country = str_to_title(Country)
  ) %>%
  saveRDS("ISA_Raw_Inst.rds")

# "Thapelo Jacobs". I think I've found this person, and that the response is from Botswana.

# dd %>% filter(!is.na(Email)) %>% select(Country, Institution, Email) %>% write_csv("EmailList.csv")
# dd = read_csv("Institutional Survey.csv")
# data.frame(InternalName = NA, iBinary = NA, ExternalName = names(dd)) %>% write_csv("ColumnNameLookupInst.csv")

pretty_strings <- function(string) {
  # If only one space, replace with \n
  # if 2 spaces, put beside longest word
  # If 3 spaces, put after 2nd one
  # If 4 or more, put every 2nd space
  
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