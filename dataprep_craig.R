library(tidyverse)
source("Helpers.R")

countryLookup = read_csv("ISA – WIDSR – Country Lookup.csv", n_max = 17, na = c("N/A")) %>%
  select(Country:GGI)

ourNamesInd = read_csv("ColumnNameLookupInd.csv")

read_csv("Individual Survey.csv", col_names = ourNamesInd$InternalName, skip = 3, na = c(""," ", "N / A")) %>%
  
  mutate( Country = str_to_title(Country)
          
          , NumPubs_PatentApps = as.numeric(NumPubs_PatentApps) # correct data type
          , NumPubs_Patents = as.numeric(NumPubs_Patents)
          
          , Country               = case_when(  str_detect(Country, 'Alice|Botsawana')       ~ 'Botswana'
                                                , str_detect(Country, 'Trinidad|Tobago')       ~ "Trinidad & Tobago" 
                                                , str_detect(Country, 'Kitts|Nevis')           ~ "St. Kitts & Nevis"        
                                                , str_detect(Country, 'Maldives')              ~ "Maldives"
                                                , str_detect(Country, 'Fiji')                  ~ "Fiji"
                                                , str_detect(Country, 'Mauritus')              ~ "Mauritius"
                                                , str_detect(Country, 'Mocambique|Moçambique|Tabitha') ~ 'Mozambique'
                                                , TRUE ~ Country)
          
          , Age = fct_relevel(Age, c("18 - 24", "25 - 34"))
          # , Field = pretty_strings(Field)
          
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
          
          , iPartnerWorkingForMoney_Ed     = case_when(str_detect(iPartnerWorkingForMoney_Ed, 'full') ~ 'Yes, full-time'
                                                       , str_detect(iPartnerWorkingForMoney_Ed, 'part') ~ 'Yes, part-time'
                                                       , TRUE ~ iPartnerWorkingForMoney_Ed)
          
          , GendersTreatedEquallyAtWork    = case_when(str_detect(GendersTreatedEquallyAtWork, 'sometimes') ~ 'Sometimes'
                                                       , str_detect(GendersTreatedEquallyAtWork, 'No') ~ 'Consistent \nInequities'
                                                       , str_detect(GendersTreatedEquallyAtWork, 'Yes') ~ 'Always \nEquitable')
          
          , EthnicityTreatedEquallyAtWork  = case_when(str_detect(EthnicityTreatedEquallyAtWork, 'sometimes') ~ 'Sometimes'
                                                       , str_detect(EthnicityTreatedEquallyAtWork, 'No') ~ 'Consistent \nInequities'
                                                       , str_detect(EthnicityTreatedEquallyAtWork, 'Yes') ~ 'Always \nEquitable')
          
          , AwareGenderPayGap              = case_when(str_detect(AwareGenderPayGap, 'Yes') ~ 'Yes, men make more'
                                                       , TRUE ~ AwareGenderPayGap)
          , AwareGenderPayGap_Ed           = case_when(str_detect(AwareGenderPayGap_Ed, 'Yes') ~ 'Yes, men make more' 
                                                       , TRUE ~ AwareGenderPayGap_Ed)
          
          , iGendersTreatedEquallyAtSchool = case_when(str_detect(iGendersTreatedEquallyAtSchool, 'sometimes') ~ 'Sometimes'
                                                       , str_detect(iGendersTreatedEquallyAtSchool, 'No') ~ 'Consistent \nInequities'
                                                       , str_detect(iGendersTreatedEquallyAtSchool, 'Yes') ~ 'Always \nEquitable')
          
          , iWomenMinorityOverlooked_Ed    = case_when(str_detect(iWomenMinorityOverlooked_Ed, 'No') ~  'Equitable \nTreatment'
                                                       , str_detect(iWomenMinorityOverlooked_Ed, 'Yes') ~  'Inequitable \nTreatment')
          , iWomenMinorityOverlooked       = case_when(str_detect(iWomenMinorityOverlooked, 'No') ~  'Equitable \nTreatment'
                                                       , str_detect(iWomenMinorityOverlooked, 'Yes') ~  'Inequitable \nTreatment')
          
          
          , NumPubs_Articles               = if_else(NumPubs_Articles == 0.1, 0, NumPubs_Articles)
          , NumPubs_Books                  = if_else(NumPubs_Books == 0.1, 0, NumPubs_Books)
          , NumPubs_Chapters               = if_else(NumPubs_Chapters == 0.1, 0, NumPubs_Chapters)
          , NumPubs_PatentApps             = if_else(NumPubs_PatentApps == 0.1, 0, NumPubs_PatentApps)
          , NumPubs_Patents                = if_else(NumPubs_Patents == 0.1, 0, NumPubs_Patents)
          
          ## Try to replace the above NumPubs_* with one line code as below. 
          ## %>% mutate_at(vars(contains("NumPubs")), list(~case_when(.==0.1 ~ 0, TRUE ~ .x ) # Right now this .x does not work.
          
          ## Coalesce all _Ed variables
          , AwareGenderPayGap = coalesce(AwareGenderPayGap, AwareGenderPayGap_Ed)
          , iDiscrimAtWork = coalesce(iDiscrimAtWork, iDiscrimAtWork_Ed)
          , BasisWorkDiscrim = coalesce(BasisWorkDiscrim, BasisWorkDiscrim_Ed)
          , RecipientLeers = coalesce(RecipientLeers, RecipientLeers_Ed)
          
          , DomesticArrangement = coalesce(DomesticArrangement, DomesticArrangement_Ed)
          , iDependants = coalesce(iDependants, iDependants_Ed)
          , ChildrenCaredForMethod = coalesce(ChildrenCaredForMethod, ChildrenCaredForMethod_Ed)
          , iPartnerWorkingForMoney = coalesce(iPartnerWorkingForMoney, iPartnerWorkingForMoney_Ed)
          
          , iProgrammeNotCompleted = coalesce(iProgrammeNotCompleted, iProgrammeNotCompleted_Ed)
          , ProgrammeNotCompleted_WhyNotComplete = coalesce(ProgrammeNotCompleted_WhyNotComplete, ProgrammeNotCompleted_WhyNotComplete_Ed)
          , ProgrammeNotCompleted_Level = coalesce(ProgrammeNotCompleted_Level, ProgrammeNotCompleted_Level_Ed)
          , ProgrammeNotCompleted_WhyEnrol = coalesce(ProgrammeNotCompleted_WhyEnrol, ProgrammeNotCompleted_WhyEnrol_Ed)
          , NumHrsHousehold_Shopping = coalesce(NumHrsHousehold_Shopping, NumHrsHousehold_Shopping_Ed)
          , NumHrsHousehold_Laundry = coalesce(NumHrsHousehold_Laundry, NumHrsHousehold_Laundry_Ed)
          , NumHrsHousehold_HouseMaintenance = coalesce(NumHrsHousehold_HouseMaintenance, NumHrsHousehold_HouseMaintenance_Ed)
          , NumHrsHousehold_Gardening = coalesce(NumHrsHousehold_Gardening, NumHrsHousehold_Gardening_Ed)
          , NumHrsHousehold_Food = coalesce(NumHrsHousehold_Food, NumHrsHousehold_Food_Ed)
          
          , iWomenMinorityOverlooked = coalesce(iWomenMinorityOverlooked, iWomenMinorityOverlooked_Ed)
          , ReasonsTurnDownTravel = coalesce(ReasonsTurnDownTravel, ReasonsTurnDownTravel_Ed)
          , ActivityFrequency_SciencePres = coalesce(ActivityFrequency_SciencePres, ActivityFrequency_SciencePres_Ed)
          , ActivityFrequency_RadioTV = coalesce(ActivityFrequency_RadioTV, ActivityFrequency_RadioTV_Ed)
          , ActivityFrequency_NewspaperMagazine = coalesce(ActivityFrequency_NewspaperMagazine, ActivityFrequency_NewspaperMagazine_Ed)
          , iWomenSuccessfulAsMen = coalesce(iWomenSuccessfulAsMen, iWomenSuccessfulAsMen_Ed)
          , iOSEquallySuitedGender = coalesce(iOSEquallySuitedGender, iOSEquallySuitedGender_Ed)
          
          , iFundingApplied5Years = coalesce(iFundingApplied5Years, iFundingApplied5Years_Ed)
          , GrantsResearchStay_National = coalesce(GrantsResearchStay_National, GrantsResearchStay_National_Ed)
          , GrantsResearchStay_International = coalesce(GrantsResearchStay_International, GrantsResearchStay_International_Ed)
          , GrantsAttendConf_National = coalesce(GrantsAttendConf_National, GrantsAttendConf_National_Ed)
          , GrantsAttendConf_International = coalesce(GrantsAttendConf_International, GrantsAttendConf_International_Ed)
  ) %>% 
  select(-ends_with("_Ed")) %>% # Remove '_Ed' variables now, they have been coalesced
  
  ## All variables to de-coalesce and count
  # DONE: Faizan - These need to be prefixed with the original column name
  # DONE: Faizan - Instead of removing spaces, replace with __ so that you can replace with a space downstream, otherwise it's unreadable
  # TODO: Faizan - Clean up Grants* columns - some of the regions overlap and can now be combined
  # TODO: Faizan - 
  # TODO: Faizan - 
  # TODO: Faizan - 
  sep_col("PersonalEngagement") %>%
  sep_col("PeopleInfluenced") %>%
  sep_col("ReasonsForLeavingJob") %>%
  sep_col("ReasonsForNotEnoughOppsConferences") %>%
  sep_col("NatureWorkDiscrim") %>%
  sep_col("BasisWorkDiscrim") %>%
  sep_col("TrainingSubsequentActivity") %>%
  sep_col("CareerProgressSupport") %>%
  sep_col("ProgrammeNotCompleted_WhyEnrol") %>%
  sep_col("ProgrammeNotCompleted_WhyNotComplete") %>%
  sep_col("InterruptionReasons") %>%
  sep_col("PositionsHeld") %>%
  sep_col("DifficultFulfillResp_Reasons") %>%
  sep_col("ReasonsTurnDownTravel") %>%
  
  filter(Consent == "I consent") %>%
  filter(iStudyOrEmployed == "Yes") %>%
  select(-c(Timestamp, Consent, iStudyOrEmployed)) %>%
  left_join(countryLookup, by = "Country") %>%
  saveRDS("ISA_Raw_Ind.rds")

dd = readRDS("ISA_Raw_Ind.rds")
dd$Field = pretty_strings(dd$Field)
  
dd %>% 
  select(-c(starts_with("Helped_")
                 , starts_with("EmploymentSatisfaction_")
                 , starts_with("PoliciesInOrg_")
                 , starts_with("OrgHavePolicies_")
                 , starts_with("Funding_")
                 , starts_with("InstitutionPolicies_")
                 , starts_with("ActivityFrequency_")
                 )) %>% 
  saveRDS("ISA_Ind.rds")



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


table_plus <- function(n) {
  print(colnames(dd)[n - 1])
  print(table(   dd[[n - 1]])   )
  print(colnames(dd)[n - 1])
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
