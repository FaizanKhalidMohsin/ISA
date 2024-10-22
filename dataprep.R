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
          , CurrentSituation = ifelse(grepl("enrolled", CurrentSituation), "Studying", "Working")
          , AnnualEarnings = clean_money(AnnualEarnings)
          
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
          
          ## Deal with numerics
          , Months = parse_number(MonthsUntilWork)
          , Months = ifelse(grepl("(a )|(an )", MonthsUntilWork, ignore.case = T), 1, Months)
          , Months = ifelse(grepl("(already)|(immediat)|(finish)|(before)", MonthsUntilWork, ignore.case = T), 0, Months)
          , Months = ifelse(grepl("one", MonthsUntilWork, ignore.case = T), 1, Months)
          , Months = ifelse(grepl("two", MonthsUntilWork, ignore.case = T), 2, Months)
          , Months = ifelse(grepl("nine", MonthsUntilWork, ignore.case = T), 9, Months)
          , MonthsUntilWorkNum = ifelse(grepl("year", MonthsUntilWork, ignore.case = T), Months*12, Months)
          
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
  mutate(across(starts_with("Helped_"), fix_gender_col)) %>%
  mutate(across(starts_with("NumPubs"), function(x){if_else(x == 0.1, 0, x)})) %>%
  mutate(across(starts_with("Funding_"), function(x){if_else(x %in% c("5-6", "7-8", "9-10", "10+"), "5+", x)})) %>%
  mutate(across(starts_with("Grants"), con_true_false)) %>%
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
  sep_col("StudyReasons") %>%
  sep_col("ReasonsForLeavingJob") %>%
  sep_col("ReasonsForNotEnoughOppsConferences") %>%
  sep_col("NatureWorkDiscrim") %>%
  sep_col("BasisWorkDiscrim") %>%
  sep_col("PersonalHarassed") %>%
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
  
  # mutate(across(where(is.character), pretty_strings)) %>% 
  saveRDS("ISA_Raw_Ind.rds")

dd = readRDS("ISA_Raw_Ind.rds")

# dd %>% select(MonthsUntilWork, Months, MonthsUntilWorkNum) %>% print(n = 150)

dd$Country = pretty_strings(dd$Country)
dd$Field = pretty_strings(dd$Field)
dd$Institution = pretty_strings(dd$Institution)
# dd$PeopleInfluenced = pretty_strings(dd$PeopleInfluenced)
# dd$NewPositionReason = pretty_strings(dd$NewPositionReason)
# dd$ReasonsForLeavingJob = pretty_strings(dd$ReasonsForLeavingJob)
# dd$ReasonsForNotEnoughOppsConferences = pretty_strings(dd$ReasonsForNotEnoughOppsConferences)
# dd$ChildrenCaredForMethod = pretty_strings(dd$ChildrenCaredForMethod)
# dd$ReasonsTurnDownTravel = pretty_strings(dd$ReasonsTurnDownTravel)
# dd$NatureWorkDiscrim = pretty_strings(dd$NatureWorkDiscrim)
# dd$BasisWorkDiscrim = pretty_strings(dd$BasisWorkDiscrim)
dd$iWomenMinorityOverlooked = pretty_strings(dd$iWomenMinorityOverlooked)
dd$iOSEquallySuitedGender = pretty_strings(dd$iOSEquallySuitedGender)
# dd$TrainingSubsequentActivity = pretty_strings(dd$TrainingSubsequentActivity)
# dd$CareerProgressSupport = pretty_strings(dd$CareerProgressSupport)
# dd$PositionsHeld = pretty_strings(dd$PositionsHeld)
# dd$Education = pretty_strings(dd$Education)
# dd$ProgrammeNotCompleted_WhyEnrol = pretty_strings(dd$ProgrammeNotCompleted_WhyEnrol)
# dd$ProgrammeNotCompleted_Level = pretty_strings(dd$ProgrammeNotCompleted_Level)
# dd$FinancialSupportSource = pretty_strings(dd$FinancialSupportSource)
# dd$FacultyAdvisorGender = pretty_strings(dd$FacultyAdvisorGender)
# dd$InterruptionReasons = pretty_strings(dd$InterruptionReasons)
# dd$ProgrammeNotCompleted_WhyEnrol_Ed = pretty_strings(dd$ProgrammeNotCompleted_WhyEnrol_Ed)
# dd$ProgrammeNotCompleted_WhyNotComplete_Ed = pretty_strings(dd$ProgrammeNotCompleted_WhyNotComplete_Ed)
# dd$DifficultFulfillResp_Reasons = pretty_strings(dd$DifficultFulfillResp_Reasons)
# dd$iPartnerWorkingForMoney = pretty_strings(dd$iPartnerWorkingForMoney)
# dd$NumHrsHousehold_Shopping = pretty_strings(dd$NumHrsHousehold_Shopping)
# DomesticArrangement_Ed
  
dd %>% 
  select(-c(starts_with("Helped_")
            , starts_with("EmploymentSatisfaction_", ignore.case = T)
            , starts_with("PoliciesInOrg_", ignore.case = T)
            , starts_with("OrgHavePolicies_", ignore.case = T)
            , starts_with("InstitutionPolicies_", ignore.case = T)
            , starts_with("OverallExperienceProgramme_", ignore.case = T)
            , starts_with("StudyReasons", ignore.case = T)
            , starts_with("PERSONALENGAGEMENT", ignore.case = T)
            , starts_with("PEOPLEINFLUENCED", ignore.case = T)
            , starts_with("REASONSFORLEAVINGJOB", ignore.case = T)
            , starts_with("REASONSFORNOTENOUGHOPPSCONFERENCES", ignore.case = T)
            , starts_with("NATUREWORKDISCRIM", ignore.case = T)
            , starts_with("BASISWORKDISCRIM", ignore.case = T)
            , starts_with("PersonalHarassed", ignore.case = T)
            , starts_with("TRAININGSUBSEQUENTACTIVITY", ignore.case = T)
            , starts_with("CAREERPROGRESSSUPPORT", ignore.case = T)
            , starts_with("PROGRAMMENOTCOMPLETED_WHYENROL", ignore.case = T)
            , starts_with("PROGRAMMENOTCOMPLETED_WHYNOTCOMPLETE", ignore.case = T)
            , starts_with("INTERRUPTIONREASONS", ignore.case = T)
            , starts_with("POSITIONSHELD", ignore.case = T)
            , starts_with("DIFFICULTFULFILLRESP_REASONS", ignore.case = T)
            , starts_with("ReasonsTurnDownTravel", ignore.case = T)
            , starts_with("Funding_", ignore.case = T)
            , starts_with("Grants", ignore.case = T)
            , starts_with("ActivityFrequency_", ignore.case = T)
            , starts_with("ParentalLeave_", ignore.case = T)
            , starts_with("NumHrsHousehold_", ignore.case = T)
            , starts_with("Dependants_", ignore.case = T)
                 )) %>% 
  saveRDS("ISA_Ind.rds")
# tt = readRDS("ISA_Ind.rds")


# read_csv("Institutional Survey.csv") %>%
#   rename(Country = `Please enter the name of your country.`) %>%
#   mutate(
#     Country = str_to_title(Country)
#   ) %>%
#   saveRDS("ISA_Raw_Inst.rds")
# 
# read_csv("National Survey.csv") %>%
#   rename(Country = `Please enter the name of your country.`) %>%
#   mutate(
#     Country = str_to_title(Country)
#   ) %>%
#   saveRDS("ISA_Raw_NFP.rds")



# "Thapelo Jacobs". I think I've found this person, and that the response is from Botswana.

# dd %>% filter(!is.na(Email)) %>% select(Country, Institution, Email) %>% write_csv("EmailList.csv")
# dd = read_csv("Institutional Survey.csv")
# data.frame(InternalName = NA, iBinary = NA, ExternalName = names(dd)) %>% write_csv("ColumnNameLookupInst.csv")


table_plus <- function(n) {
  print(colnames(dd)[n - 1])
  print(table(   dd[[n - 1]])   )
  print(colnames(dd)[n - 1])
}

# table_plus(3)


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
