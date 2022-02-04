library(tidyverse)

ourNamesInd = read_csv("ColumnNameLookupInd.csv")

dd = read_csv("Individual Survey.csv", col_names = ourNamesInd$InternalName, skip=3, na = "N / A") %>%
  mutate(
    Country = str_to_title(Country)
  ) %>%
  mutate(
    
    
    
    
    
  ) %>%
  saveRDS("ISA_Raw_Ind.rds")

read_csv("Institutional Survey.csv") %>%
  rename(Country = `Please enter the name of your country.`) %>%
  mutate(
    Country = str_to_title(Country)
  ) %>%
  saveRDS("ISA_Raw_Inst.rds")


## Ignore

dd = read_csv("Institutional Survey.csv")
data.frame(InternalName = NA, iBinary = NA, ExternalName = names(dd)) %>% write_csv("ColumnNameLookupInst.csv")

table(dd$Country)

LEAST DEVELOPED COUNTRIES (LDCs) (24 countries)
SIDS
Region
Lower Income
Lower Middle Income
Upper Middle Income
Africa

Comoros

Asia

Timor-Leste

Pacific

Vanuatu
Tuvalu

Kiribati


Sao Tome & Principe

Latin America and the Caribbean
Haiti


Landlocked
Region
Lower Income
Lower Middle Income
Upper Middle Income
Africa
Malawi
Lesotho

Uganda
Chad
Zambia
Burkina Faso
Niger
Asia
Nepal
Lao Peoples Democratic Republic
Africa
Mozambique
Djibouti
Madagascar
Mauritania
Guinea-Bissau
Benin
Democratic Republic of the Congo
Asia
Bangladesh
Myanmar

data.frame(
  Country = c("")
  , Region = c("Asia-Pacific")
  , Type = c("LDC", "LLDC", "SIDS")
  , Income = c("Lower", "Lower Middle", "Upper Middle")
)


Zimbabwe
Botswana

Asia- Pacific
Mongolia
Azerbaijan

Moldova
Kazakhstan

Latin America and the Caribbean
Bolivia
Paraguay


SMALL ISLAND DEVELOPING STATES (17 countries)
Region
Lower Middle Income
Upper Middle Income
High Income
Africa
Cabo Verde

Mauritius


Seychelles
Asia- Pacific
Papua New Guinea
Maldives
Nauru

Fiji
Singapore

Cook Island
Tonga

Middle East
Bahrain

Latin America and the Caribbean
Jamaica
Trinidad and Tobago
Suriname
St. Kitts and Nevis
Belize
Cuba

