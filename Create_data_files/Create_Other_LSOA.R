library(tidyverse)
library(readxl)
library(curl)

setwd('C:/Users/aecun/OneDrive/Documents/Blackpool Mapping/mapdata')

# NB - except for Standardised Ratios, the variable must already be in the Metadata.csv file, 
# because we need to take the England average from the 'DivergePoint' column

#  Read metadata
metadata <- read_csv("Metadata.csv")

################ Fuel Poverty ##################
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1081191/sub-regional-fuel-poverty-2022-tables.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

FuelPov <- read_excel(temp,sheet="Table 3",range="A3:H32847",col_names = TRUE) %>% filter(`LA Name` %in% c('Blackpool','Fylde','Wyre')) %>%
add_column(IndID = "Fuel_Poverty")

# Append the England average (stored in metadata as 'DivergePoint') for comparison
FuelPov <- FuelPov %>% left_join(metadata,by = "IndID") %>%
  select(IndID,polycode = `LSOA Code`,value = `Proportion of households fuel poor (%)`,FPhhlds =`Number of households in fuel poverty`,England=DivergePoint) %>%
  mutate(label = paste0("LSOA: ",polycode,"<br/>",
                        FPhhlds," households in this LSOA are fuel poor<br/>",
                        "which is ",paste0(round(value,digits=1),"% of all households<br/>"),
                        "(England average = ",paste0(round(England,digits=1),"%)")))  %>%
  select(IndID,polycode,value,label)

################# House Prices #################
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/housing/datasets/medianpricepaidbylowerlayersuperoutputareahpssadataset46/current/hpssadataset46medianpricepaidforresidentialpropertiesbylsoa.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

options(scipen = 999) # don't want scientific notation
HousePrices <- read_excel(temp, sheet = 'Data', range = "A6:DF34759", na = ":") %>% filter(`Local authority name` %in% c('Blackpool','Fylde','Wyre')) 
latestyear <- tail(names(HousePrices),1) # i.e. name of last column, something like 'Year ending Mar 2019'
HousePrices <- HousePrices %>%
  select(polycode = `LSOA code`,value = tail(names(.), 1)) %>% # i.e. take value from last column
  add_column(IndID = "House_Prices_LSOA") %>%
  left_join(metadata, by = "IndID") %>%
  select(IndID,polycode,value,England=DivergePoint) %>%
  mutate(label = paste0("LSOA: ",polycode,"<br/>",
                        "Median house price for","<br/>",latestyear,": ",
                        ifelse(is.na(value),"<br/>N/A (fewer than 5 sales)",
                               paste0("£",prettyNum(value,format = "g",decimal.mark = ".",big.mark = ",",drop0trailing = TRUE))),"<br/>",
                        "(England average = £",prettyNum(England*1000,format = "g",decimal.mark = ".",big.mark = ",",drop0trailing = TRUE),")")) %>%
  mutate(value = round(value/1000)) %>%
  select(IndID,polycode,value,label)

Other <- bind_rows(FuelPov,HousePrices)
write_csv(Other,"Other_LSOA.csv")
