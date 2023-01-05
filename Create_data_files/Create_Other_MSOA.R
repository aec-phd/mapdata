library(tidyverse)
library(sqldf)
library(stringr)
library(readxl)
library(readr)
library(curl)

# Read in House of Commons Library MSOA names

# Method of downloading and opening .csv file taken from Colin Angus's example at https://github.com/VictimOfMaths/Experiments/blob/master/COVIDLAHeatmap.R

temp <- tempfile()
source <- "https://houseofcommonslibrary.github.io/msoanames/MSOA-Names-Latest2.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
msoanames <- read.csv(temp) %>% rename(polycode = 1) %>% # 1st column name is corrupted
select(polycode,msoa21hclnm)

setwd('C:/Users/aecun/OneDrive/Documents/Blackpool Mapping/mapdata')

# NB - except for Standardised Ratios, the variable must already be in the Metadata.csv file, 
# because we need to take the England average from the 'DivergePoint' column
metadata <- read_csv("Metadata.csv")

#################################################################################################
# Now reading the ONS MSOA-level modelled income estimates (Equivalised Before Housing Costs)   #
#################################################################################################

# NB - not using read.csv because it doesn't handle the commas in the amounts of money properly

BHC <- read_csv("https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/smallareaincomeestimatesformiddlelayersuperoutputareasenglandandwales/financialyearending2018/netannualincomebeforehousingcosts2018.csv",skip=5,n_max=7201,
                   col_names = c("MSOA code","MSOA name","LA code","LA name","Region code","Region name",
                                 "Income","X8","X9","X10"))
BHC <- BHC %>%
  select(`MSOA code`,`MSOA name`,`LA code`,`LA name`,`Region code`,`Region name`,`Income`) %>%
  filter(`Region name` != "Wales")

BHC$rank <- as.integer(floor(rank(as.numeric(BHC$Income))))
BHC$decile <- pmin(((BHC$rank-1)%/%679)+1,10)
BHC$decile <- factor(BHC$decile)
LocalBHC <- BHC %>% filter(`LA name` %in% c('Blackpool','Fylde','Wyre')) %>% add_column(IndID = "Net_income_BHC")

# Attach House of Commons MSOA names
LocalBHC <- LocalBHC %>% left_join(msoanames,by = c("MSOA code" = "polycode"))

LocalBHC <- LocalBHC %>%
  mutate(label = paste0("MSOA: ", `MSOA code`,"<br/>",
                        "(aka ",sQuote(msoa21hclnm),")<br/>",
                        "Net Income BHC: £",Income,"<br>",
                        "Rank: ",rank," (out of 6791)","<br>",
                        "Decile: ",decile," (out of 10)","<br>","(Lowest ranks/deciles = lowest income)")) %>%
  select(IndID,polycode =`MSOA code`,value = decile,label)

#################################################################################################
# Now reading the ONS MSOA-level modelled income estimates (Equivalised After Housing Costs)    #
#################################################################################################

# NB - not using read.csv because it doesn't handle the commas in the amounts of money properly

AHC <- read_csv("https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/smallareaincomeestimatesformiddlelayersuperoutputareasenglandandwales/financialyearending2018/netannualincomeafterhousingcosts2018.csv",skip=5,n_max=7201,
                col_names = c("MSOA code","MSOA name","LA code","LA name","Region code","Region name",
                              "Income","X8","X9","X10"))
AHC <- AHC %>%
  select(`MSOA code`,`MSOA name`,`LA code`,`LA name`,`Region code`,`Region name`,`Income`) %>%
  filter(`Region name` != "Wales")

AHC$rank <- as.integer(floor(rank(as.numeric(AHC$Income))))
AHC$decile <- pmin(((AHC$rank-1)%/%679)+1,10)
AHC$decile <- factor(AHC$decile)
LocalAHC <- AHC %>% filter(`LA name` %in% c('Blackpool','Fylde','Wyre')) %>% add_column(IndID = "Net_income_AHC")

# Attach House of Commons MSOA names
LocalAHC <- LocalAHC %>% left_join(msoanames,by = c("MSOA code" = "polycode"))

LocalAHC <- LocalAHC %>%
  mutate(label = paste0("MSOA: ", `MSOA code`,"<br/>",
                        "(aka ",sQuote(msoa21hclnm),")<br/>",
                        "Net Income AHC: £",Income,"<br>",
                        "Rank: ",rank," (out of 6791)","<br>",
                        "Decile: ",decile," (out of 10)","<br>","(Lowest ranks/deciles = lowest income)")) %>%
  select(IndID,polycode =`MSOA code`,value = decile,label)

###############################################
# Now reading the ONS MSOA-level house prices #
###############################################
temp <- tempfile()
temp2 <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/housing/datasets/hpssadataset2medianhousepricebymsoaquarterlyrollingyear/current/hpssadataset2medianpricepaidbymsoa.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)
options(scipen = 999) # don't want scientific notation
HousePrices <- read_excel(file.path(temp2,"HPSSA Dataset 2 - Median price paid by MSOA.xls"), sheet = '1a', range = "A6:DF7207", na = ":") %>% 
  filter(`Local authority name` %in% c('Blackpool','Fylde','Wyre')) 

latestyear <- tail(names(HousePrices),1) # i.e. name of last column, something like 'Year ending Mar 2019'
HousePrices <- HousePrices %>%
  select(polycode = `MSOA code`,value = tail(names(.), 1)) %>% # i.e. take value from last column
  add_column(IndID = "House_Prices_MSOA") %>%
  left_join(metadata, by = "IndID") %>%
  select(IndID,polycode,value,England=DivergePoint) %>%
  left_join(msoanames,by = "polycode") %>% # Attach House of Commons MSOA names
  mutate(label = paste0("MSOA: ",polycode,"<br/>",
                        "(aka ",sQuote(msoa21hclnm),")<br/>",
                        "Median house price for","<br/>",latestyear,": ",
                        ifelse(is.na(value),"<br/>N/A (fewer than 5 sales)",
                               paste0("£",prettyNum(value,format = "g",decimal.mark = ".",big.mark = ",",drop0trailing = TRUE))),"<br/>",
                        "(England average = £",prettyNum(England*1000,format = "g",decimal.mark = ".",big.mark = ",",drop0trailing = TRUE),")")) %>%
  mutate(value = round(value/1000)) %>%
  select(IndID,polycode,value,label)

###############################################
# Reading Nomis data on private rented tenure
###############################################

PrivRent <- read.csv('https://www.nomisweb.co.uk/api/v01/dataset/NM_2072_1.data.csv?date=latest&geography=637536726...637536744,637539209...637539217,637539319...637539332&c2021_tenure_9=1004&measures=20100,20301&select=geography_code,measures_name,obs_value') %>%
  pivot_wider(names_from = MEASURES_NAME, values_from = OBS_VALUE) %>%
  rename(polycode = GEOGRAPHY_CODE, value = Percent,housecount = Value) %>%
  add_column(IndID = "Private_rented_MSOA") %>%
  left_join(msoanames,by = "polycode") %>% # Attach House of Commons MSOA names
  mutate(label = paste0("MSOA: ",polycode,"<br/>",
                        "(aka ",sQuote(msoa21hclnm),")<br/>",
                        housecount," properties are privately rented, <br/>",
                        "which is ",round(value,digits=1),"% of the housing stock <br/>",
                        "(England average 20.5%) <br/>",
                        "<em>(2021 Census results)</em>")) %>%
  select(IndID,polycode,value,label)

#################################################################
# Call the routine that creates MSOA-level data from Local Health
#################################################################
source("Create_data_files/Create_Local_Health_MSOA.R") # produces a dataframe called 'LocalHealth'


#######################################################
# Write all the above to single Other_MSOA.csv file
#######################################################

# Fingertips API was not working on 5/1/23 so did not do this:
# localOther <- rbind(HousePrices, LocalBHC, LocalAHC, LocalHealth, PrivRent)

# Did this instead:
existingLocalOther <- read.csv('Other_MSOA.csv',fileEncoding = 'UTF-8-BOM')
localOther <- rbind(existingLocalOther,PrivRent)

write_excel_csv(localOther,'Other_MSOA.csv')
