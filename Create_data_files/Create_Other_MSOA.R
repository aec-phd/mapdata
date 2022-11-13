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
                               paste0("?",prettyNum(value,format = "g",decimal.mark = ".",big.mark = ",",drop0trailing = TRUE))),"<br/>",
                        "(England average = ?",prettyNum(England*1000,format = "g",decimal.mark = ".",big.mark = ",",drop0trailing = TRUE),")")) %>%
  mutate(value = round(value/1000)) %>%
  select(IndID,polycode,value,label)

#######################################################
# Write all the above to single Other_MSOA.csv file
#######################################################

# PennineOther <- rbind(PennineNCMP,PennineIncome, HousePrices)
# write_excel_csv(PennineOther,'PennineOther_MSOA.csv')
write_excel_csv(HousePrices,'Other_MSOA.csv')
