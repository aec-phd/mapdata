# Designed to be called from Create_Other_MSOA.R

library(tidyverse)
library(sqldf)
library(stringr)
library(curl)
library(readr)
library(fingertipsR)
library(DescTools)

# Read in House of Commons Library MSOA names

# Method of downloading and opening .csv file taken from Colin Angus's example at https://github.com/VictimOfMaths/Experiments/blob/master/COVIDLAHeatmap.R

temp <- tempfile()
source <- "https://houseofcommonslibrary.github.io/msoanames/MSOA-Names-2.2.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
msoanames <- read.csv(temp) %>% rename(AreaCode = 1) %>% # 1st column name is corrupted
  select(AreaCode,msoa21hclnm)

setwd('C:/Users/aecun/OneDrive/Documents/Blackpool Mapping/mapdata')

metadata <- read_csv("Metadata.csv")

our_inds <- c(93283)

BPdata <- fingertips_data(ProfileID = 143, AreaTypeID = 3, IndicatorID = our_inds) %>%
  filter(ParentName %in% c('Blackpool','Lancashire'),AreaType == "MSOA") %>%
  filter(AreaName %like% "Blackpool%" | AreaName %like% "Fylde%" | AreaName %like% "Wyre%")

fingertips.switch <- function(x,y=NULL)
{
  ifelse(x == 93283 & y == "Male","LE_male",
  ifelse(x == 93283 & y == "Female","LE_female",""                                                                                                                                                                 
 ))                                                                                                                                                                                                                       
}

# Attach House of Commons MSOA names
BPdata <- BPdata %>% left_join(msoanames,by = "AreaCode")

# Just selecting expectancies expressed in years
BPExpectancies <- BPdata %>% filter(`IndicatorID` %in% c(93283,93298))
BPExpectancies <- BPExpectancies %>% transmute(IndID = fingertips.switch(`IndicatorID`,Sex),
                                                         polycode = `AreaCode`,
                                                         value = Value,
                                                         label = paste0("MSOA: ",`AreaCode`,"<br/>",
                                                                        "(aka ",sQuote(msoa21hclnm),")<br/>",
                                                                        ifelse(is.na(value),"",
                                                                               ifelse(`IndicatorID` == 93283,"Life Expectancy: ","Healthy Life Expectancy: ")),
                                                                        ifelse(is.na(value),"No data for this area",round(Value,digits=1)),"<br/>",
                                                                        ifelse(`ComparedtoEnglandvalueorpercentiles` ==                                                                                      "Similar","Similar to England",
                                                                               ifelse(`ComparedtoEnglandvalueorpercentiles` ==                                                                                    "Higher", "Higher than England",
                                                                                      ifelse(`ComparedtoEnglandvalueorpercentiles` ==       
                                                                                               "Lower", "Lower than England",
                                                                                             ifelse(`ComparedtoEnglandvalueorpercentiles` ==
                                                                                                      "Better", "Better than England",
                                                                                                    ifelse(`ComparedtoEnglandvalueorpercentiles` ==
                                                                                                             "Worse", "Worse than England","")))))))

# If we have data for the chosen MSOA, append the England average (stored in metadata as 'DivergePoint') to the label for comparison
BPExpectancies <- BPExpectancies %>% left_join(metadata,by = "IndID") %>%
  mutate(label2 = ifelse(str_detect(label,"England$"),paste0(label," (= ",round(DivergePoint,digits=1),")"),label)) %>%
  select(IndID,polycode,value,label=label2)

LocalHealth <- BPExpectancies # gets picked up and used by Create_Other_MSOA.R
