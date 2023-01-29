library(tidyverse)
library(sqldf)
library(stringr)

setwd('C:/Users/aecun/OneDrive/Documents/Blackpool Mapping/mapdata')

Dep <- read.csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833982/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators.csv",header=TRUE)

Dep <- Dep %>% 
  filter(Local.Authority.District.name..2019. %in% c('Blackpool','Fylde','Wyre')) %>%
  select(lsoa11cd = ends_with("LSOA.code..2011."), # beginning of field name seems to be corrupted
         IMDScore = Index.of.Multiple.Deprivation..IMD..Score,
         IMDRank = Index.of.Multiple.Deprivation..IMD..Rank..where.1.is.most.deprived.,
         IMDDecile = Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.,
         IncomeScore = Income.Score..rate.,
         IncomeRank = Income.Rank..where.1.is.most.deprived.,
         IncomeDecile = Income.Decile..where.1.is.most.deprived.10..of.LSOAs.,
         EmpScore = Employment.Score..rate.,
         EmpRank = Employment.Rank..where.1.is.most.deprived.,
         EmpDecile = Employment.Decile..where.1.is.most.deprived.10..of.LSOAs.,
         EducScore = Education..Skills.and.Training.Score,
         EducRank = Education..Skills.and.Training.Rank..where.1.is.most.deprived.,
         EducDecile = Education..Skills.and.Training.Decile..where.1.is.most.deprived.10..of.LSOAs.,
         HealthScore = Health.Deprivation.and.Disability.Score,
         HealthRank = Health.Deprivation.and.Disability.Rank..where.1.is.most.deprived.,
         HealthDecile = Health.Deprivation.and.Disability.Decile..where.1.is.most.deprived.10..of.LSOAs.,
         CrimeScore = Crime.Score,
         CrimeRank =  Crime.Rank..where.1.is.most.deprived.,
         CrimeDecile = Crime.Decile..where.1.is.most.deprived.10..of.LSOAs.,
         BarriersScore = Barriers.to.Housing.and.Services.Score,
         BarriersRank = Barriers.to.Housing.and.Services.Rank..where.1.is.most.deprived.,
         BarriersDecile = Barriers.to.Housing.and.Services.Decile..where.1.is.most.deprived.10..of.LSOAs.,
         LivEnvScore = Living.Environment.Score,
         LivEnvRank = Living.Environment.Rank..where.1.is.most.deprived.,
         LivEnvDecile = Living.Environment.Decile..where.1.is.most.deprived.10..of.LSOAs.,
         IDACIScore = Income.Deprivation.Affecting.Children.Index..IDACI..Score..rate.,
         IDACIRank = Income.Deprivation.Affecting.Children.Index..IDACI..Rank..where.1.is.most.deprived.,
         IDACIDecile  = Income.Deprivation.Affecting.Children.Index..IDACI..Decile..where.1.is.most.deprived.10..of.LSOAs.,
         IDAOPIScore = Income.Deprivation.Affecting.Older.People..IDAOPI..Score..rate.,
         IDAOPIRank = Income.Deprivation.Affecting.Older.People..IDAOPI..Rank..where.1.is.most.deprived.,
         IDAOPIDecile = Income.Deprivation.Affecting.Older.People..IDAOPI..Decile..where.1.is.most.deprived.10..of.LSOAs.) %>%
  gather(key=type,value=value,-lsoa11cd) %>% 
  mutate(measure = str_extract(type,"Score$|Rank$|Decile$")) %>%
  mutate(index_domain = str_replace(type,"Score|Rank|Decile",""))

  Dep$index_domain <- recode(Dep$index_domain,"IMD" = "IMD",
         "Income" = "Income_dep",
         "Emp" = "Emp_dep",
         "Educ" = "Educ_dep",
         "Health" = "Health_dep",
         "Crime" = "Crime_dep",
         "Barriers" = "Barriers_dep",
         "LivEnv" = "Env_dep",
         "IDACI" = "IDACI",
         "IDAOPI" = "IDAOPI")
  Dep <- select(Dep,IndID = index_domain, polycode = lsoa11cd,measure,value) %>%
    spread(measure, value) %>% 
    rename(score = Score, rank = Rank, decile = Decile) %>%
    mutate(label = paste0("LSOA: ", polycode,"<br/>",
                          "Rank: ",rank," (out of 32844)","<br>",
                          "Decile: ",decile," (out of 10)","<br>","(Lowest ranks/deciles = most deprived)")) %>%
    select(IndID,polycode,value = decile,label) %>%
    mutate(value = as.integer(value))
  
write_csv(Dep,"IMD_2019_LSOA.csv")

# Mood and anxiety composite indicator comes from a different file, and has been manipulated in Excel to assign deciles using the PHE tool at https://fingertips.phe.org.uk/profile/guidance

MoodDep <- read.csv("MoodAnxietyDep.csv") %>%
  filter(Local.Authority.District.name..2019. %in% c('Blackpool','Fylde','Wyre')) %>%
  add_column(IndID = "Mood_dep") %>%
  rename(polycode = `LSOA.code..2011.`,rank=MoodRank,decile=MoodDecile) %>%
  mutate(label = paste0("LSOA: ", polycode,"<br/>",
                        "Rank: ",rank," (out of 32844)","<br>",
                        "Decile: ",decile," (out of 10)","<br>","(Lowest ranks/deciles = most deprived)")) %>%
  select(IndID,polycode,value = decile,label)

# Was adding MoodDep later, without running the whole script
Dep <- read.csv('IMD_2019_LSOA.csv',fileEncoding = 'UTF-8-BOM') # not needed if running whole script
write_csv(bind_rows(Dep,MoodDep),"IMD_2019_LSOA.csv")
