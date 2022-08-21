# Georgia Clift (2022)
# Code to Reanalyse Fehr et al. (2008)'s data

#### LIBRARY ####

library("here") # to use with Rproject
library("readxl") # to read raw data
library("broom") # Convert statistical analysis objects from R into tidy format
library("tidyverse") # loads multiple tidyverse packages (see explanation below)
library("writexl") # to write excel file with output values

# use of tidyverse packages
# "ggplot2", for data visualisation
# "dplyr", for data manipulation
# "tidyr", for data tidying
# "readr", for data import
# "purrr", for functional programming
# "tibble", for tibbles, a modern re-imagining of data frames
# "stringr", for strings
# "forcats", for factors.

# to display numbers to 2 decimal places
options(scipen=999)

#### LOAD DATA

#use read_excel package to read raw data
md <- read_excel(here::here("raw_data", "RohDaten Kids.xls"), sheet = "Daten")

#### PREPROCESSING DATA #### 

#choosing only relevant variables specifying the select function in dplyr
mv <-data.frame( dplyr::select(md, 
                               Ingroup, 
                               Prosocial, 
                               efficiency, 
                               Share, 
                               age, 
                               ageclass, 
                               gender))

#creating ParticipantID variable
mv<- rowid_to_column(mv, "ParticipantID")

#update Main Variables (mv) data frame with participantID variable specifying the select function in dplyr
mv <-data.frame(dplyr::select(mv, 
                              ParticipantID, 
                              Ingroup, 
                              Prosocial, 
                              efficiency, 
                              Share, 
                              age, 
                              ageclass, 
                              gender)) 

#observing cohort breakdown
mv %>%
  dplyr::count(Ingroup, ageclass) %>%
  dplyr::group_by(Ingroup, ageclass)

#rename variables
#have named them "num" as these will be dummy number variables 
mv <- dplyr::rename(mv, 
                    GroupStat_Num = Ingroup, 
                    Envy_Num = efficiency, 
                    Age = age, 
                    Prosocial_Num = Prosocial, 
                    Share_Num = Share, 
                    AgeClass_Num = ageclass, 
                    Gender_Num = gender)

#reclassification of variables
mv <- mv %>% 
  #use tidyverse to class variables correctly
  
  #participantID classed as numeric as it is a number
  mutate(ParticipantID = as.numeric(ParticipantID),
         
         #GroupStat_Num classed as factor for 2 conditions (1) In-Group and (0) Out-Group
         GroupStat_Num = as.factor(GroupStat_Num),
         
         #Prosocial_Num classed as factor as it is a categorical variable
         Prosocial_Num = as.factor(Prosocial_Num),
         
         #Envy_Num classed as factor as it is a categorical variable
         Envy_Num = as.factor(Envy_Num),
         
         #Share_Num classed as factor as it is a categorical variable
         Share_Num = as.factor(Share_Num),
         
         #AgeClass_Num classed as ordered as there is an ascending categorical order for (0) 3-4 yos, (1) 5-6 yos,(3) 7-8 yos
         AgeClass_Num = as.ordered(AgeClass_Num),
         
         #Gender is coded as a factor variable as (0) Female and (1) Male
         Gender_Num = as.factor(Gender_Num),
         
         #Age coded as numeric variable
         Age = as.numeric(Age))

#observing incorrectly coded Envy Game Outcome coding in accordance with reported results
mv %>%
  dplyr::count(Envy_Num) %>%
  dplyr::group_by (Envy_Num)

#recode envy game to correct result
#use recode factor function as (0) is Non-Egalitarian, (1) is Egalitarian
mv$Envy_Num <- recode_factor(mv$Envy_Num, '1'="0", '0'="1")

#pivot data to create gametype variable
mv <- mv %>% 
  
  #indicate that this action is going to create more rows of data
  pivot_longer(
    
    # gathers values from the columns Prosocial_Num, Envy_Num and Share_Num 
    cols = c('Prosocial_Num', 'Envy_Num', 'Share_Num'),
    
    #creates new column GameType to indicate which GameType the GameOutcome_Num value refers
    names_to = "GameType", 
    
    # creates new column GameOutcome_Num
    values_to = "GameOutcome_Num"
  )

#use write xlsx package to export clean data file with correctly named and organised variables
write_xlsx(mv, here::here("processed_data", "fehrdata_processed_for_reanalysis.xlsx"))