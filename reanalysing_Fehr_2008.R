# Georgia Clift (2022)
# Code to Reanalyse Fehr et al. (2008)'s data

#### LIBRARY ####

library("here") # to use with Rproject
library("readxl") # to read raw data
library("broom") # Convert statistical analysis objects from R into tidy format
library("tidyverse") # loads multiple tidyverse packages (see explanation below)
library("jtools") # adds APA style to ggplot figures
library("writexl") # to write excel file with output values
library("mfx") # to run probit regressions and find marginal effects
library("lme4") # for fitting linear and generalized linear mixed-effects models
library("blorr") # for developing binary logistic regression models
library("sjPlot") # data visualization for statistics in social science
library("emmeans") # for estimated marginal means
library("plyr") # for splitting, applying and combining large problems into simpler problems

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

#### LOAD PREPROCESSED DATA

#use read_excel package to read preprocessed data file
mv <- read_excel(here::here("processed_data", "fehrdata_processed_for_reanalysis.xlsx"))

#### CLASSIFYING VARIABLES

#reclassification of variables
mv <- mv %>% 
  #use tidyverse to class variables correctly
  
  #participantID classed as numeric as it is a number
  mutate(ParticipantID = as.numeric(ParticipantID),
         
         #GroupStat_Num classed as factor for 2 conditions (1) In-Group and (0) Out-Group
         GroupStat_Num = as.factor(GroupStat_Num),
         
         #Prosocial_Num classed as factor as it is a categorical variable
         GameOutcome_Num = as.factor(GameOutcome_Num),
         
         #Envy_Num classed as factor as it is a categorical variable
         GameType_Num = as.factor(GameOutcome_Num),
         
         #AgeClass_Num classed as ordered as there is an ascending categorical order for (0) 3-4 yos, (1) 5-6 yos,(3) 7-8 yos
         AgeClass_Num = as.ordered(AgeClass_Num),
         
         #Gender is coded as a factor variable as (0) Female and (1) Male
         Gender_Num = as.factor(Gender_Num),
         
         #Age coded as numeric variable
         Age = as.numeric(Age))

#### FILTERING THE DATA ####

#filtering per game type

#use tidyverse to create data subset with only Prosocial GameType
pv<- filter(mv, GameType == "Prosocial_Num")

#use tidyverse to create data subset with only Envy GameType
ev<- filter(mv, GameType == "Envy_Num")

#use tidyverse to create data subset with only Sharing GameType
sv<- filter(mv, GameType == "Share_Num")

#### CREATE TEXT VARIABLES ####

#creating GroupStat text variable for Main Variables "MV" data
#copying data from GroupStat_Num variable
mv$GroupStat <- factor(mv$GroupStat_Num, 
                       
                       #specifying values in GroupStat_Num variable to be copied
                       levels = c("0","1"),
                       
                       #specifying "OutGroup" as label for "0"
                       labels = c("Out-Group",
                                  
                                  #specifying "InGroup" as label for "1"
                                  "In-Group"))

#creating GameType as a number variable for Main Variables "MV" data
#copying data from GameType variable
mv$GameType_Num <- factor(mv$GameType, 
                          
                          #specifying values in "GameType" variable to be copied
                          levels = c("Prosocial_Num","Envy_Num", "Share_Num"),
                          
                          #specifying "0" as numerical label for "Prosocial_Num"
                          labels = c("0", 
                                     
                                     #specifying "1" as numerical label for "CorEnvy_Num"
                                     "1",
                                     
                                     #specifying "2" as numerical label for "Share_Num"
                                     "2"))

#creating GameOutcome text variable for Main Variables "MV" data
#copying data from GameOutcome_Num variable 
mv$GameOutcome <- factor(mv$GameOutcome_Num,
                         
                         #specifying values in "GameOutcome_Num" variable to be copied
                         levels = c("0","1"),
                         
                         #specifying "NonEgalitarian" as text label for "0"
                         labels = c("NonEgalitarian", 
                                    
                                    #specifying "Egalitarian" as text label for "1"
                                    "Egalitarian"))

#creating AgeClass text variable
#copying data from AgeClass_Num variable 
mv$AgeClass <- factor(mv$AgeClass_Num,
                      
                      #specifying values in "AgeClass_Num" variable to be copied
                      levels = c("0","1","2"),
                      
                      #specifying "3-4yos" as text label for "0"
                      labels = c("3-4yos",
                                 
                                 #specifying "5-6yos" as text label for "1"
                                 "5-6yos", 
                                 
                                 #specifying "7-8yos" as text label for "2"
                                 "7-8yos"))

#creating GroupStat text variable for Prosocial subset "PV" data
#copying data from GroupStat_Num variable
pv$GroupStat <- factor(pv$GroupStat_Num, 
                       
                       #specifying values in GroupStat_Num variable to be copied
                       levels = c("0","1"),
                       
                       #specifying "OutGroup" as label for "0"
                       labels = c("Out-Group",
                                  
                                  #specifying "InGroup" as label for "1"
                                  "In-Group"))

#creating GameType as a Number variable for Prosocial subset "PV" data
#copying data from GameType variable
pv$GameType_Num <- factor(pv$GameType, 
                          
                          #specifying values in "GameType" variable to be copied
                          levels = c("Prosocial_Num","CorEnvy_Num", "Share_Num"),
                          
                          #specifying "0" as numerical label for "Prosocial_Num"
                          labels = c("0", 
                                     
                                     #specifying "1" as numerical label for "CorEnvy_Num"
                                     "1",
                                     
                                     #specifying "2" as numerical label for "Share_Num"
                                     "2"))

#creating GameOutcome text variable for Prosocial subset "PV" data
#copying data from GameOutcome_Num variable
pv$GameOutcome <- factor(pv$GameOutcome_Num,
                         
                         #specifying values in "GameOutcome_Num" variable to be copied
                         levels = c("0","1"),
                         
                         #specifying "NonEgalitarian" as text label for "0"
                         labels = c("NonEgalitarian", 
                                    
                                    #specifying "Egalitarian" as text label for "1"
                                    "Egalitarian"))

#creating AgeClass text variable for Prosocial subset "PV" data
#copying data from AgeClass_Num variable
pv$AgeClass <- factor(pv$AgeClass_Num,
                      
                      #specifying values in "AgeClass_Num" variable to be copied
                      levels = c("0","1","2"),
                      
                      #specifying "3-4yos" as text label for "0"
                      labels = c("3-4yos",
                                 
                                 #specifying "5-6yos" as text label for "1"
                                 "5-6yos", 
                                 
                                 #specifying "7-8yos" as text label for "2"
                                 "7-8yos"))

#creating GroupStat text variable for Envy subset "EV" data
#copying data from GroupStat_Num variable
ev$GroupStat <- factor(ev$GroupStat_Num, 
                       
                       #specifying values in GroupStat_Num variable to be copied
                       levels = c("0","1"),
                       
                       #specifying "OutGroup" as label for "0"
                       labels = c("Out-Group",
                                  
                                  #specifying "InGroup" as label for "1"
                                  "In-Group"))

#creating GameType as a Number variable for Envy subset "EV" data
#copying data from GameType variable
ev$GameType_Num <- factor(ev$GameType, 
                          
                          #specifying values in "GameType" variable to be copied
                          levels = c("Prosocial_Num","CorEnvy_Num", "Share_Num"),
                          
                          #specifying "0" as numerical label for "Prosocial_Num"
                          labels = c("0", 
                                     
                                     #specifying "1" as numerical label for "CorEnvy_Num"
                                     "1",
                                     
                                     #specifying "2" as numerical label for "Share_Num"
                                     "2"))

#creating GameOutcome text variable for Envy subset "EV" data
#copying data from GameOutcome_Num variable
ev$GameOutcome <- factor(ev$GameOutcome_Num,
                         
                         #specifying values in "GameOutcome_Num" variable to be copied
                         levels = c("0","1"),
                         
                         #specifying "NonEgalitarian" as text label for "0"
                         labels = c("NonEgalitarian", 
                                    
                                    #specifying "Egalitarian" as text label for "1"
                                    "Egalitarian"))

#creating AgeClass text variable for Envy subset "EV" data
#copying data from AgeClass_Num variable
ev$AgeClass <- factor(ev$AgeClass_Num,
                      
                      #specifying values in "AgeClass_Num" variable to be copied
                      levels = c("0","1","2"),
                      
                      #specifying "3-4yos" as text label for "0"
                      labels = c("3-4yos",
                                 
                                 #specifying "5-6yos" as text label for "1"
                                 "5-6yos", 
                                 
                                 #specifying "7-8yos" as text label for "2"
                                 "7-8yos"))

#creating GroupStat text variable for Sharing subset "SV" data
#copying data from GroupStat_Num variable
sv$GroupStat <- factor(sv$GroupStat_Num, 
                       
                       #specifying values in GroupStat_Num variable to be copied
                       levels = c("0","1"),
                       
                       #specifying "OutGroup" as label for "0"
                       labels = c("Out-Group",
                                  
                                  #specifying "InGroup" as label for "1"
                                  "In-Group"))

#creating GameType as a Number variable for Sharing subset "SV" data
#copying data from GameType variable
sv$GameType_Num <- factor(sv$GameType, 
                          
                          #specifying values in "GameType" variable to be copied
                          levels = c("Prosocial_Num",
                                     "CorEnvy_Num", 
                                     "Share_Num"),
                          
                          #specifying "0" as numerical label for "Prosocial_Num"
                          labels = c("0", 
                                     
                                     #specifying "1" as numerical label for "CorEnvy_Num"
                                     "1",
                                     
                                     #specifying "2" as numerical label for "Share_Num"
                                     "2"))

#creating GameOutcome text variable for Sharing subset "SV" data
#copying data from GameOutcome_Num variable
sv$GameOutcome <- factor(sv$GameOutcome_Num,
                         
                         #specifying values in "GameOutcome_Num" variable to be copied
                         levels = c("0",
                                    "1"),
                         
                         #specifying "NonEgalitarian" as text label for "0"
                         labels = c("NonEgalitarian", 
                                    
                                    #specifying "Egalitarian" as text label for "1"
                                    "Egalitarian"))

#creating AgeClass text variable for Sharing subset "SV" data
#copying data from AgeClass_Num variable
sv$AgeClass <- factor(pv$AgeClass_Num,
                      
                      #specifying values in "AgeClass_Num" variable to be copied
                      levels = c("0","1","2"),
                      
                      #specifying "3-4yos" as text label for "0"
                      labels = c("3-4yos",
                                 
                                 #specifying "5-6yos" as text label for "1"
                                 "5-6yos", 
                                 
                                 #specifying "7-8yos" as text label for "2"
                                 "7-8yos"))

### PROPORTION ANALYSES ####

#creating dataframe with text variables to be used in proportion observation
selectmv <-
  data.frame(dplyr::select (mv, 
                            ParticipantID, 
                            GroupStat, 
                            GameType, 
                            AgeClass, 
                            GameOutcome))

#create table with relevant variables
egalsum = table(selectmv$GameOutcome, 
                selectmv$AgeClass, 
                selectmv$GroupStat, 
                selectmv$GameType)

#create data frame from table
egalsum = as.data.frame(egalsum)

#dictate column names
colnames(egalsum) = c("GameOutcome", 
                      "AgeClass", 
                      "GroupStat",
                      "GameType", 
                      "Freq")

#add frequencies to dataframe
egalpercent = as.data.frame(
  group_by(
    egalsum, 
    AgeClass, 
    GroupStat, 
    GameType) %>%
    
    #create new variable for percentage
    mutate(
      percent = Freq/sum(
        Freq)*100))

#calculate CIs
#create new variable with sample size
egalpercent$sample = as.data.frame(table(selectmv$AgeClass, selectmv$GroupStat, selectmv$GameType))$Freq

#create new variable for standard error
egalpercent$SE = sqrt(egalpercent$percent*(100-egalpercent$percent)/egalpercent$sample)

#creating proportions table object
egalprop <- egalsum

#pivoting GameOutcome to observe specific Egalitarian proportion
egalprop <- egalprop %>% pivot_wider(names_from = GameOutcome, values_from = Freq)

#reclassify pivoted GameOutcome as numeric variables
egalprop <- egalprop %>%
  mutate("NonEgalitarian" = as.numeric(
    NonEgalitarian),
    "Egalitarian" = as.numeric(
      Egalitarian))

#calculating and creating a variable for the total N across both GameOutcome values
egalprop$N <- egalprop$NonEgalitarian + egalprop$Egalitarian

#calculate binomial tests
egalbinomtable <- egalprop %>%
  
  #specifying to examine data in rows
  rowwise %>%
  
  #mutating data to perform binomial tests on proportion of children chosing egalitarian Game Outcome
  dplyr::mutate(egalprop = list(broom::tidy(
    binom.test(
      Egalitarian, 
      N, 
      p=0.5, 
      conf.level=0.95)))) %>%
  tidyr::unnest(egalprop)

#write csv file of this data table for use in the results
write.csv(egalbinomtable, here::here("results", "binomial_table_reanalysed.csv"), row.names = FALSE)

### GRAPHS ####

#filter by game type
#create envy game proportion table
evsum = table(ev$GameOutcome, ev$AgeClass, ev$GroupStat, ev$GameType)

#turn envy game proportion table into data frame
evsum = as.data.frame(evsum)

#dictate column names
colnames(evsum) = c("GameOutcome", "AgeClass", "GroupStat","GameType", "Freq")

#add frequencies to dataframe
evpercent = as.data.frame(group_by(evsum, AgeClass, GroupStat, GameType) %>%
                            
                            #create new variable for percentage
                            mutate(percent = Freq/sum(Freq)*100))

#create prosocial game proportion table
pvsum = table(pv$GameOutcome, pv$AgeClass, pv$GroupStat, pv$GameType)

#turn prosocial game proportion table into data frame
pvsum = as.data.frame(pvsum)

#dictate column names
colnames(pvsum) = c("GameOutcome", "AgeClass", "GroupStat","GameType", "Freq")

#add frequencies to dataframe
pvpercent = as.data.frame(group_by(pvsum, AgeClass, GroupStat, GameType) %>%
                            
                            #create new variable for percentage
                            mutate(percent = Freq/sum(Freq)*100))

#create sharing game proportion table
svsum = table(sv$GameOutcome, sv$AgeClass, sv$GroupStat, sv$GameType)

#turn sharing game proportion table into data frame
svsum = as.data.frame(svsum)

#dictate column names
colnames(svsum) = c("GameOutcome", "AgeClass", "GroupStat","GameType", "Freq")

#add frequencies to dataframe
svpercent = as.data.frame(group_by(svsum, AgeClass, GroupStat, GameType) %>%
                            
                            #create new variable for percentage
                            mutate(percent = Freq/sum(Freq)*100))

#followed from this youtube tutorial: https://www.youtube.com/watch?v=6MexZiX-2W8&t=268s&ab_channel=StatisticsofDOOM

#specify observation of egalitarian Game Outcome across all variables
fullgraphdata = subset(egalpercent, GameOutcome == "Egalitarian")

#specify observation of egalitarian Game Outcome in Prosocial game
pvgraphdata = subset(pvpercent, GameOutcome == "Egalitarian")

#specify observation of egalitarian Game Outcome in Envy game
evgraphdata = subset(evpercent, GameOutcome == "Egalitarian")

#specify observation of egalitarian Game Outcome in Sharing game
svgraphdata = subset(svpercent, GameOutcome == "Egalitarian")

#### MAKING SELECT VARIABLE SETS ####

#create numeric main variable sets
mv_num <-
  
  #select variables to add to data frame
  data.frame(dplyr::select 
             (mv,ParticipantID, 
               Gender_Num, 
               GroupStat_Num, 
               GameType_Num, 
               AgeClass_Num, 
               GameOutcome_Num, 
               Age))

#create numeric variable set filtered by the Prosocial game
pv_num <-
  
  #select variables to add to data frame
  data.frame(dplyr::select (pv,
                            ParticipantID, 
                            Gender_Num,
                            GroupStat_Num, 
                            GameType_Num, 
                            AgeClass_Num, 
                            Age))

#create numeric variable set filtered by the Envy game
ev_num <-
  
  #select variables to add to data frame
  data.frame(dplyr::select (ev,
                            ParticipantID, 
                            Gender_Num, 
                            GroupStat_Num, 
                            GameType_Num, 
                            AgeClass_Num, 
                            Age))

#create numeric variable set filtered by the Sharing game
sv_num <-
  
  #select variables to add to data frame
  data.frame(dplyr::select (sv,
                            ParticipantID, 
                            Gender_Num, 
                            GroupStat_Num, 
                            GameType_Num, 
                            AgeClass_Num, 
                            Age))

#### MIXED EFFECTS MODELS ####

##building mixed effect models
#build null glm model with no fixed effects or random effects
mm1 <- glm(
  
  #specifying Game Outcome as the outcome variable
  GameOutcome_Num ~ 1,
  
  #specify binary data
  family = binomial(
    
    #specify use of logistic regression logit link
    link="logit"),
  
  #specify use of 'mv_num' data
  data = mv_num)

#view summary for null model
summary(mm1)

#Use 'glmer` to build model with Participant as random intercept
mm2 <- glmer(
  
  #specifying Game Outcome as the outcome variable
  GameOutcome_Num ~ 1 + 
    
    #specifying Participant as the random effect
    (1 | ParticipantID), 
  
  #specify use of 'mv_num' data
  data = mv_num, 
  
  #specify model details
  family = binomial(
    
    #specify use of logistic regression logit link function
    link="logit"),
  
  #specify control parameters
  control = glmerControl(
    
    #specify bobyqa optimizer
    optimizer = "bobyqa"),
  
  #use default NAGQ setting
  nAGQ = 1)

#view summary of mixed model with random intercept
summary(mm2)

## add Groupstat

#build glmer model
mm3 <- glmer(
  
  #specifying Game Outcome as the outcome variable
  GameOutcome_Num ~ 
    
    #adding Group Status as fixed effect
    GroupStat_Num + 
    
    #specifying Participant as the random effect
    (GroupStat_Num | ParticipantID),
  
  #specify use of 'mv_num' data
  data = mv_num, 
  
  #specify model details
  family = binomial(
    
    #specify use of logistic regression logit link function
    link="logit"),
  
  #specify control parameters
  control = glmerControl(
    
    #specify bobyqa optimizer
    optimizer = "bobyqa"),
  
  #use default NAGQ setting
  nAGQ = 1)

#view summary of mm3 model
summary(mm3)

## Add age as Fixed Effect
#build glmer model
mm4 <- glmer(
  
  #specifying Game Outcome as the outcome variable
  GameOutcome_Num ~ 
    
    #adding Age Group as fixed effect
    AgeClass_Num + 
    
    #specifying Participant as the random effect
    (AgeClass_Num | ParticipantID),
  
  #specify use of 'mv_num' data
  data = mv_num, 
  
  #specify model details
  family = binomial(
    
    #specify use of logistic regression logit link function
    link="logit"),
  
  #specify control parameters
  control = glmerControl(
    
    #specify bobyqa optimizer
    optimizer = "bobyqa"),
  
  #use default NAGQ setting
  nAGQ = 1)

#view summary of mm4 model
summary(mm4)

## Add Interaction of GroupStat and AgeClass
#build glmer model
mm5 <- glmer(
  
  #specifying Game Outcome as the outcome variable
  GameOutcome_Num ~ 
    
    #adding Group Status as fixed effect
    GroupStat_Num 
  
  #in addition to an interaction effect with... (see next commentary line)
  * 
    
    #Age Group, which also examined as fixed effect
    AgeClass_Num + 
    
    #specifying Participant as the random effect
    (GroupStat_Num | ParticipantID),
  
  #specify use of 'mv_num' data
  data = mv_num, 
  
  #specify model details
  family = binomial(
    
    #specify use of logistic regression logit link function
    link="logit"),
  
  #specify control parameters
  control = glmerControl(
    
    #specify bobyqa optimizer
    optimizer = "bobyqa"),
  
  #use default NAGQ setting
  nAGQ = 1)

#view summary for 'mm5'
summary(mm5)

# full fixed effects model
#build glmer model
mm6 <- glmer(
  
  #specifying Game Outcome as the outcome variable
  GameOutcome_Num ~ 
    
    #specifying Game Outcome as the outcome variable
    GroupStat_Num + 
    
    #adding Age Group as fixed effect
    AgeClass_Num + 
    
    #adding Game Type as fixed effect
    GameType_Num + 
    
    #specifying Participant as the random effect
    (GameType_Num | ParticipantID), 
  
  #specify use of 'mv_num' data
  data = mv_num, 
  
  #specify model details
  family = binomial(
    
    #specify use of logistic regression logit link function
    link="logit"),
  
  #specify control parameters
  control = glmerControl(
    
    #specify bobyqa optimizer
    optimizer = "bobyqa"),
  
  #use default NAGQ setting
  nAGQ = 1)

#view summary of 'mm6'
summary(mm6)

# COMPLETE MIXED EFFECTS INTERACTIONS MODEL
#build glmer model
mm7 <- glmer(
  
  #specifying Game Outcome as the outcome variable
  GameOutcome_Num ~ 
    
    #adding Group Status as a fixed effect
    GroupStat_Num 
  
  #in addition to an interaction effect with... (see next commentary line)
  * 
    
    #adding Age Group as fixed effect
    AgeClass_Num 
  
  #in addition to an interaction effect with... (see next commentary line)
  * 
    
    #adding Game Type as fixed effect
    GameType_Num 
  
  #in addition to an interaction effect with... (see next commentary line)
  * 
    
    #specifying Participant as the random effect
    (GameType_Num | ParticipantID), 
  
  #specify use of 'mv_num' data
  data = mv_num, 
  
  #specify model details
  family = binomial(
    
    #specify use of logistic regression logit link function
    link="logit"),
  
  #specify control parameters
  control = glmerControl(
    
    #specify bobyqa optimizer
    optimizer = "bobyqa"),
  
  #use default NAGQ setting
  nAGQ = 1)

#view summary of 'mm6'
#observe singular fit
#proceed with generalised linear models
summary(mm7)

#### GENERALIZED LINEAR MODELS ####

#build null glm model with no fixed effects
glm1 <- glm(
  
  #specifying Game Outcome as the outcome variable
  GameOutcome_Num ~ 1,
  
  #specify use of 'mv_num' data
  data = mv_num,
  
  #specify link function
  family = binomial(
    
    #use logit link function
    link="logit"),
  
  #specify action for missing values
  na.action(
    
    #exclude missing values
    na.omit))

#view summary for null model
summary(glm1)

## add GroupStat
#build glm model
glm2 <- glm(
  
  #specifying Game Outcome as the outcome variable
  GameOutcome_Num ~ 
    
    #adding Group Status as a fixed effect
    GroupStat_Num,
  
  #specify use of 'mv_num' data
  data = mv_num,
  
  #specify link function
  family = binomial(
    
    #use logit link function
    link="logit"),
  
  #specify action for missing values
  na.action(
    
    #exclude missing values
    na.omit))

#view summary for 'glm2' model
summary(glm2)

# Add Age as Fixed Effect
#build glm model
glm3 <- glm(
  
  #specifying Game Outcome as the outcome variable
  GameOutcome_Num ~ 
    
    #adding Age Group as a fixed effect
    AgeClass_Num,
  
  #specify use of 'mv_num' data
  data = mv_num,
  
  #specify link function
  family = binomial(
    
    #use logit link function
    link="logit"),
  
  #specify action for missing values
  na.action(
    
    #exclude missing values
    na.omit))

#view summary for 'glm3' model
summary(glm3)

# Add Interaction of GroupStat and AgeClass
#build glm model
glm4 <- glm(
  
  #specifying Game Outcome as the outcome variable
  GameOutcome_Num ~ 
    
    #adding Group Status as a fixed effect
    GroupStat_Num 
  
  #in addition to an interaction effect with... (see next commentary line)
  * 
    
    #Age Group, also examine as fixed effect
    AgeClass_Num, 
  
  #specify use of 'mv_num' data
  data = mv_num,
  
  #specify link function
  family = binomial(
    
    #use logit link function
    link="logit"),
  
  #specify action for missing values
  na.action(
    
    #exclude missing values
    na.omit))

#view summary for 'glm4' model
summary(glm4)

# Full fixed effects model
glm5 <- glm(GameOutcome_Num ~ GroupStat_Num + AgeClass_Num + GameType_Num,
            data=, mv_num,
            family = binomial(link="logit"),
            na.action(na.omit))
#build glm model
glm5 <- glm(
  
  #specifying Game Outcome as the outcome variable
  GameOutcome_Num ~ 
    
    #adding Group Status as a fixed effect
    GroupStat_Num +
    
    #adding Age Group as fixed effect
    AgeClass_Num +
    
    #adding Game Type as fixed effect
    GameType_Num,
  
  #specify use of 'mv_num' data
  data = mv_num,
  
  #specify link function
  family = binomial(
    
    #use logit link function
    link="logit"),
  
  #specify action for missing values
  na.action(
    
    #exclude missing values
    na.omit))

#view summary for 'glm5' model
summary(glm5)

## COMPLETE GLM6 EFFECTS INTERACTIONS MODEL
#build glm model
glm6 <- glm(
  
  #specifying Game Outcome as the outcome variable
  GameOutcome_Num ~ 
    
    #adding Group Status as a fixed effect
    GroupStat_Num 
  
  #in addition to an interaction effect with... (see next commentary line)
  * 
    
    #adding Age Group as fixed effect
    AgeClass_Num 
  
  #in addition to an interaction effect with... (see next commentary line)
  * 
    
    #adding Game Type as fixed effect
    GameType_Num,
  
  #specify use of 'mv_num' data
  data = mv_num,
  
  #specify link function
  family = binomial(
    
    #use logit link function
    link="logit"),
  
  #specify action for missing values
  na.action(
    
    #exclude missing values
    na.omit))

#view summary for 'glm6' model
summary(glm6)

## Model Comparisons
#compare model fits and statistics
#create dataframe
modelanova <- as_tibble(
  
  #with an analasis of variance
  anova(
    
    #using 'glm1', 'glm2', 'glm3', 'glm4, 'glm5', 'glm6' models
    glm1, 
    glm2, 
    glm3, 
    glm4, 
    glm5, 
    glm6, 
    
    #specifying usage of Chi-Square test
    test="Chisq"))

#write csv file of 'modelanova' table
write.csv(modelanova, here::here("results", "model_anova_reanalysis.csv"), row.names = FALSE)

#create data frame
modelstats <- as.list(blr_multi_model_fit_stats(
  
  #using 'glm1', 'glm2', 'glm3', 'glm4, 'glm5', 'glm6' models
  glm1, 
  glm2, 
  glm3, 
  glm4, 
  glm5, 
  glm6))

#write csv file of 'modelstats' table
write.csv(modelanova, here::here("results", "model_stats_reanalysis.csv"), row.names = FALSE)

#use SjPlot Package to create model summary table
tab_model(
  
  #using 'glm1', 'glm2', 'glm3', 'glm4, 'glm5', 'glm6' models
  glm1, 
  glm2, 
  glm3, 
  glm4, 
  glm5, 
  glm6)

#### PAIRWISE COMPARISONS ####

#observe pairwise comparisons
#create object
estmeanstable <- 
  
  #calculating the estimated marginal means
  emmeans(
    
    #using 'glm6'
    glm6, 
    
    #with pairwise comparisons
    pairwise ~ 
      
      #grouped by Group Status, Age Group and Game Type
      GroupStat_Num + AgeClass_Num + GameType_Num)

#create data frame
contraststable <- as.data.frame(
  
  #with the contrasts from 'estmeanstable'
  estmeanstable$contrasts)

#create data drame
estmeansdf <- as.data.frame(
  
  #with the estimated marginal means from 'estmeanstable'
  estmeanstable$emmeans)

#write excel file of 'estmeansdf' table
write_xlsx(estmeansdf, here::here("results", "estimated_means_table_reanalysis.xlsx"))


#write excel file of 'contraststable' table
write_xlsx(contraststable, here::here("results", "contrasts_table_reanalysis.xlsx"))

#### BUILD SEPARATE MODELS FOR MINI GAMES ####

### PROSOCIAL ###

#build Prosocial mini-game only model
mp1 <- glm(
  
  #specifying Game Outcome as the outcome variable
  GameOutcome_Num ~ 
    
    #adding Group Status as a fixed effect
    GroupStat_Num 
  
  #in addition to an interaction effect with... (see next commentary line)
  * 
    
    #adding Age in Years as a fixed effect
    Age,
  
  #specify use of 'pv_num' data
  data = pv,
  
  #specify link function
  family = binomial(
    
    #use logit link function
    link="logit"),
  
  #specify action for missing values
  na.action(
    
    #exclude missing values
    na.omit))

#view summary for 'mp1' model
summary(mp1)

## Probability Graph ##
#tutorial used: https://www.youtube.com/watch?v=dmiB-qgOink&ab_channel=QuantitativeSocialScienceDataAnalysis

#create data object for Prosocial Plot with predictor variables
pvplotdata <- 
  
  #using the 'pv' data subset
  with(pv, 
       
       #to create a data frame
       data.frame(
         
         #and specifies 'Age in Years' as a predictor
         Age = rep(seq(from = 0, to = 10,
                       length.out=100),2),
         
         #and specifies 'Group Status' as a predictor
         GroupStat_Num = factor(rep(0:1, each = 100))))

#create object for Prosocial Plot
pvplotdata2 <-
  
  #combining into one object
  cbind(
    
    #specifying use of 'pvpplotdata'
    pvplotdata, 
    
    #set up predicted values
    predict(
      
      #using 'mp1' model
      mp1, 
      
      #and 'pvplotdata' predictors
      pvplotdata, 
      
      #specifying prediction type
      type="link", 
      
      #indicate standard errors
      se=TRUE))

#mutate variables to class correctly
pvplotdata2 <- pvplotdata2 %>%
  mutate(
    
    #Age in Years classed as numeric as it is a numeric variable
    Age = as.numeric(Age),
    
    #Group Status classed as factor as it is a categorical variable
    GroupStat_Num = as.factor(GroupStat_Num),
    
    #Fit classed as numeric as it is a numeric variable
    fit = as.numeric(fit),
    
    #Standard Error Fit classed as numeric as it is a numeric variable
    se.fit = as.numeric(se.fit),
    
    #Residual classed as numeric as it is a numeric variable
    residual.scale = as.numeric(residual.scale))

#save new object 
pvplotdata2 <-
  
  #new data frame that contains a new column with the results of...
  within(pvplotdata2,
         
         #a new saved object...
         {pvprob <- 
           
           #the probability distribution of 'mp1'
           plogis(fit)})

#extract Group factor from 'pvplotdata'
pvplotdata2$Group <- 
  
  #as factors
  as.factor(
    
    #with values mapped from GroupStat_Num
    mapvalues(pvplotdata2$GroupStat_Num, 
              
              #'0' and '1'
              from=c("0","1"),
              
              #Give Out-Group and In-Group Labels
              to = c("Out-Group", "In-Group")))

#create table with Group factor from 'pvplotdata'
table(pvplotdata2$Group)

#Create Prosocial game Probability Plot
pprobgraph <- ggplot(data=pvplotdata2,
                     
                     #Define Age  in Years as x axis variable, Probability as y axis variable
                     aes(x=Age, y=pvprob)) + 
  
  #Line type will be defined by Group Status value
  geom_line(mapping=aes(linetype=Group), size=1) +
  
  #place legend at top of graph
  theme(legend.position="top") +
  
  #Specify axis labels
  labs(x="Age", y="Probablity of Egalitarian Choice", 
       
       #Specify graph title
       title="Probability of Egalitarian Choice in Prosocial Mini-Game") + 
  
  #set scale limits for y axis variable
  scale_y_continuous(name = "Probablity of Egalitarian Choice", limits = c(0, 1))

#use jtools package to apply APA theme to graph
pprobgraph + theme_apa()

#save graph as a .png file  
ggsave(here::here("images", "pprobgraph_reanalysis.png"), width = 4, height = 4, scale = 1, limitsize = FALSE)

### ENVY GAME ###

#build Envy mini-game only model
me1 <- glm(
  
  #specifying Game Outcome as the outcome variable
  GameOutcome_Num ~ 
    
    #adding Group Status as a fixed effect
    GroupStat_Num 
  
  #in addition to an interaction effect with... (see next commentary line)
  * 
    
    #adding Age in Years as a fixed effect
    Age,
  
  #specify use of 'ev' data
  data = ev,
  
  #specify link function
  family = binomial(
    
    #use logit link function
    link="logit"),
  
  #specify action for missing values
  na.action(
    
    #exclude missing values
    na.omit))

#view summary for 'me1' model
summary(me1)

#tutorial https://www.youtube.com/watch?v=dmiB-qgOink&ab_channel=QuantitativeSocialScienceDataAnalysis
#create data object for Envy Plot with predictor variables
evplotdata <- 
  
  #using the 'ev' data subset
  with(ev, 
       
       #to create a data frame
       data.frame(
         
         #and specifies 'Age in Years' as a predictor
         Age = rep(seq(from = 0, to = 10,
                       length.out=100),2),
         
         #and specifies 'Group Status' as a predictor
         GroupStat_Num = factor(rep(0:1, each = 100))))

#create object for Envy Plot
evplotdata2 <-
  
  #combining into one object
  cbind(
    
    #specifying use of 'evpplotdata'
    evplotdata, 
    
    #set up predicted values
    predict(
      
      #using 'me1' model
      me1, 
      
      #and 'evplotdata' predictors
      evplotdata, 
      
      #specifying prediction type
      type="link", 
      
      #indicate standard errors
      se=TRUE))

#mutate variables to class correctly
evplotdata2 <- evplotdata2 %>%
  mutate(
    
    #Age in Years classed as numeric as it is a numeric variable
    Age = as.numeric(Age),
    
    #Group Status classed as factor as it is a categorical variable
    GroupStat_Num = as.factor(GroupStat_Num),
    
    #Fit classed as numeric as it is a numeric variable
    fit = as.numeric(fit),
    
    #Standard Error Fit classed as numeric as it is a numeric variable
    se.fit = as.numeric(se.fit),
    
    #Residual classed as numeric as it is a numeric variable
    residual.scale = as.numeric(residual.scale))

#save new object 
evplotdata2 <-
  
  #new data frame that contains a new column with the results of...
  within(evplotdata2,
         
         #a new saved object...
         {evprob <- 
           
           #the probability distribution of 'me1'
           plogis(fit)})

#extract Group factor from 'evplotdata'
evplotdata2$Group <- 
  
  #as factors
  as.factor(
    
    #with values mapped from GroupStat_Num
    mapvalues(evplotdata2$GroupStat_Num, 
              
              #'0' and '1'
              from=c("0","1"),
              
              #Give Out-Group and In-Group Labels
              to = c("Out-Group", "In-Group")))

#create table with Group factor fron 'evplotdata'
table(evplotdata2$Group)

#Create Envy game Probability Plot
eprobgraph <- ggplot(data=evplotdata2,
                     
                     #Define Age  in Years as x axis variable, Probability as y axis variable
                     aes(x=Age, y=evprob)) + 
  
  #Line type will be defined by Group Status value
  geom_line(mapping=aes(linetype=Group), size=1) +
  
  #place legend at top of graph
  theme(legend.position="top") +
  
  #Specify axis labels
  labs(x="Age", y="Probablity of Egalitarian Choice", 
       
       #Specify graph title
       title="Probability of Egalitarian Choice in Envy Mini-Game") + 
  
  #set scale limits for y axis variable
  scale_y_continuous(name = "Probablity of Egalitarian Choice", limits = c(0, 1))

#use jtools package to apply APA theme to graph
eprobgraph + theme_apa()

#save graph as a .png file  
ggsave(here::here("images", "eprobgraph_reanalysis.png"), width = 4, height = 4, scale = 1, limitsize = FALSE)

### SHARING GAME ###
#build Sharing mini-game only model
ms1 <- glm(
  
  #specifying Game Outcome as the outcome variable
  GameOutcome_Num ~ 
    
    #adding Group Status as a fixed effect
    GroupStat_Num 
  
  #in addition to an interaction effect with... (see next commentary line)
  * 
    
    #adding Age in Years as a fixed effect
    Age,
  
  #specify use of 'sv' data
  data = sv,
  
  #specify link function
  family = binomial(
    
    #use logit link function
    link="logit"),
  
  #specify action for missing values
  na.action(
    
    #exclude missing values
    na.omit))

#view model summary for 'ms1'
summary(ms1)

#tutorial https://www.youtube.com/watch?v=dmiB-qgOink&ab_channel=QuantitativeSocialScienceDataAnalysis
#create data object for Sharing Plot with predictor variables
svplotdata <- 
  
  #using the 'sv' data subset
  with(sv, 
       
       #to create a data frame
       data.frame(
         
         #and specifies 'Age in Years' as a predictor
         Age = rep(seq(from = 0, to = 10,
                       length.out=100),2),
         
         #and specifies 'Group Status' as a predictor
         GroupStat_Num = factor(rep(0:1, each = 100))))

#create object for Sharing Plot
svplotdata2 <-
  
  #combining into one object
  cbind(
    
    #specifying use of 'svpplotdata'
    svplotdata, 
    
    #set up predicted values
    predict(
      
      #using 'ms1' model
      ms1, 
      
      #and 'svplotdata' predictors
      svplotdata, 
      
      #specifying prediction type
      type="link", 
      
      #indicate standard errors
      se=TRUE))

#mutate variables to class correctly
svplotdata2 <- svplotdata2 %>%
  mutate(
    
    #Age in Years classed as numeric as it is a numeric variable
    Age = as.numeric(Age),
    
    #Group Status classed as factor as it is a categorical variable
    GroupStat_Num = as.factor(GroupStat_Num),
    
    #Fit classed as numeric as it is a numeric variable
    fit = as.numeric(fit),
    
    #Standard Error Fit classed as numeric as it is a numeric variable
    se.fit = as.numeric(se.fit),
    
    #Residual classed as numeric as it is a numeric variable
    residual.scale = as.numeric(residual.scale))

#save new object 
svplotdata2 <-
  
  #new data frame that contains a new column with the results of...
  within(svplotdata2,
         
         #a new saved object...
         {svprob <- 
           
           #the probability distribution of 'ms1'
           plogis(fit)})

#extract Group factor from 'svplotdata'
svplotdata2$Group <- 
  
  #as factors
  as.factor(
    
    #with values mapped from GroupStat_Num
    mapvalues(svplotdata2$GroupStat_Num, 
              
              #'0' and '1'
              from=c("0","1"),
              
              #Give Out-Group and In-Group Labels
              to = c("Out-Group", "In-Group")))

#create table with Group factor fron 'svplotdata'
table(svplotdata2$Group)

#Create Sharing game Probability Plot
sprobgraph <- ggplot(data=svplotdata2,
                     
                     #Define Age in Years as x axis variable, Probability as y axis variable
                     aes(x=Age, y=svprob)) + 
  
  #Line type will be defined by Group Status value
  geom_line(mapping=aes(linetype=Group), size=1) +
  
  #place legend at top of graph
  theme(legend.position="top") +
  
  #Specify axis labels
  labs(x="Age", y="Probablity of Egalitarian Choice", 
       
       #Specify graph title
       title="Probability of Egalitarian Choice in Sharing Mini-Game") + 
  
  #set scale limits for y axis variable
  scale_y_continuous(name = "Probablity of Egalitarian Choice", limits = c(0, 1))

#use jtools package to apply APA theme to graph
sprobgraph + theme_apa()

#save graph as a .png file  
ggsave(here::here("images", "sprobgraph_reanalysis.png"), width = 4, height = 4, scale = 1, limitsize = FALSE)

#### MODEL TABLES BY GAME #### 
## SHARING GAME ##

#use SjPlot Package to create model summary table
tab_model(
  
  #using 'mp1', 'me1', 'ms1' models
  mp1, me1, ms1)

#### PAIRWISE DIFFERENCES IN LINEAR TREND FOR SHARING GAME ####

#observe pairwise comparisons
#create object
sharingemtrends <- 
  
  #calculating the estimated marginal means of linear trends
  emtrends(
    
    #using 'ms1'
    ms1, 
    
    #with pairwise comparisons
    pairwise ~ 
      
      #grouped by Group Status
      GroupStat_Num,
    
    #of liniear Age in Years trend
    var = "Age")

#create data frame
sharingcontraststable <- as.data.frame(
  
  #with the contrasts from 'sharingcontraststable'
  sharingemtrends$contrasts)

#write csv file of 'sharingcontraststable' table
write_xlsx(sharingcontraststable, 
           here::here("results", "sharing_contrasts_table_reanalysis.xlsx"))



