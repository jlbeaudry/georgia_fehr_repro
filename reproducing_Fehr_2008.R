# Georgia Clift (2022)
# Code to Reproduce Fehr et al. (2008)'s analyses

#### LIBRARY ####

library("here") # to use with Rproject
library("readxl") #to read raw data
library("broom") #Convert statistical analysis objects from R into tidy format
library("tidyverse") #loads multiple tidyverse packages (see explanation below)
library("jtools") #adds APA style to ggplot figures
library("writexl") #to write excel file with output values
library("mfx") #to run probit regressions and find marginal effects

# use of tidyverse packages
# "ggplot2", for data visualisation
# "dplyr", for data manipulation
# "tidyr", for data tidying
# "readr", for data import
# "purrr", for functional programming
# "tibble", for tibbles, a modern re-imagining of data frames
# "stringr", for strings
# "forcats", for factors.

#to display numbers to 2 decimal places
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

#### WRANGLING DATA ####
#filtering per game type

#use tidyverse to create data subset with only Prosocial GameType
pv<- filter(mv, GameType == "Prosocial_Num")

#use tidyverse to create data subset with only Envy GameType
ev<- filter(mv, GameType == "Envy_Num")

#use tidyverse to create data subset with only Sharing GameType
sv<- filter(mv, GameType == "Share_Num")

#use tidyverse to create data subset with only participants in the Out-Group Condition Only (OGO = Out-Group Only)
ogo<- filter(mv, GroupStat_Num == "0")

#use tidyverse to create data subset with only participants in the In-Group Condition Only (IGO = In-Group Only)
igo<- filter(mv, GroupStat_Num == "1")

#use tidyverse to create data subset with only Sharing Game Outcomes the In-Group Condition Only (IGO = In-Group Only Sharing)
igos<- filter(igo, GameType == "Share_Num")

#use tidyverse to create data subset with only Sharing Game Outcomes the Out-Group Condition Only (IGO = Out-Group Only Sharing)
ogos<- filter(ogo, GameType == "Share_Num")


#### TEXT VARIABLES ####

#creating GroupStat text variable for Main Variables "MV" data
#copying data from GroupStat_Num variable
mv$GroupStat <- factor(mv$GroupStat_Num, 
                       
                       #specifying values in GroupStat_Num variable to be copied
                       levels = c("0","1"),
                       
                       #specifying "OutGroup" as label for "0"
                       labels = c("OutGroup",
                                  
                                  #specifying "InGroup" as label for "1"
                                  "InGroup"))

#creating GameType as a number variable for Main Variables "MV" data
#copying data from GameType variable
mv$GameType_Num <- factor(mv$GameType, 
                          
                          #specifying values in "GameType" variable to be copied
                          levels = c("Prosocial_Num","CorEnvy_Num", "Share_Num"),
                          
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
                       labels = c("OutGroup",
                                  
                                  #specifying "InGroup" as label for "1"
                                  "InGroup"))

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
                       labels = c("OutGroup",
                                  
                                  #specifying "InGroup" as label for "1"
                                  "InGroup"))

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
                       labels = c("OutGroup",
                                  
                                  #specifying "InGroup" as label for "1"
                                  "InGroup"))

#creating GameType as a Number variable for Sharing subset "SV" data
#copying data from GameType variable
sv$GameType_Num <- factor(sv$GameType, 
                          
                          #specifying values in "GameType" variable to be copied
                          levels = c("Prosocial_Num","CorEnvy_Num", "Share_Num"),
                          
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
                         levels = c("0","1"),
                         
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


#### BINOMIAL ANALYSES ####

#creating dataframe with text variables to be used in proportion observation
selectmv <-data.frame(dplyr::select (mv, ParticipantID, GroupStat, GameType, AgeClass, GameOutcome))

#create table with relevant variables
egalsum = table(selectmv$GameOutcome, selectmv$AgeClass, selectmv$GroupStat, selectmv$GameType)

#create data frame from table
egalsum = as.data.frame(egalsum)

#dictate column names
colnames(egalsum) = c("GameOutcome", "AgeClass", "GroupStat","GameType", "Freq")

#add frequencies to dataframe
egalpercent = as.data.frame(group_by(egalsum, AgeClass, GroupStat, GameType) %>%
                              
                              #create new variable for percentage
                              mutate(percent = Freq/sum(Freq)*100))

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
  mutate("NonEgalitarian" = as.numeric(NonEgalitarian),
         "Egalitarian" = as.numeric(Egalitarian))

#calculating and creating a variable for the total N across both GameOutcome values
egalprop$N <- egalprop$NonEgalitarian + egalprop$Egalitarian

#calculate binomial tests
egalbinomtable <- egalprop %>%
  
  #specifying to examine data in rows
  rowwise %>%
  
  #mutating data to perform binomial tests on proportion of children chosing egalitarian Game Outcome
  mutate(egalprop = list(broom::tidy(binom.test(Egalitarian, N, p=0.5, conf.level=0.95)))) %>%
  tidyr::unnest(egalprop)


#write csv file of this data table for use in the results
write.csv(egalbinomtable, here::here("results", "binomial_table_reproduction.csv"), row.names = FALSE)


#### FILTERING GRAPHS BY GAME TYPE ####

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



#### PREPARE GRAPH DATA SUBSETS ####

#followed from this youtube tutorial: https://www.youtube.com/watch?v=6MexZiX-2W8&t=268s&ab_channel=StatisticsofDOOM
#specify observation of egalitarian Game Outcome across all variables
fullgraphdata = subset(egalpercent, GameOutcome == "Egalitarian")

#specify observation of egalitarian Game Outcome in Prosocial game
pvgraphdata = subset(pvpercent, GameOutcome == "Egalitarian")

#specify observation of egalitarian Game Outcome in Envy game
evgraphdata = subset(evpercent, GameOutcome == "Egalitarian")

#specify observation of egalitarian Game Outcome in Sharing game
svgraphdata = subset(svpercent, GameOutcome == "Egalitarian")



#### REPLICATION PROPORTION GRAPHS ####

#Create Prosocial game proportion Graph
ppgraph <- ggplot(pvgraphdata,
                  
                  #Define AgeClass as x axis variable, Proportion as y axis variable
                  aes(x=AgeClass, y=percent, 
                      
                      #Define groups as Group Status values
                      group=GroupStat)) +
  
  #Line type will be defined by Group Status value
  geom_line(aes(linetype=GroupStat),size = 1) + 
  
  #shape of graph will be defined by Group Status value
  geom_point(aes(shape=GroupStat), size = 2) +
  
  #Specify axis labels
  labs(x="Age Group", y="Proportion of Egalitarian Choice", 
       
       #Specify graph title
       title="Reproduced Proportion of Egalitarian Choice in Prosocial Mini-Game") + 
  
  #set scale limits for y axis variable
  scale_y_continuous(name = "Proportion of Egalitarian Choice", limits = c(0, 100))

#place legend at top of graph
theme(legend.position="top")

#use jtools package to apply APA theme to graph
ppgraph + theme_apa()

#save graph as a .png file  
ggsave(here::here("images", "ppgraph_reproduction.png"), width = 4, height = 4, scale = 1, limitsize = FALSE)

#Create Envy game proportion Graph
epgraph <- ggplot(evgraphdata,
                  
                  #Define AgeClass as x axis variable, Proportion as y axis variable
                  aes(x=AgeClass, y=percent, 
                      
                      #Define groups as Group Status values
                      group=GroupStat)) +
  
  #Line type will be defined by Group Status value
  geom_line(aes(linetype=GroupStat),size = 1) + 
  
  #shape of graph will be defined by Group Status value
  geom_point(aes(shape=GroupStat), size = 2) +
  
  #Specify axis labels
  labs(x="Age Group", y="Proportion of Egalitarian Choice", 
       
       #Specify graph title
       title="Reproduced Proportion of Egalitarian Choice in Envy Mini-Game") + 
  
  #set scale limits for y axis variable
  scale_y_continuous(name = "Proportion of Egalitarian Choice", limits = c(0, 100))

#place legend at top of graph
theme(legend.position="top")

#use jtools package to apply APA theme to graph
epgraph + theme_apa()

#save graph as a .png file  
ggsave(here::here("images", "epgraph_reproduction.png"), width = 4, height = 4, scale = 1, limitsize = FALSE)


#Create Sharing game proportion Graph
spgraph <- ggplot(svgraphdata,
                  
                  #Define AgeClass as x axis variable, Proportion as y axis variable
                  aes(x=AgeClass, y=percent, 
                      
                      #Define groups as Group Status values
                      group=GroupStat)) +
  
  #Line type will be defined by Group Status value
  geom_line(aes(linetype=GroupStat),size = 1) + 
  
  #shape of graph will be defined by Group Status value
  geom_point(aes(shape=GroupStat), size = 2) +
  
  #Specify axis labels
  labs(x="Age Group", y="Proportion of Egalitarian Choice", 
       
       #Specify graph title
       title="Reproduced Proportion of Egalitarian Choice in Sharing Mini-Game") + 
  
  #set scale limits for y axis variable
  scale_y_continuous(name = "Proportion of Egalitarian Choice", limits = c(0, 100))

#place legend at top of graph
theme(legend.position="top")

#use jtools package to apply APA theme to graph
spgraph + theme_apa()

#save graph as a .png file  
ggsave(here::here("images", "spgraph_reproduction.png"), width = 4, height = 4, scale = 1, limitsize = FALSE)

#### PROBIT REGRESSION ####

# Generalised linear model build with Game Outcome (GameOutcome_Num) as Outcome Variable
# Group Status (GroupStat_Num) and Age Group (AgeClass_Num) as predictors. 
# Prosocial game data specified in "data pv"
probit_prococial <- glm(GameOutcome_Num ~ GroupStat_Num + AgeClass_Num, family = binomial(link = "probit"), 
                        data = pv)

#summary for probit_prosocial model
summary(probit_prococial)

#use mfx package to observe marginal effects for probit_prosocial model
probitmfx(probit_prococial, pv, atmean = TRUE, robust = FALSE, clustervar1 = NULL,
          clustervar2 = NULL, start = NULL, control = list())

# Generalised linear model build with Game Outcome (GameOutcome_Num) as Outcome Variable
# Group Status (GroupStat_Num) and Age Group (AgeClass_Num) as predictors. 
# Sharing game in-group only data specified in "igos"
probit_share_ingroup <- glm(GameOutcome_Num ~ AgeClass_Num, family = binomial(link = "probit"), 
                            data = igos)

#summary for probit_share_ingroup model
summary(probit_share_ingroup)

#use mfx package to observe marginal effects for probit_share_ingroup model
probitmfx(probit_share_ingroup, igos, atmean = TRUE, robust = FALSE, clustervar1 = NULL,
          clustervar2 = NULL, start = NULL, control = list())

# Generalised linear model build with Game Outcome (GameOutcome_Num) as Outcome Variable
# Group Status (GroupStat_Num) and Age Group (AgeClass_Num) as predictors
# Sharing game out-group only data specified in "ogos"
probit_share_outgroup <- glm(GameOutcome_Num ~ AgeClass_Num, family = binomial(link = "probit"), 
                             data = ogos)

#summary for probit_share_outgroup model
summary(probit_share_outgroup)

#use mfx package to observe marginal effects for probit_share_outgroup model
probitmfx(probit_share_outgroup, ogos, atmean = TRUE, robust = FALSE, clustervar1 = NULL,
          clustervar2 = NULL, start = NULL, control = list())

#set in-group as reference variable
#creating in-group reference variable to imitate fehr analaysis
sv$InGreference_Num <- factor(sv$GroupStat_Num, levels = c("1","0"))

# Generalised linear model build with Game Outcome (GameOutcome_Num) as Outcome Variable
# Group Status (with in-group as reference group as InGreference) and Age Group (AgeClass_Num) as predictors
# Sharing game data specified in "sv"
probit_share_full <- glm(GameOutcome_Num ~ InGreference_Num * AgeClass_Num, family = binomial(link = "probit"), 
                         data = sv)

#summary for probit_share_outgroup model
summary(probit_share_full)

#use mfx package to observe marginal effects for probit_share_outgroup model
probitmfx(probit_share_full, sv, atmean = TRUE, robust = FALSE, clustervar1 = NULL,
          clustervar2 = NULL, start = NULL, control = list())