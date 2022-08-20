# Georgia Clift (2022)
# Power Analyses to Reproduce Fehr et al. (2008) with online protocol

#### LIBRARY ####
#Basic Functions for Power Analysis
library("pwr")

#### POWER ANALYSES ####

#envy game out group condition difference between 3-4 year olds and 7-8 year olds
#perform proportion test

envy_out <- prop.test(
  
  #specifying the proportions to test
  x = c(
    
    #number of 3-4 year olds who selected Egalitarian Choice in out-group Envy game condition
    12, 
    
    #number of 7-8 year olds who selected Egalitarian Choice in out-group Envy game condition
    42), 
  
  #specifying N for both groups
  n = c(
    
    #total number of 3-4 year olds in the out-group Envy game condition
    27, 
    
    #total number of 7-8 year olds in the out-group Envy game condition
    49))

#perform a power analysis for a 2p proportion test
pwr.2p.test(
  
  #to the effect size of
  h = 
    
    #calculate effect size between
    ES.h(
      
      #proportion of 3-4 year olds who selected Egalitarian Choice in out-group Envy game condition
      p1 = 0.44, 
      
      #proportion of 7-8 year olds who selected Egalitarian Choice in out-group Envy game condition
      p2 = 0.86),
  
  #at an .05 significance level
  sig.level = 0.05,
  
  #at .90 power
  power = 0.90,
  
  #using a two-tailed test
  alternative = "two.sided")

#envy game in-group condition difference between 3-4 year olds and 7-8 year olds
#perform proportion test
prop.test(
  
  #specifying the proportions to test
  x = c(
    
    #number of 3-4 year olds who selected Egalitarian Choice in in-group Envy game condition
    10, 
    
    #number of 7-8 year olds who selected Egalitarian Choice in in-group Envy game condition
    45), 
  
  #specifying N for both groups
  n = c(
    
    #total number of 3-4 year olds in the out-group Envy game condition
    23, 
    
    #total number of 7-8 year olds in the out-group Envy game condition
    56))

#perform a power analysis for a 2p proportion test
pwr.2p.test(
  
  #to the effect size of
  h = 
    
    #calculate effect size between
    ES.h(
      
      #proportion of 3-4 year olds who selected Egalitarian Choice in in-group Envy game condition
      p1 = 0.43, 
      
      #proportion of 7-8 year olds who selected Egalitarian Choice in in-group Envy game condition
      p2 = 0.80),
  
  #at an .05 significance level
  sig.level = 0.05,
  
  #at .90 power
  power = 0.90,
  
  #using a two-tailed test
  alternative = "two.sided")

#sharing game in-group condition difference between 3-4 year olds and 7-8 year olds
#perform proportion test
prop.test(
  
  #specifying the proportions to test
  x = c(
    
    #number of 3-4 year olds who selected Egalitarian Choice in the in-group Sharing mini-game
    2, 
    
    #number of 7-8 year olds who selected Egalitarian Choice in the in-group Sharing mini-game
    25), 
  
  #specifying N for both groups
  n = c(
    
    #total number of 3-4 year olds in the out-group Sharing mini-game
    23, 
    
    #total number of 7-8 year olds in the out-group Sharing mini-game
    56))

#perform a power analysis for a 2p proportion test
pwr.2p.test(
  
  #to the effect size of
  h = 
    
    #calculate effect size between
    ES.h(
      
      #proportion of 3-4 year olds who selected Egalitarian Choice in in-group Sharing mini-game
      p1 = 0.09, 
      
      #proportion of 7-8 year olds who selected Egalitarian Choice in in-group Sharing mini-game
      p2 = 0.45),
  
  #at an .05 significance level
  sig.level = 0.05,
  
  #at .90 power
  power = 0.90,
  
  #using a two-tailed test
  alternative = "two.sided")

#sharing game difference in proportion for 7-8 year olds between the in-group and out-group conditions
#perform proportion test
prop.test(
  
  #specifying the proportions to test
  x = c(
    
    #number of In-Group 7-8 year olds who selected Egalitarian Choice in the Sharing mini-game
    25, 
    
    #number of Out-Group 7-8 year olds who selected Egalitarian Choice in the Sharing mini-game
    6), 
  
  #specifying N for both groups
  n = c(
    
    #total number of 7-8 year olds in the in-group Sharing mini-game
    56, 
    
    #total number of 7-8 year olds in the out-group Sharing mini-game
    49))

#perform a power analysis for a 2p proportion test
pwr.2p.test(
  
  #to the effect size of
  h = 
    
    #calculate effect size between
    ES.h(
      
      #proportion of in-Group 7-8 year olds who selected Egalitarian Choice in the Sharing mini-game
      p1 = 0.45, 
      
      #proportion of out-Group 7-8 year olds who selected Egalitarian Choice in the Sharing mini-gamee
      p2 = 0.12),
  
  #at an .05 significance level
  sig.level = 0.05,
  
  #at .90 power
  power = 0.90,
  
  #using a two-tailed test
  alternative = "two.sided")


