library(rio)
library(tidyverse)


wave1 <- import('data/wave1.rda')
wave5 <- import('data/wave5.rda')

#select id, age, and depression symptom variables from wave1
dat1 <- wave1 %>%
  select(AID, H1GI1Y, IYEAR,
         H1FS1, H1FS2, H1FS3, H1FS4, H1FS5, H1FS6,
         H1FS7, H1FS8, H1FS9, H1FS10, H1FS11, H1FS12,
         H1FS13, H1FS14, H1FS15, H1FS16, H1FS17, 
         H1FS18, H1FS19)

#select depression diagnosis variables from wave5
dat5 <- wave5 %>%
  select(AID, H5ID6G, H5ID6GA)


#create new dataframe with observations that exist in both wave1 and wave5
dat <- inner_join(dat1, dat5, by = "AID")






colnames(dat) <- c("ID", "B_YR", "I_YR", 
                   "D1", "D2", "D3", "D4", "D5",
                   "D6", "D7", "D8", "D9", "D10", 
                   "D11", "D12", "D13", "D14", "D15",
                   "D16", "D17", "D18", "D19",
                   "DEPR", "AGEDEPR")

apply((dat[,-1]), 2, table)



#clean up age data to compute age, then remove B_YR and I_YR
dat <- dat %>%
  mutate(B_YR = case_when(B_YR == "(74) (74) 1974" ~ 1974,
                          B_YR == "(75) (75) 1975" ~ 1975,
                          B_YR == "(76) (76) 1976" ~ 1976,
                          B_YR == "(77) (77) 1977" ~ 1977,
                          B_YR == "(78) (78) 1978" ~ 1978,
                          B_YR == "(79) (79) 1979" ~ 1979,
                          B_YR == "(80) (80) 1980" ~ 1980,
                          B_YR == "(81) (81) 1981" ~ 1981,
                          B_YR == "(82) (82) 1982" ~ 1982,
                          B_YR == "(83) (83) 1983" ~ 1983))


dat <- dat %>%
  mutate(I_YR = case_when(I_YR == "(94) (94) 1994" ~ 1994,
                          I_YR == "(95) (95) 1995" ~ 1995))

dat <- dat %>%
  mutate(AGE = I_YR - B_YR, .after = I_YR)

dat <- dat %>%
  select(-c(B_YR, I_YR))



# clean up variables ------------------------------------------------------
#These questions will ask about how you feel emotionally and about how you feel in
#general. How often was each of the following things true during the past week?


####################    D1 - BOTHER #############
# You were bothered by things that usually don’t bother you
dat <- dat %>%
  mutate(D1 = case_when(D1 == "(0) (0) Never/rarely" ~ 0,
                        D1 == "(1) (1) Sometimes" ~ 1,
                        D1 == "(2) (2) A lot of the time" ~ 2,
                        D1 == "(3) (3) Most/all of the time" ~ 3))





#####################       D2 - LOW APPETITE   ###################
# You didn't feel like eating, your appetite was poor.

dat <- dat %>%
  mutate(D2 = case_when(D2 == "(0) (0) Never/rarely" ~ 0,
                        D2 == "(1) (1) Sometimes" ~ 1,
                        D2 == "(2) (2) A lot of the time" ~ 2,
                        D2 == "(3) (3) Most/all of the time" ~ 3))


#####################       D3 - BLUES   ###################
# You felt that you could not shake off the blues, even with help from
# your family and your friends.

dat <- dat %>%
  mutate(D3 = case_when(D3 == "(0) (0) Never/rarely" ~ 0,
                        D3 == "(1) (1) Sometimes" ~ 1,
                        D3 == "(2) (2) A lot of the time" ~ 2,
                        D3 == "(3) (3) Most/all of the time" ~ 3))



#####################       D4 - GOOD AS OTHERS   ###################
# You felt that you were just as good as other people.

dat <- dat %>%
  mutate(D4 = case_when(D4 == "(0) (0) Never/rarely" ~ 0,
                        D4 == "(1) (1) Sometimes" ~ 1,
                        D4 == "(2) (2) A lot of the time" ~ 2,
                        D4 == "(3) (3) Most/all of the time" ~ 3))



#####################       D5 - CONCENTRATE   ###################
# You had trouble keeping your mind on what you were doing.

dat <- dat %>%
  mutate(D5 = case_when(D5 == "(0) (0) Never/rarely" ~ 0,
                        D5 == "(1) (1) Sometimes" ~ 1,
                        D5 == "(2) (2) A lot of the time" ~ 2,
                        D5 == "(3) (3) Most/all of the time" ~ 3))


#####################       D6 - DEPRESSED   ###################
# You felt depressed

dat <- dat %>%
  mutate(D6 = case_when(D6 == "(0) (0) Never/rarely" ~ 0,
                        D6 == "(1) (1) Sometimes" ~ 1,
                        D6 == "(2) (2) A lot of the time" ~ 2,
                        D6 == "(3) (3) Most/all of the time" ~ 3))



###################        D7 - FATIGUE       ##################
# You felt that you were too tired to do things. 
dat <- dat %>%
  mutate(D7 = case_when(D7 == "(0) (0) Never/rarely" ~ 0,
                        D7 == "(1) (1) Sometimes" ~ 1,
                        D7 == "(2) (2) A lot of the time" ~ 2,
                        D7 == "(3) (3) Most/all of the time" ~ 3))




###################      D8 - HOPE       ##################
# You felt hopeful about the future.

dat <- dat %>%
  mutate(D8 = case_when(D8 == "(0) (0) Never/rarely" ~ 0,
                        D8 == "(1) (1) Sometimes" ~ 1,
                        D8 == "(2) (2) A lot of the time" ~ 2,
                        D8 == "(3) (3) Most/all of the time" ~ 3))



###################      D9 - FAILURE       ##################
# You thought your life had been a failure.

dat <- dat %>%
  mutate(D9 = case_when(D9 == "(0) (0) Never/rarely" ~ 0,
                        D9 == "(1) (1) Sometimes" ~ 1,
                        D9 == "(2) (2) A lot of the time" ~ 2,
                        D9 == "(3) (3) Most/all of the time" ~ 3))


###################      D10 - FEARFUL       ##################
# You felt fearful.

dat <- dat %>%
  mutate(D10 = case_when(D10 == "(0) (0) Never/rarely" ~ 0,
                         D10 == "(1) (1) Sometimes" ~ 1,
                         D10 == "(2) (2) A lot of the time" ~ 2,
                         D10 == "(3) (3) Most/all of the time" ~ 3))


###################      D11 - HAPPY       ##################
# You were happy

dat <- dat %>%
  mutate(D11 = case_when(D11 == "(0) (0) Never/rarely" ~ 0,
                         D11 == "(1) (1) Sometimes" ~ 1,
                         D11 == "(2) (2) A lot of the time" ~ 2,
                         D11 == "(3) (3) Most/all of the time" ~ 3))



###################      D12 - TALKED LESS       ##################
# You talked less than usual.

dat <- dat %>%
  mutate(D12 = case_when(D12 == "(0) (0) Never/rarely" ~ 0,
                         D12 == "(1) (1) Sometimes" ~ 1,
                         D12 == "(2) (2) A lot of the time" ~ 2,
                         D12 == "(3) (3) Most/all of the time" ~ 3))


###################      D13 - TALKED LESS       ##################
# You felt lonely

dat <- dat %>%
  mutate(D13 = case_when(D13 == "(0) (0) Never/rarely" ~ 0,
                         D13 == "(1) (1) Sometimes" ~ 1,
                         D13 == "(2) (2) A lot of the time" ~ 2,
                         D13 == "(3) (3) Most/all of the time" ~ 3))



###################      D14 - UNFRIENDLY      ##################
# People were unfriendly to you.

dat <- dat %>%
  mutate(D14 = case_when(D14 == "(0) (0) Never/rarely" ~ 0,
                         D14 == "(1) (1) Sometimes" ~ 1,
                         D14 == "(2) (2) A lot of the time" ~ 2,
                         D14 == "(3) (3) Most/all of the time" ~ 3))


###################      D15 - ENJOYED LIFE      ##################
# You enjoyed life. 

dat <- dat %>%
  mutate(D15 = case_when(D15 == "(0) (0) Never/rarely" ~ 0,
                         D15 == "(1) (1) Sometimes" ~ 1,
                         D15 == "(2) (2) A lot of the time" ~ 2,
                         D15 == "(3) (3) Most/all of the time" ~ 3))


###################      D16 - SAD        ##################
# You felt sad

dat <- dat %>%
  mutate(D16 = case_when(D16 == "(0) (0) Never/rarely" ~ 0,
                         D16 == "(1) (1) Sometimes" ~ 1,
                         D16 == "(2) (2) A lot of the time" ~ 2,
                         D16 == "(3) (3) Most/all of the time" ~ 3))


###################      D17 - DISLIKE        ##################
# You felt that people disliked you

dat <- dat %>%
  mutate(D17 = case_when(D17 == "(0) (0) Never/rarely" ~ 0,
                         D17 == "(1) (1) Sometimes" ~ 1,
                         D17 == "(2) (2) A lot of the time" ~ 2,
                         D17 == "(3) (3) Most/all of the time" ~ 3))




###################      D18 - GET STARTED        ##################
# It was hard to get started doing things.

dat <- dat %>%
  mutate(D18 = case_when(D18 == "(0) (0) Never/rarely" ~ 0,
                         D18 == "(1) (1) Sometimes" ~ 1,
                         D18 == "(2) (2) A lot of the time" ~ 2,
                         D18 == "(3) (3) Most/all of the time" ~ 3))


###################      D18 - SUICIDAL IDEATION       ##################
# You felt life was not worth living

dat <- dat %>%
  mutate(D19 = case_when(D19 == "(0) (0) Never/rarely" ~ 0,
                         D19 == "(1) (1) Sometimes" ~ 1,
                         D19 == "(2) (2) A lot of the time" ~ 2,
                         D19 == "(3) (3) Most/all of the time" ~ 3))




dat[, 23][dat[, 23] == 997] <- NA


#save(dat, file = "cleaned_data.RData")

load('cleaned_data.RData')

# keep adolescents only
dat <- dat %>%
  subset(AGE <= 17)

# remove those with adolescent depr diagnosis
dat <- dat[-which(dat$AGEDEPR<18),]


# our dataframe contains adolescent depression symptoms, and whether or not someone
# reported a diagnosis of adult depression


library(qgraph)          #visualize networks
library(mgm)           #mixed graphical modeling
library(bootnet)         # runs all basic network analyses
library(NetworkComparisonTest)  #compare networks
library(networktools)         #bridge symptoms, goldbricker for node select


#remove redundant nodes


nameslong <- c(
  "You were bothered by things that usually don’t bother you.",
  "You didn’t feel like eating, your appetite was poor.",
  "You felt that you could not shake off the blues, even with help from your family and your friends",
  "You felt that you were just as good as other people.",
  "You had trouble keeping your mind on what you were doing.",
  "You felt depressed.",
  "You felt that you were too tired to do things.",
  "You felt hopeful about the future.",
  "You thought your life had been a failure.",
  "You felt fearful.",
  "You were happy.",
  "You talked less than usual.",
  "You felt lonely.",
  "People were unfriendly to you.",
  "You enjoyed life.",
  "You felt sad.",
  "You felt that people disliked you.",
  "It was hard to get started doing things.",
  "You felt life was not worth living.")
  
  
  
  
  
  

cesd <- dat %>%   #dataframe with symptom items
  select(D1:D19)

gb <- goldbricker(cesd, p = 0.01, method = "hittner2003",
                 threshold = 0.25, corMin = 0.5, progressbar = T)
gb
# Suggested reductions: Less than 25 % of correlations are significantly different for the following pairs: 
#   [1] "No suggested reductions"


net1 <- estimateNetwork(cesd, default = "EBICglasso",
                        missing = "pairwise",
                        signed = T)


netplot1 <- plot(net1, layout = "spring", vsize = 5, 
                 border.color="black",
                 nodeNames = nameslong)


pdf("expectedinfluenceplot1.pdf", width = 4)
ef1 <- centralityPlot(netplot1, include = "ExpectedInfluence")
dev.off()

ef1.df <- cbind(ef1$data$node, ef1$data$value)














         
         