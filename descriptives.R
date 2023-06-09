library(rio)
library(tidyverse)


wave1 <- import('data/wave1.rda')
wave11 <- import('data/wave1_1.rda')
wave5 <- import('data/wave5.rda')




dat <- wave1 %>%
  select(AID, H1GI1Y, IYEAR,
        
         
         
         
         H1FS1, H1FS2, H1FS3, H1FS4, H1FS5, H1FS6,
         H1FS7, H1FS8, H1FS9, H1FS10, H1FS11, H1FS12,
         H1FS13, H1FS14, H1FS15, H1FS16, H1FS17, 
         H1FS18, H1FS19)

dat5 <- wave5 %>%
  select(AID, H5ID6G, H5ID6GA)

#clean up age data to compute age, then remove B_YR and I_YR
dat <- dat %>%
  mutate(H1GI1Y = case_when(H1GI1Y == "(74) (74) 1974" ~ 1974,
                            H1GI1Y == "(75) (75) 1975" ~ 1975,
                            H1GI1Y == "(76) (76) 1976" ~ 1976,
                            H1GI1Y == "(77) (77) 1977" ~ 1977,
                            H1GI1Y == "(78) (78) 1978" ~ 1978,
                            H1GI1Y == "(79) (79) 1979" ~ 1979,
                            H1GI1Y == "(80) (80) 1980" ~ 1980,
                            H1GI1Y == "(81) (81) 1981" ~ 1981,
                            H1GI1Y == "(82) (82) 1982" ~ 1982,
                            H1GI1Y == "(83) (83) 1983" ~ 1983))


dat <- dat %>%
  mutate(IYEAR = case_when(IYEAR == "(94) (94) 1994" ~ 1994,
                           IYEAR == "(95) (95) 1995" ~ 1995))

dat <- dat %>%
  mutate(AGE = IYEAR - H1GI1Y, .after = IYEAR)

dat <- dat %>%
  select(-c(H1GI1Y, IYEAR))




dat <- inner_join(dat, wave11, by = "AID")
dat <- inner_join(dat, dat5, by = "AID")



#   
# a <- c("ID", "AGE", 
#                    "D1", "D2", "D3", "D4", "D5",
#                    "D6", "D7", "D8", "D9", "D10", 
#                    "D11", "D12", "D13", "D14", "D15",
#                    "D16", "D17", "D18", "D19",
#                    "MATCH", "MOVER", 
#                    "URBAN", "RACE", "RACE0", "HISP",
#                    "SEXCOMP", "MED_AGE", "DISP1", "MARITAL", "DISP2",
#                    "UNDER5", "MIGRATE", "DISP3", "HOUSEHOLD", "DISP4",
#                    "MEDHHINCOME", "DISP5", "MEDFAMILYINCOME", "DISP6",
#                    "PROPPOV",
#                    "HHEDU", "DISP7", "HHFEMLABOR", "HHUNEMPLOY", "HHOCCUP",
#                    "DISP8", "HHTENURE", "PROPOCCHOUSE", "MEDIANVAL", "DISP9",
#                    
#                    
#                    
#                    "DEPR", "AGEDEPR")




descript <- dat %>%
  select(AGE, BST90P02, BST90P05, BST90P06, BST90P08,
         BST90P10, BST90P13, BST90P15, BST90P19,
         BST90P20, BST90P23, BST90P24)

colnames(descript) <- c("age", "race", "hh_sex", "hh_median_age",
                        "hh_maritalstatus", "childrn_under_5",
                        "hh_type", "med_hh_income", "prop_below_pov",
                        "hh_edu", "unemployment", "hh_occupation")
  

library(table1)


































