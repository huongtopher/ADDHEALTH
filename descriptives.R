library(rio)
library(tidyverse)


wave1 <- import('data/wave1.rda')
wave5 <- import('data/wave5.rda')




dat1 <- wave1 %>%
  select(AID, H1GI1Y, IYEAR,
         H1GI8, #race,
         
         
         
         H1FS1, H1FS2, H1FS3, H1FS4, H1FS5, H1FS6,
         H1FS7, H1FS8, H1FS9, H1FS10, H1FS11, H1FS12,
         H1FS13, H1FS14, H1FS15, H1FS16, H1FS17, 
         H1FS18, H1FS19)













