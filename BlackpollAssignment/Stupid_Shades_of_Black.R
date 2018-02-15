library(tidyverse)

str(blpw.all$band)
blpw.all$band <- factor(blpw.all$band)
table(blpw.all$band)

# find thoes with band number recorded more than once 
recaptured <- anyDuplicated(blpw.all$band, fromLast = TRUE)
recap <-  blpw.all %>% group_by(band) %>% filter(n()>1) 
# no clue how it works, but all the ones recaptured DO have an 
# R in the recapture column, which is good to know... now

table(recap$location) # good there are only two locations (thank you)

# select thoes on Seal Island
seal <- filter(recap, location == "ABO-SI")
bp <- filter(recap, location == "ABO-BP")
# so there are only 38 observations, when we told it to filter thoes 
# that match seal island and had "R" under recapture there was 40! what the shit




# find difference in weight from first record
# im just dicken around here with no clue what I actually want
if(blpw.all$recap == "R" & blpw.all$year | blpw.all$month | blpw.all$day <), 




#---- Read in Data and Packages -----
getwd()
setwd("/Users/joelgoodwin/Google Drive/School/Masters/Classes/Biol 5023- Research Methods II/VarroaETG_Percent/Biol5023/BlackpollAssignment")
given_data<- readRDS("blpw.all.RDS")

library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)
#---- View the Data Frame -----
ls(banding_data)
summary(banding_data)

#---- Collapse Columns to Make the Date Data into a Date ---- 
banding_data <- given_data %>%
  mutate(date = make_date(year,month, day)) %>%
  filter(recap== "R")
# mutate(date = make_date(year,month, day))

# Some birds banded, not all
# Some are recaptured in more than 1 year
# Whats relevant is how much they gain in a particular year
# Gain in mass over time for birds that have been recaptured
# Work in groups
# individually hand in assignment on ACORN
# RMD file with graph and code-- echo the code



#---- Graph Code ----
ggplot(banding_data)+
  geom_point(aes(x= year,y= mass, color= band, group= band), show.legend = FALSE)+
  xlab("Time of Year")+ylab("Mass (in grams, relative to capture date)")+
  theme_bw(10)+
  facet_wrap(~location)








