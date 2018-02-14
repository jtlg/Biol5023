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








