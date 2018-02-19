#------Some Instructions ---------------------------------------------------------------------- 
# Some are recaptured in more than 1 year
# Whats relevant is how much they gain in a particular year
# Gain in mass over time for birds that have been recaptured
# Work in groups
# individually hand in assignment on ACORN
# RMD file with graph and code-- echo the code

#------Dealing with Location and Mass-------------------------------------------------------------------------------------------- ------
#LOAD IN NECESSARY PACKAGES
library(tidyverse)
library(lubridate)
library(reshape2)

#VISUALIZE THE DATA FRAME
str(blpw.all$band)
blpw.all$band <- factor(blpw.all$band)
table(blpw.all$band)

# FIND THOSE BIRDS WITH BAND NUMBER RECORDED MORE THAN ONCE
recaptured <- anyDuplicated(blpw.all$band, fromLast = TRUE)
recap <-  blpw.all %>% group_by(band, location) %>% filter(n()>1) 
      # no clue how it works, but all the ones recaptured DO have an 
      # R in the recapture column, which is good to know... now

# SEPARATE THE DATA ACCORDING TO THE TWO LOCATIONS
table(recap$location) # good there are only two locations (thank you)
recap <- ungroup(recap)
    # select thoes on Seal Island
seal <- filter(recap, location == "ABO-SI")
bp <- filter(recap, location == "ABO-BP")
    # so there are only 38 observations, when we told it to filter those 
    # that match seal island and had "R" under recapture there was 40! what the shit




#----------------FIND THE DIFFERENCE IN WEIGHT UPON RECAPTURE AT SEAL ISLAND-------------------------------------
    # im just dicken around here with no clue what I actually want
    # if(blpw.all$recap == "R" & blpw.all$year | blpw.all$month | blpw.all$day <), 
library(reshape2)
seal <- select (seal, band, mass, year, month, day)
    # Test <- melt(seal)
    # Test <- dcast(seal, year ~ band, value="band")
    # seal <-  seal %>% group_by(band, year, month, day)
    # seal <- ungroup(seal)
library(lubridate)
    # creates a date column so we can organize the captures by date
seal$date <- with(seal, ymd(sprintf('%04d%02d%02d', year, month, day))) 

#GROUP BY BAND NUMBER SO WE CAN FIND DIFFERENCE IN MASS UPON RECAPTURE
testicle <- group_by(seal, band) #%>%
    #arrange(desc(date))
    # dont need the arrange really

    # trying to add a conditional mutate so that we can 
    # subtract the mass of early from later, beyond my abilities
# topline group 
topn <- top_n(x = testicle, n = -1, wt = date)
testicle <- ungroup(testicle)
topn <- ungroup(topn)

# renamed column "mass" to "firstmass"
colnames(topn)[2] <- "firstmass"

# joins the tables together, with the creation of a new row "firstmass"
# so this one in paricular allows us to duplicate the "firstmass" so we can
# tell the mutate to subtract column "firstmass" from "mass"
Test <- left_join(testicle, topn, by = "band")

Test <- ungroup(Test)
Test <- group_by(Test, band) %>%
  mutate(sass = mass-firstmass) %>%
  mutate(yday = (yday(date.y)))
  
# now, lets mutate this! #boob
# so it's mutated and the column called "sass" is ready to be plotted
# I need to do this for the BP island later (double check things are right, ect.)

# Try graphing this and see what kind of magic happens
ggplot(data = Test,mapping = aes(x = yday, y = sass, colour = band), show.legend = FALSE) +
  xlab("Time of Year")+ylab("Mass (in grams, relative to capture date)") +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  theme_bw()


str(Test)



#----------------FIND THE DIFFERENCE IN WEIGHT UPON RECAPTURE AT BON PORTAGE----------------------------
library(reshape2)
bp <- select (bp, band, mass, year, month, day)
# Test <- melt(seal)
# Test <- dcast(seal, year ~ band, value="band")
# seal <-  seal %>% group_by(band, year, month, day)
# seal <- ungroup(seal)
library(lubridate)
# creates a date column so we can organize the captures by date
bp$date <- with(bp, ymd(sprintf('%04d%02d%02d', year, month, day))) 

#GROUP BY BAND NUMBER SO WE CAN FIND DIFFERENCE IN MASS UPON RECAPTURE
grumpy_old_man <- group_by(bp, band) #%>%
#arrange(desc(date))
# dont need the arrange really

# trying to add a conditional mutate so that we can 
# subtract the mass of early from later, beyond my abilities
# topline group 
tops <- top_n(x = grumpy_old_man, n = -1, wt = date)
grumpy_old_man <- ungroup(grumpy_old_man)
tops <- ungroup(tops)

# renamed column "mass" to "firstmass"
colnames(tops)[2] <- "firstmass"

# joins the tables together, with the creation of a new row "firstmass"
# so this one in paricular allows us to duplicate the "firstmass" so we can
# tell the mutate to subtract column "firstmass" from "mass"
Bertha <- left_join(grumpy_old_man, tops, by = "band")

Bertha <- ungroup(Bertha)
Bertha <- group_by(Bertha, band) %>%
  mutate(sassy = mass-firstmass) 
  #mutate(yday = (yday(date.y)))

# now, lets mutate this! #boob
# so it's mutated and the column called "sass" is ready to be plotted
# I need to do this for the BP island later (double check things are right, ect.)


#----------------Try graphing this and see what kind of magic happens-------------------------
ggplot(data = Test,mapping = aes(x = yday, y = sass, colour = band), show.legend = FALSE) +
  xlab("Time of Year")+ylab("Mass (in grams, relative to capture date)") +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  theme_bw()
#-------Dealing with Dates------------------------------------------------------------------------------------------- ----

    #---- Read in Data and Packages -----
getwd()
setwd("/Users/joelgoodwin/Google Drive/School/Masters/Classes/Biol 5023- Research Methods II/VarroaETG_Percent/Biol5023/BlackpollAssignment")
given_data<- readRDS("blpw.all.RDS")

library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)
library(scales)

    #---- View the Data Frame -----
ls(banding_data)
summary(banding_data)
str(banding_data)
    #---- Collapse Columns to Make the Date Data into a Date ---- 

banding_data <- given_data %>%
  mutate(date = make_date(year,month, day)) %>%
  mutate(yday = (yday(date))) %>%
  filter(recap== "R")

format(as.Date(banding_data$date), format="%m-%d") 
as.Date(banding_data$date)
as.Date(banding_data$yday, origin = "%01")
#Code from internet to remove year from date
format(as.Date(banding_data$date), "%m/%d")
#mutate(yday = yday(date)) 
#mutate(yday(date)) %>%
#mutate(month = month(banding_data$month, label = TRUE, abbr = TRUE)) %>%
update.packages()
  ?as.Date

mutate(given_data, format(banding_data$date, format="%m-%d")) 
# mutate(date = make_date(year,month, day))

    #---- Graph Code ----
ggplot(data = banding_data,mapping = aes(x = yday(banding_data$date), y = mass, colour = band), show.legend = FALSE) +
  xlab("Time of Year")+ylab("Mass (in grams, relative to capture date)") +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~location) +
  #scale_x_date(breaks = date_breaks("months"), labels = date_format("%m")) +
  #scale_x_date(date_breaks ="1 month", date_labels = "%m")+
  #scale_x_date(labels = date_format("%m"), breaks = date_breaks("1 month"))+
  #scale_x_date(labels = date_format("%m"), date_breaks='1 month') +
  theme_bw()

  