# Some Instructions ---------------------------------------------------------------------- 

# Some are recaptured in more than 1 year
# Whats relevant is how much they gain in a particular year
# Gain in mass over time for birds that have been recaptured
# Work in groups
# individually hand in assignment on ACORN
# RMD file with graph and code-- echo the code

#---- Dealing with Location and Mass-------------------------------------------------------------------------------------------- ------
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
      # all the ones recaptured DO have an R in the recapture column

# SEPARATE THE DATA ACCORDING TO THE TWO LOCATIONS
table(recap$location) # good there are only two locations (thank you)
recap <- ungroup(recap)
    # select thoes on Seal Island
seal <- filter(recap, location == "ABO-SI")
# select thoes on BP Island
bp <- filter(recap, location == "ABO-BP")
    # so there are only 38 observations, when we told it to filter those 
    # that match seal island and had "R" under recapture there was 40! what the shit




    #---- FIND THE DIFFERENCE IN WEIGHT UPON RECAPTURE AT SEAL ISLAND-------------------------------------
    # im just dicken around here with no clue what I actually want
    # if(blpw.all$recap == "R" & blpw.all$year | blpw.all$month | blpw.all$day <), 
library(reshape2)
seal <- select (seal, location, band, mass, year, month, day)
    # Test <- melt(seal)
    # Test <- dcast(seal, year ~ band, value="band")
    # seal <-  seal %>% group_by(band, year, month, day)
    # seal <- ungroup(seal)
library(lubridate)
    # creates a date column so we can organize the captures by date
seal$date <- with(seal, ymd(sprintf('%04d%02d%02d', year, month, day))) 


# now that we seperated SEal Island from BP, there will be a few single-bird observations:
sealrecaptured <- anyDuplicated(dogfood$band, fromLast = TRUE)
sealrecap <-  seal %>% group_by(band, year) %>% filter(n()>1) 

#GROUP BY BAND NUMBER SO WE CAN FIND DIFFERENCE IN MASS UPON RECAPTURE
dogfood <- group_by(sealrecap, band, year)

#SELECTING BIRDS GROUPED BY BAND OF THE EARLIER DATE OF CAPTURE
topn <- top_n(x = dogfood, n = -1, wt = date)
dogfood <- ungroup(dogfood)
topn <- ungroup(topn)

# RENAMING NEW WEIGHTS COLUMN "mass" TO "firstmass"
colnames(topn)[3] <- "firstmass"

#JOINING THE TABLES TOGETEHER, WHILE DUPLICATING "firstmass"
brezhnev <- left_join(dogfood, topn, by = "band") # proposed changing brezhnev to antifreeze

brezhnev <- ungroup(brezhnev)
brezhnev <- group_by(brezhnev, band) %>%
  mutate(sass = mass-firstmass) %>%
  mutate(yday = (yday(date.x)))
  


# Try graphing this and see what kind of magic happens
ggplot(data = brezhnev,mapping = aes(x = yday, y = sass, colour = band), show.legend = FALSE) +
  xlab("Time of Year")+ylab("Mass (in grams, relative to capture date)") +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  theme_bw()


str(brezhnev)

    #---- FIND THE DIFFERENCE IN WEIGHT UPON RECAPTURE AT BON PORTAGE----------------------------
library(reshape2)
bp <- select (bp, location, band, mass, year, month, day)
# brezhnev <- melt(seal)
# brezhnev <- dcast(seal, year ~ band, value="band")
# seal <-  seal %>% group_by(band, year, month, day)
# seal <- ungroup(seal)
library(lubridate)
# creates a date column so we can organize the captures by date
bp$date <- with(bp, ymd(sprintf('%04d%02d%02d', year, month, day))) 

# now that we seperated SEal Island from BP, there will be a few single-bird observations:
bprecaptured <- anyDuplicated(grumpy_old_man$band, fromLast = TRUE)
bprecap <-  bp %>% group_by(band, year) %>% filter(n()>1)

#GROUP BY BAND NUMBER SO WE CAN FIND DIFFERENCE IN MASS UPON RECAPTURE
grumpy_old_man <- group_by(bprecap, band, year) #%>%
#arrange(desc(date))
# dont need the arrange really

# trying to add a conditional mutate so that we can 
# subtract the mass of early from later, beyond my abilities
# topline group 
tops <- top_n(x = grumpy_old_man, n = -1, wt = date)
grumpy_old_man <- ungroup(grumpy_old_man)
tops <- ungroup(tops)

# renamed column "mass" to "firstmass"
colnames(tops)[3] <- "firstmass"

# joins the tables together, with the creation of a new row "firstmass"
# so this one in paricular allows us to duplicate the "firstmass" so we can
# tell the mutate to subtract column "firstmass" from "mass"
Bertha <- left_join(grumpy_old_man, tops, by = "band")

Bertha <- ungroup(Bertha)
Bertha <- group_by(Bertha, band) %>%
  mutate(sass = mass-firstmass) %>%
  mutate(yday = (yday(date.x)))

    #---- Try graphing this and see what kind of magic happens-------------------------
ggplot(data = Bertha,mapping = aes(x = yday, y = sass, colour = band), show.legend = FALSE) +
  xlab("Time of Year")+ylab("Mass (in grams, relative to capture date)") +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  theme_bw()
# magical...

# Dealing with Dates------------------------------------------------------------------------------------------- ----

    #---- Read in Data and Packages -----
getwd()
setwd("/Users/joelgoodwin/Google Drive/School/Masters/Classes/Biol 5023- Research Methods II/VarroaETG_Percent/Biol5023/BlackpollAssignment")
given_data<- readRDS("blpw.all.RDS")
given_data <- blpw.all # (mike just used his imported csv)
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
  mutate(jday = (yday(date))) %>%
  filter(recap== "R")

#format things as a date
as.Date(banding_data$date)

    #---- Re-joining Islands ----
# now to combine BP with SEAL
ungroup(Bertha, brezhnev)
jointest <- full_join(Bertha, brezhnev)
#This works, now need to add the location (island) before this # done
jointest <- data.frame(jointest)

# ---- making a month-day column using yday ----
monoyear <- rep("2000",times = 259) # makes a column with the year 2000
jointest <- cbind(monoyear, jointest) # adds monoyear (2000) to jointest
str(jointest) #change year 2000 to numeric
jointest$monoyear <- as.numeric(as.character(jointest$monoyear)) 



jointest <- jointest %>%
  mutate(monodate = make_date(monoyear,month.x, day.x)) #%>%
  #mutate(monoyday = (yday(date.x))) %>% # don't need this and also not working, not sure why
  #filter(recap== "R")

as.Date(jointest$yday, origin = "2000-01-01")
# checking that the dates are actually the right months this time

# ---- Graphing the whole shibam ----
# removed "yday(date)" from aes (x = ..., replaced with monoyday
ggplot(data = jointest,mapping = aes(x = monodate, y = sass, colour = band), show.legend = FALSE) +
  xlab("Time of Year") + 
  ylab("Mass (in grams, relative to capture date)") +
  geom_point(group= jointest$band, show.legend = FALSE) +
  geom_line(group= jointest$band, show.legend = FALSE) +
  facet_wrap(~location.x) +
  #scale_x_date(labels = date_format("%m")) +
  #scale_x_date( date_breaks ="1 month", date_labels = "%B")+
  #scale_x_date(labels = date_format("%m"), breaks = date_breaks("1 month"))+
  #scale_x_date(labels = date_format("%m"), date_breaks='1 month') +
  theme_bw()

str(banding_data)




# ---- Junk/ In Progress - dont need any of this stuff, but good to show we have no clue what's actually going on ----

#Code from internet to remove year from date
banding_data$monthday <- format(as.POSIXct(banding_data$date), "%m/%d")
mutate(yday = yday(date)) 
mutate(yday(date)) %>%
mutate(month = month(banding_data$month, label = TRUE, abbr = TRUE)) %>%
format(strptime(banding_data$jday, format = "%j"), format= "%m-%d")


mutate(given_data, format(banding_data$date, format="%m-%d")) 
mutate(date = make_date(year,month, day))
as.Date(banding_data$jday, format= "%j", origin= "1999-07-08")

# Try and make a new column that has all the same year, then reformat dates, then use the above code

# playing with the yday
as.Date(215, origin = "1999-01-01")
as.Date(304, origin = "1999-12-26")

    #---- Add a new column to remove the effect of year on plotting ("Monoyear") ------------------
# makes a column with the year 2000
monoyear <- rep("2000",times = 295)
banding_data <- cbind(monoyear, banding_data)

banding_data <- banding_data %>%
  mutate(monodate = make_date(monoyear,month, day)) %>%
  mutate(monoyday = (yday(date))) %>%
  filter(recap== "R")
# so this produces a new column with te exactly same values as yday... great...
# yday = monoyday
# woohoo

#---- The Graphing Code We Want (Finally!) -------------------------------------------------------------------
ggplot(data = banding_data,mapping = aes(x = as.Date(banding_data$monoyday, origin = "2000-01-01"), y = mass, colour = band), show.legend = FALSE) +
xlab("Time of Year")+ylab("Mass (in grams, relative to capture date)") +
  geom_point(group= banding_data$band, show.legend = FALSE) +
  geom_line(group= banding_data$band, show.legend = FALSE) +
  facet_wrap(~location) +
  scale_x_date(labels = date_format("%b")) +
  #scale_x_date( date_breaks ="1 month", date_labels = "%B")+
  #scale_x_date(labels = date_format("%m"), breaks = date_breaks("1 month"))+
  #scale_x_date(labels = date_format("%m"), date_breaks='1 month') +
  theme_bw()

#it works (why it didn't with the original yday, I have no clue)

