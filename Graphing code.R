# ---- Load Data and Packages ----
getwd()
library(lubridate)
library(knitr)
library(ggplot2)
library(dplyr)

data<- read.csv('DataForR.csv')
read.csv("DataForR.csv") 

# ---- Look At Data ----
str(data)
summary(data)
# produces a histogram of the percent reaction by Varroa mites
#easey way to see how data is distributed (we have three points in the 1000's)
# te maximum value when we do "summary" also is an indicator
hist(data$normalized)
# therefore we should create a subset of the data removing these

# ---- Change Data to Factors ----
#change integer to factor for trials
data$trial<-factor(data$trial)

#lists the levels of factors
levels(data$trial)

#change number to factor for concentration "conc"
data$conc<-factor(data$conc)
levels(data$conc)
#looking to see what the types of odour are
levels(data$odour)

# ---- filter data and pipeline for Methyl Palmitate and "&" concentration 0.01 ----
filter(data, odour=="Methyl Palmitate" & conc=="0.01") %>%
  summarise("Mean Methyl Palmitate Reaction at Concentration 0.01"=mean(percent))

filter(data, odour=="Methyl Palmitate" & conc=="0.01") %>%
  summarise("Standard Deviation Methyl Palmitate Reaction at Concentration 0.01"=sd(percent))

# ---- Subset Data ----
str(data)
plot(data$conc,data$percent,subset(data,conc =="0.01" & odour =="Methyl Palmitate" & gb =="Good"))

# ---- Filter and Plot Methyl Palmitate ----     
ggplot(data%>%filter(odour=="Methyl Palmitate")%>%filter(gb=="Good")%>%filter(percent>"1000"))+
  geom_point(aes(y=percent,x=conc), col="blue")+
  xlab("Conc")+ylab("Percent")+
  theme_bw(18)+
  facet_wrap(~odour,scales="free")

ggplot(data%>%filter(odour=="Methyl Palmitate")%>%filter(gb=="Good")%>%filter(percent>"1000"))+
  geom_boxplot(aes(y=percent,x=conc))+
  geom_jitter(aes(x=conc,y=percent, col=odour), width=0.25, shape=17, size =2)+
  xlab("Concentration")+ylab("Percent")+
  theme_bw(18)

# ---- subset data removing outliers in Percent of 1000 or over ----
data_subset<-subset(data, percent<1000)
hist(data_subset$percent)             
summary(data_subset)             
summary(data_subset$odour)

# ---- Steps ----
# 1. seperate data by odour
# 2. identify any odd stuff, should we leave it? aka perform statistical test
# 3. graph data "percent" to "concentration"  for each "odour" 
# in relation to 100% base line of hexane (have some kind of indicator on graph)

# ---- Subset data by odour ----
# is there a way to tell R- to perform action for every value of "odour"?

summary(data$odour)
remove(data_subset)

#subset of Methyl Palmitate
methyl_palmitate<-subset(data, odour == "Methyl Palmitate")
summary(methyl_palmitate)

# ---- 1-Hexadecanol subset and ggplot n=3 ----
#subset of 1-Hexadecanol
hexadecanol <- subset(data, odour == "1-Hexadecanol")
summary(hexadecanol)
hist(hexadecanol$percent)
hist(hexadecanol$normalized)


ggplot(hexadecanol)+
  geom_boxplot(aes(x=conc, y=normalized))+
  geom_point(aes(x=conc, y=normalized))+
  xlab("Concentration")+ ylab("Response Amplitude (%)")+ ggtitle("1-Hexadecanol (trials=3)")+
  theme_bw(10)

# ---- 2-Heptanol Subset and ggplot n=12 ----
#subset of 2-Heptanol
heptanol<-subset(data, odour == "2-Heptanol")
summary (heptanol)
hist(heptanol$normalized)

#ggplot attempt 2-Heptanol
ggplot(heptanol)+
  geom_boxplot(aes(x=conc,y=normalized))+
  geom_point(aes(x=conc,y=normalized))+
  xlab("Concentration")+ylab("Responce Amplitude (%)")+ggtitle("2-Heptanol (trials=12)")+
  theme_bw(10)

# ---- 2-Heptanone Subset and ggplot n=11 ----
heptanone<-subset(data, odour == "2-Heptanone")
summary(heptanone)

ggplot(heptanone)+
  geom_boxplot(aes(x=conc,y=normalized))+
  geom_point(aes(x=conc,y=normalized))+
  xlab("Concentration")+ylab("Responce Amplitude (%)")+ggtitle("2-Heptanone (trials=11)")+
  theme_bw(10)

# ---- 2-Hydroxyhexanoic Acid (DCM) n=12 ----
hydroxyhexanoic<-subset(data, odour == "2-Hydroxyhexanoic Acid (DCM)")
summary(hydroxyhexanoic)

ggplot(hydroxyhexanoic)+
  geom_boxplot(aes(x=conc,y=percent))+
  geom_point(aes(x=conc,y=percent))+
  xlab("Concentration")+ylab("Percent Reaction")+ggtitle("2-Hydroxyhexanoic Acid (DCM) (trials=12)")+
  theme_bw(10)

# ---- 2-Nonanal subset and ggplot n=13 ----
nonanal<-subset(data, odour == "2-Nonanal")
summary(nonanal)

ggplot(nonanal)+
  geom_boxplot(aes(x=conc,y=percent))+
  geom_point(aes(x=conc,y=percent))+
  xlab("Concentration")+ylab("Percent Reaction")+ggtitle("2-Nonanal (trials=13)")+
  theme_bw(10)

# ---- Benzoic Acid subset and ggplot n=10 ----
benzoic_acid<-subset(data, odour == "Benzoic Acid")
summary(benzoic_acid)

ggplot(benzoic_acid)+
  geom_boxplot(aes(x=conc,y=percent))+
  geom_point(aes(x=conc,y=percent))+
  xlab("Concentration")+ylab("Percent Reaction")+ggtitle("Benzoic Acid (trials=10)")+
  theme_bw(10)

# ---- Butyric Acid subset and ggplot n=9 ----
butyric_acid<-subset(data, odour == "Butyric Acid")
summary(butyric_acid)

ggplot(butyric_acid)+
  geom_boxplot(aes(x=conc,y=percent))+
  geom_point(aes(x=conc,y=percent))+
  xlab("Concentration")+ylab("Percent Reaction")+ggtitle("Butyric Acid (trials=9)")+
  theme_bw(10)



# ---- Dodecanal subset and ggplot n=10 ----
dodecanal<-subset(data, odour == "Dodecyl Aldehyde")
summary(dodecanal)

ggplot(dodecanal)+
  geom_boxplot(aes(x=conc,y=percent))+
  geom_point(aes(x=conc,y=percent))+
  xlab("Concentration")+ylab("Percent Reaction")+ggtitle("Dodecyl Aldehyde (trials=10)")+
  theme_bw(10)

# ---- Ethyl Palmitate subset and ggplot n=10 ----
ethyl_palmitate<-subset(data, odour == "Ethyl Palmitate")
summary(ethyl_palmitate)
hist(ethyl_palmitate$percent)

ggplot(ethyl_palmitate)+
  geom_boxplot(aes(x=conc,y=percent))+
  geom_point(aes(x=conc,y=percent))+
  xlab("Concentration")+ylab("Percent Reaction")+ggtitle("Ethyl Palmitate (trials=10)")+
  theme_bw(10)

# ---- Geraniol subset and ggplot n=10 ----
geraniol<-subset(data, odour == "Geraniol")
summary(geraniol)
hist(geraniol$percent)

ggplot(geraniol)+
  geom_boxplot(aes(x=conc,y=percent))+
  geom_point(aes(x=conc,y=percent))+
  xlab("Concentration")+ylab("Percent Reaction")+ggtitle("Geraniol (trials=10)")+
  theme_bw(10)



# ---- Heptacosane subset and ggplot n=9 ----
heptacosane<-subset(data, odour == "Heptacosane")
summary(heptacosane)
hist(heptacosane$percent)

ggplot(heptacosane)+
  geom_boxplot(aes(x=conc,y=percent))+
  geom_point(aes(x=conc,y=percent))+
  xlab("Concentration")+ylab("Percent Reaction")+ggtitle("Heptacosane (trials=9)")+
  theme_bw(10)


# ---- Heptadecane subset and ggplot n=10 ----
heptadecane<-subset(data, odour == "Heptadecane")
summary(heptadecane)
hist(heptadecane$percent)

ggplot(heptadecane)+
  geom_boxplot(aes(x=conc,y=percent))+
  geom_point(aes(x=conc,y=percent))+
  xlab("Concentration")+ylab("Percent Reaction")+ggtitle("Heptadecane (trials=10)")+
  theme_bw(10)




# ---- Hexadecanal subset and ggplot n=10 ----
hexadecanal<-subset(data, odour == "Hexadecanal")
summary(hexadecanal)
hist(hexadecanol$percent)

ggplot(hexadecanal)+
  geom_boxplot(aes(x=conc,y=percent))+
  geom_point(aes(x=conc,y=percent))+
  xlab("Concentration")+ylab("Percent Reaction")+ggtitle("Hexadecanal (trials=10)")+
  theme_bw(10)

# ---- Methyl Oleate subset and ggplot n=11 ----
methyl_oleate<-subset(data, odour == "Methyl Oleate")
summary(methyl_oleate)
hist(methyl_oleate$percent)

ggplot(methyl_oleate)+
  geom_boxplot(aes(x=conc,y=percent))+
  geom_point(aes(x=conc,y=percent))+
  xlab("Concentration")+ylab("Percent Reaction")+ggtitle("Methyl Oleate (trials=11)")+
  theme_bw(10)


# ---- Octadecanol subset and ggplot n=10 ----
octadecanol<-subset(data, odour == "Octadecanol")
summary(octadecanol)
hist(octadecanol$percent)

ggplot(octadecanol)+
  geom_boxplot(aes(x=conc,y=percent))+
  geom_point(aes(x=conc,y=percent))+
  xlab("Concentration")+ylab("Percent Reaction")+ggtitle("Octadecanol (trials=10)")+
  theme_bw(10)

# ---- Octanoic Acid subset anf ggplot n=11 ----
octanoic_acid<-subset(data, odour == "Octanoic Acid")
summary(octanoic_acid)
hist(octanoic_acid$percent)

ggplot(octanoic_acid)+
  geom_boxplot(aes(x=conc,y=percent))+
  geom_point(aes(x=conc,y=percent))+
  xlab("Concentration")+ylab("Percent Reaction")+ggtitle("Octanoic Acid (trials=11)")+
  theme_bw(10)

# ---- Palmitic Acid subset and ggplot n=7 at conc 10ug ----
palmitic_acid<-subset(data, odour == "Palmitic Acid")
summary(palmitic_acid)
hist(palmitic_acid$percent)

ggplot(palmitic_acid)+
  geom_boxplot(aes(x=conc,y=percent))+
  geom_point(aes(x=conc,y=percent))+
  xlab("Concentration")+ylab("Percent Reaction")+ggtitle("Palmitic Acid (trials=3)")+
  theme_bw(10)

# ---- trans-nerolidol subset and gplot n=10 ----
trans_nerolidol<-subset(data, odour == "trans-Nerolidol")
summary(trans_nerolidol)
hist(trans_nerolidol$percent)

ggplot(trans_nerolidol)+
  geom_boxplot(aes(x=conc,y=percent))+
  geom_point(aes(x=conc,y=percent))+
  xlab("Concentration")+ylab("Percent Reaction")+ggtitle("trans-Nerolidol (trials=10)")+
  theme_bw(10)




# ---- Faceting All Graphs Together ----

#ENSURE DATA IS CHANGED TO FACTORS 

# Step1: filter out messy plots
temp1<-subset(data, odour != "1-Hexadecanol")
temp2<-subset(temp1, odour != "Methyl Palmitate")
temp3<-subset(temp2, odour != "Hexane")
good_runs<-subset(temp3, odour != "Palmitic Acid")

#other ways to filter (didn't run it though)
attempt3 <- select(filter(data, odour != "Hexane"), c(trial:normalized)) #repeat with other factors
filtered_chemicals<- data[!(data$odour %in% c("Methyl Palmitate", "Hexane", "Palmitic Acid", "1-Hexadecanol")),] #run once

# Step2: Normalize scores at zero
subsetted <- mutate(good_runs,
                    Zresponse = normalized - 100)

# Step3: Filter out negative responses
nonegatives <- subsetted %>%
  mutate(Zresponse= ifelse(Zresponse <0, NA, Zresponse)) 
str(nonegatives)

# Step4: Make the plot
ggplot(nonegatives)+
  geom_boxplot(aes(x=conc,y= Zresponse, group= conc))+
  xlab("Concentration")+ylab("Normalized Percent Reaction")+ggtitle("Faceted ETG Plots")+
  theme_bw(10)+
  facet_wrap(~odour)





# ---- Correlational Tests (paired) for Temp-Humid-Time-Day-ETG ----
absoluteweather<- read.csv('absoluteweather.csv')

library(tidyverse)
absoluteweather$conc <- factor(absoluteweather$conc)
absoluteweather$trial <- factor(absoluteweather$trial)
str(absoluteweather)

ggplot(data = absoluteweather, mapping = aes(humid, absamp)) + 
  geom_jitter(aes(colour = absoluteweather$trial), width = 0.25) +
  ggtitle("amplitude vs humidity")+
  facet_wrap(~trial)

ggplot(data = absoluteweather, mapping = aes(temp, absamp)) + 
  geom_jitter(aes(colour = absoluteweather$trial), width = 0.25) +
  ggtitle("amplitude vs temperature")+
  facet_wrap(~trial)

ggplot(data = absoluteweather, mapping = aes(dewp, absamp)) + 
  geom_jitter(aes(colour = absoluteweather$trial), width = 0.25) +
  ggtitle("amplitude vs dewp")+
  facet_wrap(~trial)

# garbage
ggplot(data = absoluteweather, mapping = aes(min, absamp)) + 
  geom_jitter(aes(colour = absoluteweather$odour), width = 0.25) +
  ggtitle("amplitude vs time")+
  facet_wrap(~trial)

# histogram
ggplot(data = absoluteweather, mapping = aes(absamp))+
  geom_histogram(bins = 50)

# correlations
cor(absoluteweather$absamp, absoluteweather$temp)
cov(absoluteweather$absamp, absoluteweather$temp)
# correlational test to temperature
corr_amp_tempr <- cor.test(x=absoluteweather$absamp, 
                           y=absoluteweather$temp, method = 'spearman')
corr_amp_tempr

corr_amp_humid <- cor.test(x=absoluteweather$absamp, 
                           y=absoluteweather$humid, method = 'spearman')
corr_amp_humid

corr_amp_dewp<- cor.test(x=absoluteweather$absamp, 
                         y=absoluteweather$dewp, method = 'spearman')
corr_amp_dewp


library(fifer)
spearman.plot(absoluteweather$absamp, absoluteweather$temp)
weather_matrix <- data.matrix(absoluteweather[7:10], rownames.force = NA)

pairs(weather_matrix) # plots all the stuff simultaneiously



# ---- Plotting Time-Normalized Data Faceted by Trial ----
# Step1: Normalize scores at zero
subsetted <- mutate(absoluteweather,
                    Zresponse = normalized - 100)

# Step2: Filter out negative responses
nonegatives <- subsetted %>%
  mutate(Zresponse= ifelse(Zresponse <0, NA, Zresponse)) 
str(nonegatives$Zresponse)

# Polt that SOAB
ggplot(data = nonegatives, aes(min, absamp))+
  geom_jitter(aes(colour = nonegatives$odour), width = 0.25)+
  xlab("Time")+ylab("Absamp")+ggtitle("Faceted ETG Plots")+
  theme_bw(10)+
  facet_wrap(~trial)

# plot of the normalized data
ggplot(data = nonegatives, mapping = aes(Zresponse))+
  geom_histogram(bins = 50)



# ---- messing around with models ----
# garbage - ignore
require(ggplot2)
m2 <- glm(nonegatives$Zresponse ~ nonegatives$odour*nonegatives$conc + nonegatives$trial, poisson)
warnings()
plot(m2)


#porportion test probably garbage too
y <- cbind(absoluteweather$absamp, absoluteweather$linearhex)
y
m2 <- glm(y ~ absoluteweather$odour, family = binomial)
summary(m2)
plot(m2)

# remove noisy trials (for time being)
nonegatives$trial <- as.numeric(nonegatives$trial)
goodtrials <- nonegatives[nonegatives$trial %in% c(1:7,11, 12), ]
goodtrials$trial <- as.factor(goodtrials$trial)
str(goodtrials)
remove(subsetted)

library(tidyverse)
library(modelr)
options(na.action = na.warn)

ggplot(goodtrials, aes(proportion, Zresponse))+
  geom_point(aes(colour=trial))


