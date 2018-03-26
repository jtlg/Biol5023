#Here is the good code, excluding all the redundant stuff we did earlier
library(tidyverse)

# ---- Plotting Time-Normalized Data Faceted by Trial ----
absoluteweather <- read.csv('absoluteweather.csv')
str(absoluteweather)
absoluteweather$trial <- as.factor(absoluteweather$trial)
absoluteweather$conc <- as.factor(absoluteweather$conc)

# ---- Step1: Normalize scores at zero ----
subsetted <- mutate(absoluteweather,
                    Zresponse = normalized - 100)

# ---- Step2: Filter out negative responses ----
nonegatives <- subsetted %>% #0 or NA for nonsense values?
  mutate(Zresponse= ifelse(Zresponse <0, NA, Zresponse)) 
str(nonegatives$Zresponse)

# ---- Step3: Remove any additional zeros (Hexane) omit this step if changing NA's to zeros ----
nonegatives<-subset(nonegatives, Zresponse != 0)
# this also apparently removes any and all NA's...



# ---- Step4: Remove noisy trials (for time being) ----
goodtrials <- nonegatives[nonegatives$trial %in% c(1:8,11, 12), ]
str(goodtrials)
remove(subsetted)
# ---- Plotting the good trials & looking to see which odours to omit completely ----
ggplot(goodtrials)+
  geom_boxplot(aes(x=conc,y= Zresponse, group= conc))+
  xlab("Concentration")+ylab("Normalized Percent Reaction")+ggtitle("Faceted ETG Plots")+
  theme_bw(10)+
  facet_wrap(~odour)

summary(goodtrials$odour) # looking who to kill off...

# this actually depends on what the question is: may be worth keeping 
# single puff trials when comparing individual odours, but then tossing them when we want to compare
# if there is a trend when looking across a series of concentrations

# ---- QUESTIONS REFINED FURTHER - Need more refinement and possibly additional tests ---- 
# SO
# IS A REACTION TO AN ODOUR OF A SPECIFIC CONCENTRATION SIGNIFICANTLY DIFFERENT THAN THAT OF ANOTHER?
# in this case we want a model to look at odour exclusivly, 
#while possibly accounting trial and concentration as random variables?

# IS A REACTION OF AN ODOUR ACROSS A SERIES OF CONCENTRATIONS SIGNIFICANTLY DIFFERENT THAN THAT OF ANOTHER?
# this seems a little more difficult to proove

# AND IS THE REACTION TO AN INDIVIDUAL ODOUR ACROSS A SERIES OF CONCENTRATIONS TRENDY? (EXPONENTIAL?)
# this sounds like a completly other test required other than a GLMM and probably neds more data

# --- Cutting odours, thoes that have zero observations (open for further info) ----
# (one-off removal's should be only used if requireing a model to test across different concentrations)
gucci<- filter(goodtrials,
               !(odour %in% c("Hexane", "Palmitic Acid" )))
# to remove one-offs add: "Octadecanol", "Ethyl Palmitate" in addition to the above.

# ---- Polt that SOAB ----
ggplot(data = gucci, aes(min, absamp))+
  geom_jitter(aes(colour = gucci$odour), width = 0.25)+
  xlab("Time")+ylab("Absamp")+ggtitle("Faceted ETG Plots")+
  theme_bw(10)+
  facet_wrap(~trial)

# plot of the normalized data
ggplot(data = gucci, mapping = aes(log(Zresponse)))+
  geom_histogram(bins = 50)

# ---- messing around with models ----
# garbage - ignore
require(ggplot2)
m2 <- glm(nonegatives$Zresponse ~ nonegatives$odour*nonegatives$conc + nonegatives$trial, poisson)
warnings()
plot(m2)


#porportion test (probably garbage too)
y <- cbind(absoluteweather$absamp, absoluteweather$linearhex)
y
m2 <- glm(y ~ absoluteweather$odour, family = binomial)
summary(m2)
plot(m2)

# ---- Making a Linear model of Zresponse~conc+odour ----
library(tidyverse)
library(modelr)
library(glmm)
options(na.action = na.warn)

# changed to log of absamp... guess I need to carry this over to the rest of csv data
logging <- log(gucci$absamp)
ggplot(gucci, aes(trial, logging))+
  #geom_point(aes(colour=odour))+
  geom_jitter(aes(colour=odour))

mod01 <- lm(Zresponse~conc+odour, data = gucci)

grid <- gucci %>% 
  data_grid(data= gucci, odour,conc) %>% 
  add_predictions(mod01)
grid

ggplot(gucci, aes(x = conc)) + 
  geom_point(aes(y = Zresponse)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 0.2)+
  facet_wrap (~odour)


# did a glm model - not sure if it's any good, but looks better (gave a warning)
# had to convert Zresponse to integer for poisson
mod02 <- glm(I(as.integer(gucci$Zresponse))~conc+odour, data = gucci, poisson)

par(mfrow=c(2, 2))

plot(mod02)

mod02 <- glm(I(as.integer(gucci$Zresponse))~0, data = gucci, poisson)

set.seed(123)
mod03 <- glm(I(as.integer(goodtrials$Zresponse))~0+odour+conc,random = list(~0+trial), 
             varcomps.names = c("trial"), data = goodtrials, 
             family.glmm = poisson.glmm, m=1)
summary(mod03)
# ---- not any more helpful than before ----
# plot integer
mod04 <- lm(I(as.integer(goodtrials$Zresponse))~odour,data = goodtrials)
par(mfrow=c(2, 2))
plot(mod04)
# plot non-integer
mod04 <- lm(Zresponse~0+odour+conc+odour*conc,data = goodtrials)
par(mfrow=c(2, 2))
plot(mod04)

# plot glm integer
mod05 <- glm(I(as.integer(goodtrials$Zresponse))~0+odour+conc+odour*conc,data = goodtrials, poisson)
# plot glm non-integer
plot(mod05 <- glm((log(Zresponse))~-1+odour+conc+odour*conc,data = goodtrials))
print(mod05)



# ---- lme4 package ----
#nlmer(formula, data = NULL, control = nlmerControl(),
#   start = NULL, verbose = 0L, nAGQ = 1L, subset, weights, na.action,
# offset, contrasts = NULL, devFunOnly = FALSE, ...)
library(lme4)
glmm1 <- glmer(Zresponse ~ + odour + (1| trial),
               data = goodtrials, family = poisson)

glmm1 <- glmer(Zresponse ~ conc*odour + (1| trial),
               data = goodtrials, family = poisson)


summary(glmm1)
plot(glmm1)
print(glmm1, correlation=TRUE)
vcov(glmm1)
isGLMM(glmm1) # about as useful as an ass

# ---- Analysis of Variance ----
plot(aov_out <- aov(Zresponse~trial*odour*conc, data = gucci))
summary(aov_out)
warnings()

plot(mod_gaussin <- lm(Zresponse~-1+trial*odour*conc, data = gucci))

#I tells formula to change zresponse to an integer 
mod_poisson <- glm(I(as.integer(gucci$Zresponse))~conc+odour, data = goodtrials, poisson)
par(mfrow=c(2, 2))
plot(mod_poisson)


# Trial as a random factor
summary(gucci)
plot(gucci$trial,gucci$Zresponse)
print(fm01 <- lmer(Zresponse ~ -1+odour + conc + gb + (1|trial), gucci, REML = FALSE))




# ---- A Pratical Guide to Mixed Models ----
# PART ONE: FITTING YOUR DATA TO DIFFERENT DISTRIBUTIONS
require(car)
require(MASS)
par(mfrow=c(2, 2))# this sets the number of polots you can have in the plot window

# plot using normal distribution
qqp(gucci$Zresponse, "norm")

# plot using log-normal distribution
qqp(gucci$Zresponse, "lnorm")

# needs non-integers or something 
#nbinom <- fitdistr(gucci$Zresponse, "Negative Binomial")
#qqp(goodtrials$zerot, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
# plot poisson distribution (warnings regarding non-integer)
poisson <- fitdistr(gucci$Zresponse, "Poisson")
qqp(gucci$Zresponse, "pois", poisson$estimate)

#plot gamma distribution ("IM THE BEST" - toad, probably)
gamma <- fitdistr(gucci$Zresponse, "gamma")
qqp(gucci$Zresponse, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])




# ---- OUTPUTS OF A LMM
#THE RANDOM EFFECTS VARIANCE AND ST.DEV
# an estimate of the variance explained by the random effect, important,
#  if it's indistinguishable from zero, then your random effect probably doesn't matter
# and you can go ahead and do a regular linear model instead

# Next we have estimates of the fixed effects, with standard errors
# Some journals like you to report the results of these models as effect sizes with confidence intervals
#  if you want some p-values you'll have to turn to the Anova function in the car package
#library(car)
#Anova(<your lmm>)

# ---- FAILURE TO CONVERGE MODEL
# R may throw you a "failure to converge" error, 
# which usually is phrased "iteration limit reached without convergence."
# this means your model has too many factors and not a big enough sample size, and cannot be fit
# What you should then do is drop fixed effects and random effects from the model 
# and compare to see which fits the best.

# 1. Drop fixed effects and random effects one at a time. 
# 2. Hold the fixed effects constant and drop random effects one at a time and find what works best
# 3. Then hold random effects constant and drop fixed effects one at a time.
# anova function with a lowercase 'a' is for comparing models (from pkg: car).

# 3b. If your data are not normally distributed
# 1. we need to test whether we can use penalized quasilikelihood (PQL) or not
# a flexible technique that can deal with non-normal data, unbalanced design, and crossed random effects
#  it produces biased estimates if your response variable fits a discrete count distribution, 
# like Poisson or binomial, and the mean is less than 5 - or if your response variable is binary
mean(gucci$Zresponse) # goudda


# That means we can proceed with the PQL method. 
# But before we proceed, let's return to the matter of transformation to normality.

# performing a GLMM on an untransformed variable, 
# better because it better captures the variance of x
library(MASS)
library(tidyverse)

# Note that instead of taking all the fixed and random effects as one formula, 
# the random effects get their own argument in the glmmPQL function.

# To set the distribution to log-normal, 
# we set the family to gaussian (another word for normal) and the link to log.

# The link can be anything, 
# though if you want to use something besides log or inverse 
# then you'll have to research how to customize the link function yourself.
gucci <- tibble::rowid_to_column(gucci, "Test.ID")
str(gucci$Test.ID)

PQL <- glmmPQL(Zresponse ~ odour + conc, ~1 | trial/Test.ID, family = gaussian(link = "log"),
               data = gucci, verbose = FALSE)
summary(PQL)
plot(PQL)




# fitting general linear mixed effect models 
library(lme4)
f2r2 <- glmer(Zresponse~-1+odour+conc+(1|trial)+(1|odour:conc), data = gucci, family = gaussian)
f1r1 <- glmer(Zresponse~-1+odour+(1|trial), data = gucci, family = gaussian)
f2r1 <- glmer(Zresponse~-1+odour+conc+(1|trial), data = gucci, family = gaussian)
f2r2a <- glmer(Zresponse~-1+odour+conc+gb+(1|trial), data = gucci, family = gaussian)
f1x1r1 <- glmer(Zresponse~-1+odour*conc+(1|trial), data = gucci, family = gaussian)


#testing for significance
anova(f1r1,f2r1,f2r2,f2r2a, f1x1r1)

grid <- gucci %>% 
  data_grid(data= gucci, odour,conc) %>% 
  add_predictions(f1r1)
grid

ggplot(gucci, aes(x = conc)) + 
  geom_boxplot(aes(y = Zresponse)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 0.2)+
  facet_wrap (~odour)



