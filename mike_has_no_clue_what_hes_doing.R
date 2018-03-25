# Introduction to LM... jump!
library(tidyverse)
set.seed = 12321

# Linear model w/ one with a continuous predictor variable and a continous response variable
# The relationship described by a single parameter, B1 (slope, sand consequently a predictor of effect size)
predictor <- runif(100) - 0.5  ## a uniformly distributed random number between -0.5 and 0.5
B0 <- 1
B1 <- 3
response <- B0 + B1 * predictor
qplot(predictor, response)

m1 <- lm(response ~ predictor)
summary(m1)

par(mfrow = c(2, 2))
plot(m1)

predictor <- runif(100) - 0.5  ## a uniformly distributed random number between -0.5 and 0.5
B0 <- 3
B1 <- 3
error <- rnorm(100, mean = 0, sd = 1)  ## some normally distributed random error
response <- B0 + B1 * predictor + error
qplot(predictor, response)

qplot(predictor, response, geom = c("point", "smooth"), span = 0.9)

m1 <- lm(response ~ predictor)
summary(m1)

par(mfrow = c(2, 2))
plot(m1)


gen.mod <- function(n = 100, b0 = 2, b1 = 3, sd.err = 1) {
  predictor <- runif(n) - 0.5
  response <- b0 + b1 * predictor + rnorm(n, mean = 0, sd = sd.err)
  return(data.frame(response = response, predictor = predictor))
}

m1 <- lm(response ~ predictor, data = gen.mod(n = 100, b0 = 1, b1 = 1, sd.err = 2))
summary(m1)$coefficients["predictor", ]

par(mfrow = c(2, 2))
plot(m1)

est <- NULL
for (i in 1:1000) {
  m1 <- lm(response ~ predictor, data = gen.mod(n = 100, b0 = 1, b1 = 3, sd.err = 2))
  est[i] <- summary(m1)$coefficients["predictor", "Estimate"]
}
p <- ggplot(aes(est), data = data.frame(est = est))
p + geom_density() + geom_vline(xintercept = mean(est))

n.times <- 1000
est.df <- matrix(nrow = n.times, ncol = 2, NA)
for (i in 1:n.times) {
  m1 <- lm(response ~ predictor, data = gen.mod(n = 100, b0 = 1, b1 = 3, sd.err = 2))
  est.df[i, 1] <- summary(m1)$coefficients["predictor", "Estimate"]
  m1 <- lm(response ~ predictor, data = gen.mod(n = 1000, b0 = 1, b1 = 3, 
                                                sd.err = 2))
  est.df[i, 2] <- summary(m1)$coefficients["predictor", "Estimate"]
}
est.df <- data.frame(est.df)

## split est.df into a data frame in long format
est.df <- gather(est.df, value = "est", key="trial")
est.df$trial <- factor(est.df$trial)

est.sum.df <- group_by(est.df, trial) %>% summarize(mn.est = mean(est))

p <- ggplot(aes(est, group = trial, col = trial), data = est.df)
p + geom_density() + geom_vline(data = est.sum.df, aes(xintercept = mn.est, col = trial))



# ---- Don't Get It ----



n.times <- 1000
est.df <- matrix(nrow = n.times, ncol = 5, NA)
for (i in 1:n.times) {
  m1 <- lm(response ~ predictor, data = gen.mod(n = 100, b0 = 1, b1 = 3, sd.err = 1))
  est.df[i, 1] <- summary(m1)$coefficients["predictor", "Estimate"]
  m1 <- lm(response ~ predictor, data = gen.mod(n = 100, b0 = 1, b1 = 3, sd.err = 5))
  est.df[i, 2] <- summary(m1)$coefficients["predictor", "Estimate"]
  m1 <- lm(response ~ predictor, data = gen.mod(n = 100, b0 = 1, b1 = 3, sd.err = 10))
  est.df[i, 3] <- summary(m1)$coefficients["predictor", "Estimate"]
  m1 <- lm(response ~ predictor, data = gen.mod(n = 100, b0 = 1, b1 = 3, sd.err = 15))
  est.df[i, 4] <- summary(m1)$coefficients["predictor", "Estimate"]
  m1 <- lm(response ~ predictor, data = gen.mod(n = 100, b0 = 1, b1 = 3, sd.err = 20))
  est.df[i, 5] <- summary(m1)$coefficients["predictor", "Estimate"]
}
est.df <- data.frame(est.df)

## split est.df into a data frame in long format
est.df <- gather(est.df, value = "est", key="trial")
est.df$trial <- factor(est.df$trial)

est.sum.df <- group_by(est.df, trial) %>% summarize(mn.est = mean(est))

p <- ggplot(aes(est, group = trial, col = trial), data = est.df)
p + geom_density() + geom_vline(data = est.sum.df, aes(xintercept = mn.est, col = trial))

# ANGRY 

# --- can you please teach us, instead of assigning chapters to read - this stuff is important ----
library(tidyverse)
library(ggplot2)
set.seed = 12321

predictor <- runif(100) - 0.5  # a uniformly distributed random number between -0.5 and 0.5
B0 <- 1
B1 <- 3
response <- B0 + B1 * predictor
qplot(predictor, response)

# a function that let’s use generate some random data with some given parameter
gen.mod <- function(n = 50, b0 = 2, b1 = 3, sd.err = 1) {
  predictor <- runif(n) - 0.5
  response <- b0 + b1 * predictor + rnorm(n, mean = 0, sd = sd.err)
  return(data.frame(response = response, predictor = predictor))
}

# repeat
n.times <- 1000
# 'sample size'
n <- c(2, 20, 50)
# 'magnitude of B1'
b1 <- c(0.2, 1, 2.7, 4)
# 'error' (quality - t-value estemate)
sd.err <- c(0.1, 1.5, 5, 15)


test.df <- expand.grid(n.times = n.times, n = n, b1 = b1, sd.err = sd.err)

# Use that function to produce a data frame, with various combinations of each 
# of the three variables as 'factors' and the quality of the estimate as the response.

for (i in 1:length(test.df$n.times)) {
  tmp.df <- gen.mod(n = test.df$n[i], b0 = 2, b1 = test.df$b1[i], sd.err = test.df$sd.err[i])
  tmp.lm <- lm(response ~ predictor, data = tmp.df)
  test.df$tval[i] <- summary(tmp.lm)$coefficients["predictor", "Estimate"]
}

# Show the results visually.
p <- ggplot(data = test.df, aes(tval, group = b1, col = b1))
p + geom_density() + xlab("t value") + ylab("Density") + facet_grid(~sd.err)

#Do the same thing as above, but use a single parameter estimate of B1 = 0 (e.g. no effect).
# (Set B0 = 0). 
# Use a sample size of 100. Instead of using the ‘t’ value as your ‘quality’ estimate, 
# have the function output the ‘p-value’.

gen.mod <- function(n = 50, b0 = 2, b1 = 0, sd.err = 1) {
  predictor <- runif(n) - 0.5
  response <- b0 + b1 * predictor + rnorm(n, mean = 0, sd = sd.err)
  return(data.frame(response = response, predictor = predictor))
}

n.times <- 1000
n <- c(2, 20, 50)
b1 <- c(0)
sd.err <- c(0.1, 1.5, 5, 15)

test.df <- expand.grid(n.times = n.times, n = n, b1 = b1, sd.err = sd.err)

for (i in 1:length(test.df$n.times)) {
  tmp.df <- gen.mod(n = test.df$n[i], b0 = 2, b1 = test.df$b1[i], sd.err = test.df$sd.err[i])
  tmp.lm <- lm(response ~ predictor, data = tmp.df)
  test.df$pval[i] <- summary(tmp.lm)$coefficients["predictor", "Estimate"]
}
# what does it look like? = blue sticks
p <- ggplot(data = test.df, aes(pval, group = n, col = n))
p + geom_density() + facet_grid(~sd.err) + xlab("pee") + ylab("Density")


# 4b. What proportion of p-values are <= 0.05?
pvalues <- which(test.df$p.val <= 0.05)
tabulate(pvalues)

# 4c. How does that change when the amount of error changes. = different blue lines
p <- ggplot(data = test.df, aes(pval, group = n, col = n))
p + geom_density() + facet_grid(~sd.err)






# ---- What I’m aiming for is a visual analysis of how changing sample size (n), 
# effect size (the parameter estimate, B1) and 
# the amount of unexplained variation (error) 
#influences the quality of the parameter estimates. ----

# the gen.mod function - change to gen.df because basically we are making a dataframe 
# that will be used to produce different parameters we want to look at

library(tidyverse)

# so for the assignment, the first question is asking you for the t-values and how they
# change when we change error, B1, and n

## t- value -> is relationship of coefficient (the estemate) and the error
## p-value -> is the probability that you could have seen a result or parameter by chance

## Why we generate our own data?
# Because you know what the structure of the model you would need, 
# it is to test your understanding of how to fit the model, knowing what's going on

# This here, generates our fake data
gen.df <- function(n = 100, b0 = 2, b1 = 3, sd.err = 1) {
  predictor <- seq(-1, 1, length.out = n)
  response <- b0 + b1 * predictor + rnorm(n, mean = 0, sd = sd.err) # this is the actual formula
  return(data.frame(response = response, predictor = predictor))
}
# so the actual formula (response) will be what our lm will look like, 
# rnorm gives us data that fits a normal distribution
# values will range from -1 to 1, with a mean of zero (I think)

# Next, we need to create a data frame with all combinations of all of the variables 
# that we are interested in, each repeated ‘n.times’.

# To do this, we create vectors of the ranges of each of the variables, 
# and then create a data frame from these vectors.

n.times <- 1000
rep <- 1:n.times # this gives us every combination possible 
# (what people use in agricultural plots randomly chosen for treatment)
n <- c(10, 100, 1000)
b1 <- c(0.1, 1, 2.5, 5, 10, 20)
sd.err <- c(0.1, 1, 2, 5)

test.df <- expand.grid(rep = rep, n = n, b1 = b1, sd.err = sd.err)
# 72,000 observations!
# so now the experimental df is here, we can fit the lm to each of the rows of the dataframe
#
# the "for" function loops through every row in that dataframe 
for (i in 1:length(test.df$rep)) {
  tmp.df <- # makes a temporary dataframe that is dumpped when loop finished
    gen.df(n = test.df$n[i], 
           b0 = 2, # this is fixed (I think its the y-intercept?)
           b1 = test.df$b1[i], # represents the effect size apparently
           sd.err = test.df$sd.err[i])
  tmp.lm <- lm(response ~ predictor, data = tmp.df) # this gives us the linear model 
  test.df$est[i] <- summary(tmp.lm)$coefficients["predictor", "Estimate"]
} # this last bit applies the output of the lm to the test.df (new column est)
# not too sure why it says "predictor", "Estimate"

# And now, you have a quality estimate that you can plot, 
# for all levels of the variables that you had control over (n, b1, sd.err).

# doing it this way is labor intensive on the computer, takes a few minutes
# as opposed to doing it in chunks (all values associated with different n, error, B1)

str(test.df)
# we want B1 as factor so we can plot it..
test.df$b1 <- factor(test.df$b1)
p <- ggplot(data = test.df, aes(est, group = b1, col = b1))
p + geom_density()
# so here, we can see the change caused by effect size (B1)

# the original question was asking to use the t-value to identify these changes in the three
# B1, error, and n - so we need to do this

## so this time, we are making 10 times looking at each of the three interests
n.times <- 1:10
n <- c(10, 100, 1000)
b1 <- c(0.1, 1, 2.5, 5, 10, 20)
sd.err <- c(0.1, 1, 2, 5)
# re-make the dataframe, 720 observations
test.df <- expand.grid(n.times = n.times, n = n, b1 = b1, sd.err = sd.err)

## so again, we will run the same loop, only that we are only doing it 720 times
# same idea, but this time we will be making a new column for t-value
for (i in 1:length(test.df$n.times)) {
  tmp.df <- gen.df(n = test.df$n[i], 
                   b0 = 2, 
                   b1 = test.df$b1[i], 
                   sd.err = test.df$sd.err[i])
  tmp.lm <- lm(response ~ predictor, data = tmp.df)
  test.df$est[i] <- summary(tmp.lm)$coefficients["predictor", "t value"]
}
# again, no clue what make this "t-value" any different from what we just calculated in the 
# other loop process (what makes this code different, so that it reports the t?)

# so yes, coefficients is a code that will extract what you list so out of the "predictor" 
# we are looking for the t-value for every run of the loop (720 times) 

# so now we will change a bunch of these to factors so we can graph them:
test.df$n <- factor(test.df$n)
test.df$b1 <- factor(test.df$b1)
test.df$sd.err <- factor(test.df$sd.err)

## Let’s look at the means of those t values, and how they change with n, b1 and error.
test.sum <- 
  group_by(test.df, n, sd.err, b1) %>%
  summarise(gm = exp(mean(log(abs(est)))))
# how did you know to do the exponnt of the mean, log, absolute value of the estemate? 

p <- ggplot(data = test.sum, aes(b1, gm, col = n, group = n))
p + geom_point() + geom_line() + facet_grid(sd.err ~ .) + scale_y_log10()
## so from this we can see that the t-value gets larger with an incerase in 
## the sample size (n), the error, and the B1 (effect size) 


# Ok, now on to the ‘p-values’ part

# Do the same thing as above, but use a single parameter estimate of B1 = 0 
# (e.g. no effect; set B0 = 0). Use a sample size of 100. 
# Instead of using the ‘t’ value as your ‘quality’ estimate, 
# have the function output the ‘p-value’.

## So making the B0 = 0 means that there is no effect of the predictor on the response
# and this way we can test to see how often we get a p-value of less than 0.05 (5%)
# this tells us that indeed, we can have a significant result just by chance
# 5% of the tme we could declare an effect of the coefficient, even though there is not
# basically committing a Type I error?

n.times <- 1:2000  ## do this enough times to matter!
n <- c(100)
b1 <- c(0)
sd.err <- c(0.1, 1, 2, 5)  ## we can leave the error in here, because we will use it

test.df <- expand.grid(n.times = n.times, n = n, b1 = b1, sd.err = sd.err)

# so this looks a the p-value
for (i in 1:length(test.df$n.times)) {
  tmp.df <- gen.df(n = test.df$n[i], b0 = 0, b1 = test.df$b1[i], sd.err = test.df$sd.err[i])
  tmp.lm <- lm(response ~ predictor, data = tmp.df)
  ## extract the p-value associated with each t
  test.df$est[i] <- summary(tmp.lm)$coefficients["predictor", "Pr(>|t|)"]
}
# so somehow it pulls from the lm output the p-values

test.df$n <- factor(test.df$n)
test.df$b1 <- factor(test.df$b1)
test.df$sd.err <- factor(test.df$sd.err)
# so basically we want to find the disstribution of the p-values
# we extracted them from the lm, and they are labeled as est in the test.df

test.sub <- subset(test.df, sd.err == 5)

p <- ggplot(data = test.sub, aes(est))
p + geom_density()
# What proportion of p-values are <= 0.05?

# so using this "with" 
with(test.sub, table(est <= 0.05))/length(test.sub$est)

group_by(test.df, sd.err) %>% 
  summarise(sum.tab = table(est <= 0.05)[2]/length(est))

# And what if we change the sample size?

n.times <- 1:2000  ## do this enough times to matter!
n <- c(10, 100, 1000)
b1 <- c(0)
sd.err <- c(0.1, 1, 2, 5)  ## we can leave the error in here, because we will use it

test.df <- expand.grid(n.times = n.times, n = n, b1 = b1, sd.err = sd.err)

for (i in 1:length(test.df$n.times)) {
  tmp.df <- gen.df(n = test.df$n[i], b0 = 0, b1 = test.df$b1[i], sd.err = test.df$sd.err[i])
  tmp.lm <- lm(response ~ predictor, data = tmp.df)
  ## extract the p-value associated with each t
  test.df$est[i] <- summary(tmp.lm)$coefficients["predictor", "Pr(>|t|)"]
}

test.df$n <- factor(test.df$n)
test.df$b1 <- factor(test.df$b1)
test.df$sd.err <- factor(test.df$sd.err)

group_by(test.df, sd.err, n) %>% 
  summarise(p.05 = table(est <= 0.05)[2]/length(est))




# ---- GLMs and simulation, continued ----
# Three things. 
#1. Extending to a model matrix format 
#2. Poisson processes 
#3. Binomial processes

# Model Matrix: matrix that generates a set of responses, 
# underlining the way that a model fits

# Matrix Multiplications
# Model Matrix x Values

#B0 = 10
#B1 = 2
#  10        2   <- Parameters
#Intercept Predictor Response 
# 1           0.3     B0+B1*Predictor
# 1           0.4
# 1           0.5
# 1           0.2
# 1           0.1

# A GLM predicts the parameters (unknown) given the response values (data) and the error

## The underlying code for glm takes your ‘model’ statement (response ~ predictor) 
# and turns it in to a ‘model matrix’. 
# That is the underlying matrix that is used to help estimate the parameters for the model.

#In the simplest continuous predictor value case, 
# the model matrix is just the predictors, plus a set of ‘1s’ for the intercept.

pred <- runif(5)
mm <- model.matrix(~pred)
print(mm)


# -----------------------------------------------------------------------------------------

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
nonegatives<-subset(nonegatives, conc != 0)
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
ggplot(data = nonegatives, aes(min, Zresponse))+
  geom_jitter(aes(colour = nonegatives$odour), width = 0.25)+
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
  data_grid(data = gucci, odour,conc) %>% 
  add_predictions(mod01)
grid

ggplot(gucci, aes(x = conc)) + 
  geom_boxplot(aes(y = Zresponse)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 0.2)+
  facet_wrap (~odour)


# did a glm model - not sure if it's any good, but looks better (gave a warning)
# had to convert Zresponse to integer for poisson
mod01 <- glm(I(as.integer(goodtrials$Zresponse))~conc+odour, data = goodtrials, poisson)
par(mfrow=c(2, 2))
plot(mod01)
mod01 <- glm(I(as.integer(goodtrials$Zresponse))~0, data = goodtrials, poisson)

set.seed(123)
mod01 <- glm(I(as.integer(goodtrials$Zresponse))~ -1 +odour+conc,random = list(~0+trial), 
             varcomps.names = c("trial"), data = goodtrials, 
             family.glmm = poisson.glmm, m=1)
summary(mod01)
# ---- not any more helpful than before ----
# plot integer
mod01 <- lm(I(as.integer(goodtrials$Zresponse))~odour,data = goodtrials)
par(mfrow=c(2, 2))
plot(mod01)
# plot non-integer
mod01 <- lm(Zresponse~0+odour+conc+odour*conc,data = goodtrials)
par(mfrow=c(2, 2))
plot(mod01)

# plot glm integer
mod01 <- glm(I(as.integer(goodtrials$Zresponse))~0+odour+conc+odour*conc,data = goodtrials, poisson)
# plot glm non-integer
plot(mod01 <- glm((log(Zresponse))~-1+odour+conc+odour*conc,data = goodtrials))
print(mod01)



# ---- lme4 package ----
#nlmer(formula, data = NULL, control = nlmerControl(),
#   start = NULL, verbose = 0L, nAGQ = 1L, subset, weights, na.action,
# offset, contrasts = NULL, devFunOnly = FALSE, ...)
library(lme4)
glmm1 <- glmer(Zresponse ~ -1 + odour + (1| trial),
               data = gucci)

glmm1 <- glmer(Zresponse ~ -1 + conc*odour + (1| trial),
               data = gucci)


summary(glmm1)
plot(glmm1)
print(glmm1, correlation=TRUE)
vcov(glmm1)
isGLMM(glmm1) # about as useful as an ass

# ---- Analysis of Variance ----
plot(aov_out <- aov(Zresponse~trial*odour*conc, data = gucci))
summary(aov_out)
warnings()

plot(mod01 <- lm(Zresponse~-1+trial*odour*conc, data = gucci))

#I tells formula to change zresponse to an integer 
mod01 <- glm(I(as.integer(gucci$Zresponse))~conc+odour, data = goodtrials, poisson)
par(mfrow=c(2, 2))
plot(mod01)


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




