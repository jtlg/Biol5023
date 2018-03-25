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



#RANDOM EFFECT
# we try to estemate parameters that describe the distribution of this random effect
# 1. observations a random sample of a population (usually)
# 2. the effects observed usually random (the response)
# 3. blocking factors can often be said as random effects (a random sample of blocks potentially available)
# aka trials, odours selected, depends how you want to do it

# The decision to select random/fixed is based off your judgement and what you want to look for
# mixed effects model (has both random and fixed) an example is 2-Way ANOVA

# 

milk <- glmm(Zresponse ~ -1+odour, varcomps.names = c("Test.ID"),random = list(~0+trial), 
             data = gucci, m = 1, doPQL = TRUE, family.glmm = binomial.glmm)
# can't do gaussian

#glmer(formula, data = NULL, family = gaussian, control = glmerControl(),
#      start = NULL, verbose = 0L, nAGQ = 1L, subset, weights, na.action,
#      offset, contrasts = NULL, mustart, etastart,
 #     devFunOnly = FALSE, ...)

glmer(Zresponse~-1+odour+conc+(1|trial), data = gucci, family = gaussian)
# The random effect is represented by (1|operator) indicating that the data is grouped by operator and 
# the 1 indicating that the random effect is constant within each group. 

aov1 <- aov(Zresponse~-1+odour, gucci)
aov2 <- aov(Zresponse~-1+odour*conc, gucci)
aov3 <- aov(Zresponse~-1+odour*conc*gb, gucci)
aov4 <- aov(Zresponse~-1+odour*conc*gb*trial, gucci)
aov5 <- aov(Zresponse~-1+odour*conc*gb*trial*Test.ID, gucci)
aov6 <- aov(Zresponse~-1+odour*conc*gb*trial*Test.ID*min, gucci)

aov(aov1*aov2*aov3*aov4*aov5*aov6)

