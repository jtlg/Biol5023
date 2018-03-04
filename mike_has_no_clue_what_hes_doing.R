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

