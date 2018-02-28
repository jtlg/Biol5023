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
B1 <- 2.3
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






