# JAGS implementation of the regression model examples
# Author: Alex Ziyu Jiang

library(rethinking)
library(rjags)
rm(list = ls())

data("Howell1")
d <- Howell1[Howell1$age > 18, ]


# seed to reproduce results
set.seed(10)

nsim <- 1e4
mu.p    <- rnorm(nsim, mean = 0, sd = 100)
sd.p    <- runif(nsim, min = 0, max = 50)
h_prior <- rnorm(nsim, mean = mu.p, sd = sd.p)

# note we expect heights to be anything from -1.6 meters to 1.6 meters
hist(h_prior)
precis(h_prior)

# the Jags implementation
# notice that in Jags, the second parameter in dnorm() represents
# the precision instead of the standard deviation!!!
model_code <- "
  model{
    # likelihood
    for (i in 1:N) {
      h[i] ~ dnorm(mu, tau)
    }
    # prior
    mu ~ dnorm(0, pow(100,-2))
    sigma ~ dunif(0, 50)
    # transformed quantities
    tau <- pow(sigma,-2)
  }
"

model <- jags.model(file = textConnection(model_code),
                    data = list(h = d$height,
                                N = length(d$height)))
samples <- coda.samples(model,
                        variable.names = c("mu", "sigma"),
                        n.iter = 1e4)
samples <- as.data.frame(samples[[1]])
precis(samples, digits = 10)


# Model 2 -- weakly informative prior -------------------------------------

# now let's try to get some priors that make sense
# and match our understanding of heights

# we are going to assume:
# yi ~ normal(mu, sigma) # likelihood
# mu ~ normal(165, 20)
# sigma ~ uniform(0, 10)

# do these priors make more sense?
nsim <- 1e4
mu.p    <- rnorm(nsim, mean = 165, sd = 20)
sd.p    <- runif(nsim, min = 0, max = 10)
h_prior <- rnorm(nsim, mean = mu.p, sd = sd.p)


# note how most observations are between 1.3 meters and  2 meters
hist(h_prior)
precis(h_prior)

# how much does that change our posteriors?
model_code2 <- "
  model{
    # likelihood
    for (i in 1:N) {
      h[i] ~ dnorm(mu, tau)
    }
    # prior
    mu ~ dnorm(165, pow(20, -2))
    sigma ~ dunif(0, 10)
    # transformed quantities
    tau <- pow(sigma, -2)
  }
"

model2 <- jags.model(file = textConnection(model_code2),
                    data = list(h = d$height,
                                N = length(d$height)))
samples2 <- coda.samples(model2,
                        variable.names = c("mu", "sigma"),
                        n.iter = 1e4)
samples2 <- as.data.frame(samples2[[1]])
precis(samples2, digits = 10)
# It barely changes from the previous model!
# Here we have enough data, and our weakly informative prior leads to the same answers.


# Model 3 -- strong priors ------------------------------------------------

# now suppose we are experts on !Kung San people
# and we strongly believe that mu ~ 165cm

# we are going to assume:
# yi ~ normal(mu, sigma) # likelihood
# mu ~ normal(165, .1)
# sigma ~ uniform(0, 10)

# here's what our prior implies about expected heights:
nsim <- 1e4
mu.p    <- rnorm(nsim, mean = 165, sd = .1)
sd.p    <- runif(nsim, min = 0, max = 10)
h_prior <- rnorm(nsim, mean = mu.p, sd = sd.p)


# note how most observations are concentrated between 1.56 meters and  1.74 meters
hist(h_prior)
precis(h_prior)

# this strongly contradicts our data
# note 56% of the individuals of our data have heights below 1.56
mean(d$height < 156)

# let's see how this affects our posterior
model_code3 <- "
  model{
    # likelihood
    for (i in 1:N) {
      h[i] ~ dnorm(mu, tau)
    }
    # prior
    mu ~ dnorm(165, pow(0.1,-2))
    sigma ~ dunif(0, 10)
    # transformed quantities
    tau <- pow(sigma,-2)
  }
"

model3 <- jags.model(file = textConnection(model_code3),
                     data = list(h = d$height,
                                 N = length(d$height)))
samples3 <- coda.samples(model3,
                         variable.names = c("mu", "sigma"),
                         n.iter = 1e4)
samples3 <- as.data.frame(samples3[[1]])
precis(samples3, digits = 10)

# note how we barely changed our prior beliefs about the average height after seeing the data.
# also note that our posterior about sigma changed, and it basically reaches the
# maximum we allowed (10).
# This happens because, for the model to explain that many observations below the average, it needs to allow for greater variance.

# Updating our beliefs step by step ---------------------------------------


# starting from our very uninformative prior
# let's see how our posterior changes after each observation is added

# shuffle observations
d <- d[sample(1:346), ]

# prior beliefs
prior <- data.frame(mu = rnorm(nsim, 0, 100), sigma = runif(nsim, 0, 50))
precis(prior)

## Posterior after 5 observations

model_code_after5 <- "
  model{
    # likelihood
    for (i in 1:N) {
      h[i] ~ dnorm(mu, tau)
    }
    # prior
    mu ~ dnorm(0, pow(100,-2))
    sigma ~ dunif(0, 50)
    # transformed quantities
    tau <- pow(sigma,-2)
  }
"
model_after5 <- jags.model(file = textConnection(model_code_after5),
                     data = list(h = d$height[1:5],
                                 N = 5))
samples_after5 <- coda.samples(model_after5,
                         variable.names = c("mu", "sigma"),
                         n.iter = 1e4)
samples_after5 <- as.data.frame(samples_after5[[1]])
precis(samples_after5, digits = 10)


# note 5 observations are already enough to put most of the mass of mu in [144, 160]



## Posterior after 10 observations

model_code_after10 <- "
  model{
    # likelihood
    for (i in 1:N) {
      h[i] ~ dnorm(mu, tau)
    }
    # prior
    mu ~ dnorm(0, pow(100,-2))
    sigma ~ dunif(0, 50)
    # transformed quantities
    tau <- pow(sigma,-2)
  }
"
model_after10 <- jags.model(file = textConnection(model_code_after10),
                           data = list(h = d$height[1:10],
                                       N = 10))
samples_after10 <- coda.samples(model_after10,
                               variable.names = c("mu", "sigma"),
                               n.iter = 1e4)
samples_after10 <- as.data.frame(samples_after10[[1]])
precis(samples_after10, digits = 10)
# after 10 observations, most of the mass of mu in [149, 157]



## Posterior after 100 observations

model_code_after100 <- "
  model{
    # likelihood
    for (i in 1:N) {
      h[i] ~ dnorm(mu, tau)
    }
    # prior
    mu ~ dnorm(0, pow(100,-2))
    sigma ~ dunif(0, 50)
    # transformed quantities
    tau <- pow(sigma,-2)
  }
"
model_after100 <- jags.model(file = textConnection(model_code_after100),
                            data = list(h = d$height[1:100],
                                        N = 100))
samples_after100 <- coda.samples(model_after100,
                                variable.names = c("mu", "sigma"),
                                n.iter = 1e4)
samples_after100 <- as.data.frame(samples_after100[[1]])
precis(samples_after100, digits = 10)
