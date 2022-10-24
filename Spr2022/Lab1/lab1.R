## first we need to install the package written for the course
## for a detailed installation instruction, please see
## https://www.rdocumentation.org/packages/rethinking/versions/2.13
devtools::install_github("stan-dev/cmdstanr")
install.packages(c("coda","mvtnorm","devtools","loo"))
library(devtools)
devtools::install_github("rmcelreath/rethinking")
library(rethinking)

## Chapter 2: 2M1
p_grid <- seq( from=0 , to=1 , length.out=100 )
# likelihood of 3 water in 3 tosses
likelihood <- dbinom( 3 , size=3 , prob=p_grid )

# Since the prior is uniform, it can be omitted from the calculations
prior <- rep(1,100) # uniform prior
posterior <- likelihood * prior
posterior <- posterior / sum(posterior) # standardize the distribution

# something with nicer labels and a line instead of individual points:
plot( posterior ~ p_grid , type="l", main = "(1) W W W" )

# likelihood of 3 water in 4 tosses
likelihood_2 <- dbinom( 3 , size=4 , prob=p_grid )
posterior_2 <- likelihood_2 * prior
posterior_2 <- posterior_2 / sum(posterior_2) # standardize
plot( posterior_2 ~ p_grid , type="l", main = "(2) W W W L" )


# likelihood of 5 water in 7 tosses
likelihood_3 <- dbinom( 5 , size=7 , prob=p_grid )
posterior_3 <- likelihood_3 * prior
posterior_3 <- posterior_3 / sum(posterior_3) # standardize
plot( posterior_3 ~ p_grid , type="l", main = "(3) L W W L W W W" )

## put these together
par(mfrow = c(1,3))
plot( posterior ~ p_grid , type="l", main = "(1) W W W")
plot( posterior_2 ~ p_grid , type="l", main = "(2) W W W L")
plot( posterior_3 ~ p_grid , type="l", main = "(3) L W W L W W W")


### M2
par(mfrow = c(1,1))
p_grid <- seq( from=0 , to=1 , length.out=100 )
prior_truncated <- ifelse( p_grid < 0.5 , 0 , 1 ) # new prior
posterior_truncated <- likelihood * prior_truncated
posterior_truncated <- posterior_truncated / sum(posterior_truncated) # standardize
par(mfrow = c(1,2))
plot( posterior ~ p_grid , type="l", main = "(1) W W W" )
plot( posterior_truncated ~ p_grid , type="l" )

par(mfrow = c(1,1))

posterior_2_truncated <- likelihood_2 * prior_truncated
posterior_2_truncated <- posterior_2_truncated / sum(posterior_2_truncated) # standardize
plot(posterior_2_truncated ~ p_grid , type="l", main = "(2) W W W L" )

posterior_3_truncated <- likelihood_3 * prior_truncated
posterior_3_truncated <- posterior_3_truncated / sum(posterior_3_truncated) # standardize
plot(posterior_3_truncated ~ p_grid , type="l", main = "(3) L W W L W W W" )


### M3
0.3*0.5 / ( 0.3*0.5 + 1*0.5 )


## Chapter 3: 3E1

# coding in the textbook
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

#compute the number of samples that are below 0.2
sum( samples < 0.2 )

#compute the ratio of samples that are below 0.2
sum( samples < 0.2 ) / 1e4
# 3E2

# change to a different number
sum( samples > 0.8 ) / 1e4

# 3E3
sum( samples > 0.2 & samples < 0.8 ) / 1e4

# 3E4
quantile( samples , 0.2 )
sum( samples < 0.52  ) / 1e4

# 3E5
quantile( samples , 0.8 )
sum( samples > 0.75  ) / 1e4

# 3E6
# highes posterior density interval
HPDI(samples, prob = 0.66)

# 3E7
# percentile inervals
PI( samples , prob=0.66 )

# 3M1
## similar code to 3E1, with different data counts
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

plot( posterior ~ p_grid , type="l" )

# 3M2
# use the sample() function to sample posterior draws based on the grid
# approximation
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

# get highest posterior density interval based on the posterior draws
HPDI( samples , prob=0.9 )

# 3M3
# use rbinom to simulate samples
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
w <- rbinom( 1e4 , size=15 , prob=samples )

# compute the proportion of these simulated observations that match the data:
sum( w==8 ) / 1e4

# compute the proportion of these simulated observation matching the data
simplehist(w)


# 3M4: change the data
w <- rbinom( 1e4 , size=9 , prob=samples )
sum( w==6 ) / 1e4

# 3M5: alter the prior:
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- ifelse( p_grid < 0.5 , 0 , 1 )
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
plot( posterior ~ p_grid , type="l" )
##
HPDI( samples , prob=0.9 )
w <- rbinom( 1e4 , size=15 , prob=samples )
simplehist(w)
