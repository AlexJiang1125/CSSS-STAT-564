# beta regression
rm(list = ls())
library(rjags)
library(ggplot2)
library(rethinking)
linear_model_code <- "
  model{

   for(i in 1:n){
      # likelihood
      y[i] ~ dbeta(a[i], b[i])

      # conditional mean (logit link)
      logit(mu[i]) <- beta0 + beta1*x[i]

      # transform back to a and b
      a[i] <- mu[i] * phi
      b[i]  <- (1-mu[i]) * phi


      # compute partial derivative numerically
      ## plus 0.001
      logit(mup[i]) <- beta0 + beta1*(x[i]+0.001)
      ## minus 0.001
      logit(mum[i]) <- beta0 + beta1*(x[i]-0.001)
      ## difference
      apdx[i] <- ((mup[i] - mum[i])/(2*0.001))

      # posterior predictive
      ynew[i] ~ dbeta(a[i], b[i])
   }

    # priors
    beta0 ~ dnorm(0, 1)
    beta1 ~ dnorm(0, 1)
    phi ~ dunif(0, 500)
  }
"

# read data
d <- read.csv(file = "data/qog_jan16.csv")

# transform variables
d$wdi_mortinftot <- d$wdi_mortinftot/1000
d$epi_watsup <- d$epi_watsup - mean(d$epi_watsup)

# y and x for jags
y <- d$wdi_mortinftot
x <- d$epi_watsup

library(rjags)

# fit model, 4 chains
model <- jags.model(file = textConnection(linear_model_code),
                    data = list(x = x,
                                y = y,
                                n = length(y)), n.chains = 4)

# burn-in 1e3
update(model, n.iter = 1e3)

# sample betas + phi
betas <- coda.samples(model,
                      variable.names = c("beta0", "beta1", "phi"),
                      n.iter = 3e3)

# posterior mean + credible intervals
precis(as.data.frame(betas[[1]]), digits = 4, prob = .95)

# compare with betareg
breg <- betareg(wdi_mortinftot ~ epi_watsup, data = d)
summary(breg)

# check trace plot
plot(betas)

# effective sample size
effectiveSize(betas)

# Rhat
gelman.diag(betas)


# sample mu and ynew
ymu.samples <- jags.samples(model,
                            variable.names = c("mu", "ynew"),
                            n.iter = 1e3)

# add posterior mean and quantiles to data.frame
d$mu.mean  <-  apply(ymu.samples$mu,  1, mean)
d$mu.lwr   <-  apply(ymu.samples$mu, 1, quantile, .025)
d$mu.upr   <-  apply(ymu.samples$mu, 1, quantile, .975)
d$ynew.lwr <-  apply(ymu.samples$ynew,1, quantile, .025)
d$ynew.upr <-  apply(ymu.samples$ynew,1, quantile, .975)

# ggplot
ggplot(d, aes(y = wdi_mortinftot, x = epi_watsup))  +
  geom_point(col = alpha("black", 1), cex = .8) +
  geom_line(aes(y = mu.mean), col = "red") +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), fill = alpha("blue", .5)) +
  geom_ribbon(aes(ymin = ynew.lwr, ymax = ynew.upr), fill = alpha("red", .3))  +
  theme_bw()

# density overlay
dens(y, ylim = c(0, 30), adj = 1, col = "black")
for(i in 1:200){
  dens(ymu.samples$ynew[,i,], adj = 1, col = alpha("lightblue", alpha = 0.5), add = T)
}
dens(y, adj = 1, col = "black", add = T)


# apd
apdx <- coda.samples(model, "apdx", n.iter = 1e3)
apdx <- do.call("rbind", apdx)

library(gtools)
bb_weights <- rdirichlet(nrow(apdx), alpha = rep(1, ncol(apdx)))
apd <- apply(bb_weights*apdx, 1, sum)
apd <- apd*1000
precis(apd)
