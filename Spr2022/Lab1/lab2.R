install.packages("rjags")
install.packages("runjags")

library(rjags)

# invent some coin flip data
jags_data <- list(
  y = c(0, 1, 0, 0, 0, 1, 1, 0, 0)
)
jags_data$N <- length(jags_data$y)

jags_model <- jags.model(file = "bernbeta.txt",
                         data = jags_data,
                         n.chains = 3,
                         n.adapt = 500)

# burn-in
update(jags_model, n.iter = 500)

# sampling
coda_samples <- coda.samples(jags_model, variable.names =  c("theta"),
                             n.iter = 1000)


#### STAN

install.packages("rstan", repos = "https://cloud.r-project.org/",
                 dependencies = TRUE)


pkgbuild::has_build_tools(debug = TRUE)


library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

schools_dat <- list(J = 8,
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))
fit <- stan(file = "8schools.stan", data = schools_dat)


#### STAN

mod_bern <- stan_model("bernbeta.stan")
mod_bern

fit_bern <- sampling(mod_bern, data = jags_data)

print(fit_bern)

stan_plot(fit_bern, show_density = TRUE)
