library(nimble)
library(dplyr)
library(readxl)
library(ggplot2)
library(basicMCMCplots)
set.seed(1)

setwd("/usr3/graduate/anshap/Thesis")

dat <- read_excel("updated_country_dat.xlsx") %>%
  filter(Country == "Cambodia",
         Year < 2020)

k <- 5.85
r <- 2.11
x0 = 2007

#2002
p0 <- 1-(1-(1/7.64))^(k/r)

#2011
p1 <- 1-(1-(1/5.07))^(k/r)

#make bottom 0 because that is what the equation likes
l <- p1-p0

dat <- dat %>%
  mutate(p = l/(1+exp(-.5*(Year-x0)))+ p0)

# fit logistic curve for M:F ratio

tmp <- dat %>%
  filter(Year < 2018)

plot(tmp$Year, tmp$p, xlab = "Year", ylab = "p")
title("(a)", adj = 0)

# remove notifications that were incident before study period
dat <- dat %>%
  mutate(adj = pgamma(row_number(), k, rate = r),
         Notifs_m = round(Notifs_m*adj))

#====================================================================
# functions
#====================================================================

f_vec <- nimbleFunction(
  run = function(x = double(0), 
                 p = double(0)){
    
    k <- 5.85
    r <- 2.11

    tmp_vec <- rep(NA, x)
    
    for (i in 1:x) {
      tmp_vec[i] <- p*(pgamma(x-(i-1), k, rate = r) - pgamma(x-i, k, rate = r))
    }
    
    return(tmp_vec)
    returnType(double(1))
  })


# code penalty as prior
dpen <- nimbleFunction(
  run = function(x = double(1), 
                 theta = double(0), L = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    logProb <- -(theta/2)*sum((log(x[1:(L-2)]) - 2*log(x[2:(L-1)]) + log(x[3:L]))^2)
    if(log) return(logProb)
    else return(exp(logProb))
  })
#====================================================================
# set up MCMC
#====================================================================
modelCode <- nimbleCode({
  
  for (i in 1:L){
    #get mu 
    mu[i] <- sum(f_vec(i, p[i])*lambda[1:i])
    
    x[i] ~ dpois(mu[i])
  }
  
  #priors
  lambda[1:L] ~ dpen(theta = theta, L = L)
})




modelConsts <- list(L = nrow(dat), 
                    p = dat$p, 
                    theta = mean(dat$Notifs_m))

modelData <- list(x = dat$Notifs_m)

initsList <- list(lambda = abs(rnorm(nrow(dat), mean(dat$Notifs_m), sd(dat$Notifs_m))))


model <- nimbleModel(modelCode, data = modelData,  inits = initsList, constants = modelConsts) #i
model$initializeInfo()
model$calculate() 


#====================================================================
# Compile and run model
#====================================================================

# compile into nimble
Cmodel <- compileNimble(model)
# Cmodel$initializeInfo()

#run MCMC
# another invocation
modelConf <- configureMCMC(Cmodel, enableWAIC = TRUE,  
                           monitors = c("lambda")) #, "theta"

modelMCMC <- buildMCMC(modelConf)
CmodelMCMC <- compileNimble(modelMCMC)

initsList <- list(list(lambda = abs(rnorm(nrow(dat), mean(dat$Notifs_m), sd(dat$Notifs_m)))),
                  list(lambda = abs(rnorm(nrow(dat), mean(dat$Notifs_m), sd(dat$Notifs_m)))),
                  list(lambda = abs(rnorm(nrow(dat), mean(dat$Notifs_m), sd(dat$Notifs_m)))))

niter <- 10000000
mcmc.out <- runMCMC(CmodelMCMC, inits = initsList,
                    niter = niter, nchains = 3, nburnin = 4000000, thin = 10,
                    summary = T, WAIC = T)

save(mcmc.out, file = "project1_mcmc/cure_models/remove_extra_notifs/by_sex/output/cam_m.RData")
