library(nimble)
library(dplyr)
library(readxl)
library(ggplot2)
library(basicMCMCplots)
set.seed(1)

setwd("/usr3/graduate/anshap/Thesis")

## 10/5: implement penalized likelihood from Bacchetti 1993

## 12/7: use updated definition of p and logistic curve for p

dat <- read_excel("updated_country_dat.xlsx") %>% 
  filter(Country == "Philippines", Year <2020) 

#Phl: 
k <- 6.06
r <- 2.94
x0 <- 2012

#2007
p0 <- 1-(1-1/9.16)^(k/r)

#2016
p1 <- 1-(1-1/6.4)^(k/r)

#make bottom 0 because that is what the equation likes
l <- p0-p1

#slope 
# s <- (p1-p0)/(2016-2007)

dat <- dat %>%
  mutate(p = l/(1+exp(.5*(Year-x0)))+ p1)

# tmp <- dat %>%
#   filter(Year < 2018)
# 
# plot(tmp$Year, tmp$p, xlab = "Year", ylab = "p")
# title("(b)", adj = 0)

# remove Notifs_m that were incident before study period
dat <- dat %>%
  mutate(adj = pgamma(row_number(), k, rate = r),
         Notifs_m = round(Notifs_m*adj))

#====================================================================
# functions
#====================================================================

f_vec <- nimbleFunction(
  run = function(x = double(0), 
                 p = double(0)){
    
    # #Phl: 
    k <- 6.06
    r <- 2.94

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

model <- nimbleModel(modelCode, data = modelData,  inits = initsList, constants = modelConsts)
model$initializeInfo()
model$calculate() 

model$getNodeNames()
model$x
model$lambda
model$mu

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

save(mcmc.out, file = "project1_mcmc/cure_models/remove_extra_notifs/by_sex/output/phl_m.RData")
