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
p0_m <- 1-(1-(1/7.64))^(k/r)
p0_f <- 1-(1-1/5.8)^(k/r)

#2011
p1_m <- 1-(1-(1/5.07))^(k/r)
p1_f <- 1-(1-1/4.14)^(k/r)

#make bottom 0 because that is what the equation likes
l_m <- p1_m-p0_m
l_f <- p1_f-p0_f

dat <- dat %>%
  mutate(p_m = l_m/(1+exp(-.5*(Year-x0)))+ p0_m,
         p_f = l_f/(1+exp(-.5*(Year-x0)))+ p0_f)

# plot(tmp$Year, tmp$p, xlab = "Year", ylab = "p")
# title("(a)", adj = 0)

# remove notifications that were incident before study period
dat <- dat %>%
  mutate(adj = pgamma(row_number(), k, rate = r),
         Notifs_m = round(Notifs_m*adj),
         Notifs_f = round(Notifs_f*adj))

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
    mu_m[i] <- sum(f_vec(i, p_m[i])*lambda_m[1:i])
    x_m[i] ~ dpois(mu_m[i])
    
    mu_f[i] <- sum(f_vec(i, p_f[i])*lambda_f[1:i])
    x_f[i] ~ dpois(mu_f[i])
    
    #get total and ratio
    lambda_tot[i] <-lambda_m[i] + lambda_f[i]
    ratio[i] <- lambda_m[i]/lambda_f[i]
  }
  
  #priors
  lambda_m[1:L] ~ dpen(theta = theta_m, L = L)
  lambda_f[1:L] ~ dpen(theta = theta_f, L = L)
})

modelConsts <- list(L = nrow(dat), 
                    p_m = dat$p_m, 
                    p_f = dat$p_f, 
                    theta_m = mean(dat$Notifs_m), 
                    theta_f = mean(dat$Notifs_f))

modelData <- list(x_m = dat$Notifs_m, 
                  x_f = dat$Notifs_f)

initsList <- list(lambda_m = abs(rnorm(nrow(dat), mean(dat$Notifs_m), sd(dat$Notifs_m))),
                  lambda_f = abs(rnorm(nrow(dat), mean(dat$Notifs_f), sd(dat$Notifs_f))))


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
                           monitors = c("lambda_f", "lambda_m", "lambda_tot", "ratio")) 

modelMCMC <- buildMCMC(modelConf)
CmodelMCMC <- compileNimble(modelMCMC)

initsList <- list(list(lambda_m = abs(rnorm(nrow(dat), mean(dat$Notifs_m), sd(dat$Notifs_m))), 
                       lambda_f = abs(rnorm(nrow(dat), mean(dat$Notifs_f), sd(dat$Notifs_f)))),
                  list(lambda_m = abs(rnorm(nrow(dat), mean(dat$Notifs_m), sd(dat$Notifs_m))), 
                       lambda_f = abs(rnorm(nrow(dat), mean(dat$Notifs_f), sd(dat$Notifs_f)))),
                  list(lambda_m = abs(rnorm(nrow(dat), mean(dat$Notifs_m), sd(dat$Notifs_m))), 
                       lambda_f = abs(rnorm(nrow(dat), mean(dat$Notifs_f), sd(dat$Notifs_f)))))

niter <- 10000000
mcmc.out <- runMCMC(CmodelMCMC, inits = initsList,
                    niter = niter, nchains = 3, nburnin = 4000000, thin = 10,
                    summary = T, WAIC = T)

save(mcmc.out, file = "project1_mcmc/cure_models/remove_extra_notifs/by_sex/output/cam_all.RData")
