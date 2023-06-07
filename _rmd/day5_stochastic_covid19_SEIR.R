########################################
############ DAY 5
############ SARS-CoV2 Final project
########################################

#if (!require("drat")) install.packages("drat")
#drat:::add("mrc-ide")
#install.packages("dde")
#install.packages("odin")

library(odin)
library(ggplot2)
library(reshape2)
library(dplyr)
library(here)


seir_generator <- odin::odin({
  ## Core equations for transitions between compartments:
  update(S) <- S - n_SE + n_RS + n_bd
  update(E) <- E - n_EI + n_SE
  update(I) <- I + n_EI - n_IR
  update(R) <- R + n_IR - n_RS
  
  output(cases) <- n_EI * psymp
  ## Individual probabilities of transition:
  p_SE <- 1 - exp(-beta * I / N) # S to I
  p_EI <- 1 - exp(-omega)
  p_IR <- 1 - exp(-gamma)        # I to R
  p_RS <- 1 - exp(-delta)        # R to S
  
  ## Draws from binomial distributions for numbers changing between
  ## compartments:
  n_SE <- rbinom(S, p_SE)
  n_EI <- rbinom(E, p_EI)
  n_IR <- rbinom(I, p_IR)
  n_RS <- rbinom(R, p_RS)  
  n_bd <- N*birth_rate/365
  ## Total population size
  N <- S + E + I + R
  
  # Define beta in R0 terms
  beta <- R0*omega

  
  ## Initial states:
  initial(S) <- S_ini
  initial(E) <- E_ini
  initial(I) <- I_ini
  initial(R) <- 0
  
  ## User defined parameters - default in parentheses:
  S_ini <- user(1000)
  E_ini <- user(1)
  I_ini <- user(0)
  R0    <- user(2)
  gamma <- user(0.1)
  delta <- user(0.08)
  omega <- user(0.2)
  birth_rate <- user(0.0137)
  psymp <- user(0.5)
  
}, verbose = FALSE)



# Seed for random numbers
set.seed(1)
N<-100000
# Call SEIR object

seir <- seir_generator$new(
  S_ini=N-round(0.001*N), 
  E_ini = round(0.001*N), 
  R0=2.2, 
  gamma=1/5.1, 
  delta=1/365, 
  omega=1/3, 
  psymp=0.5)




# Multiple runs (100)
t_end<- 365 * 2 # sim time (2 years)
Y_limit <- sqrt(N) # define Ncrit


seir_100 <- seir$run(0:t_end, replicate = 100)
mean <- rowMeans(seir_100[, 6,])#apply(seir_100[, 4,],1,median,na.rm=T)
matplot(seir_100[, 1,],seir_100[, 6,], xlab = "Days", ylab = "Number of cases",
        type = "l", lty = 1, col="grey")
lines(seir_100[, 1,1],mean,col="purple")
lines(c(0,t_end),c(Y_limit,Y_limit),type="l", col="red") 
legend("topright", lwd = 1, col = "grey", legend = c("I"), bty = "n")

# 
# df<-data.frame(day=1:30,cases=round(rowMeans(seir_100[1:30,"cases",])))
# write.csv(df,here("data","daily_cases_countryX.csv") )



## Load case data 
case_data<-read.csv(here("data","daily_cases_countryX.csv"))


### Plot cases
mean <- rowMeans(seir_100[,"cases",])
matplot(seir_100[, "step",],seir_100[, "cases",], xlab = "Days", ylab = "Number of cases",
        type = "l", lty = 1, col="grey")
lines(seir_100[, "step",1],mean,col="purple")
points(case_data$day,case_data$cases,type = "p",col="black", pch=17)
legend("topright", lwd = c(1,NA), col = c("Purple","black"), legend = c("Mean cases","Data"), lty=c(1,NA),pch=c(NA,17),bty = "n")


###################
## Lockdowns
#####################

# Create lockdown function
get_lockdown<-function(x,reduction,start_lockdown, duration){
  y<-x*0 + 1
  for (ii in 1:length(start_lockdown)){
    
    y[start_lockdown[ii]:(start_lockdown[ii]+duration)]<-1-reduction
    
  }
  return(y)
}



## new model with lockdown

seir_generator2 <- odin::odin({
  ## Core equations for transitions between compartments:
  update(S) <- S - n_SE + n_RS + n_bd
  update(E) <- E - n_EI + n_SE
  update(I) <- I + n_EI - n_IR
  update(R) <- R + n_IR - n_RS
  
  output(cases) <- n_EI * psymp
  ## Individual probabilities of transition:
  p_SE <- 1 - exp(-beta * I / N) # S to I
  p_EI <- 1 - exp(-omega)
  p_IR <- 1 - exp(-gamma)        # I to R
  p_RS <- 1 - exp(-delta)        # R to S
  
  ## Draws from binomial distributions for numbers changing between
  ## compartments:
  n_SE <- rbinom(S, p_SE)
  n_EI <- rbinom(E, p_EI)
  n_IR <- rbinom(I, p_IR)
  n_RS <- rbinom(R, p_RS)  
  n_bd <- N*birth_rate/365
  ## Total population size
  N <- S + E + I + R
  
  # Lockdown
  lock_effect <- interpolate(lock_effect_t, lock_effect_y, "constant")
  # Define beta in R0 terms
  beta <- R0*omega*lock_effect
  
  
  ## Initial states:
  initial(S) <- S_ini
  initial(E) <- E_ini
  initial(I) <- I_ini
  initial(R) <- 0
  
  ## User defined parameters - default in parentheses:
  S_ini <- user(1000)
  E_ini <- user(1)
  I_ini <- user(0)
  R0    <- user(2)
  gamma <- user(0.1)
  delta <- user(0.08)
  omega <- user(0.2)
  birth_rate <- user(0.0137)
  psymp <- user(0.5)
  lock_effect_t[]   <- user()
  lock_effect_y[]   <- user()
  
  
  # Define dimensions
  dim(lock_effect_t)  <-user()
  dim(lock_effect_y)  <-user()
  
}, verbose = FALSE)


# Create a lockdown 
effect<-0 # Reduction in transmission given lockdown
starts<- c(30,150,270,390,510) # starting dates of each lock down (day)
duration<-60 # duration in days of each lockdown

x <- seq(1:t_end)
yy<- get_lockdown(x,reduction=effect,starts,duration)
plot(x,yy,type="l")



# Seed for random numbers
set.seed(1)
N<-100000
# Call SEIR object

seir <- seir_generator2$new(
  S_ini=N-round(0.001*N), 
  E_ini = round(0.001*N), 
  R0=2.2, 
  gamma=1/5.1, 
  delta=1/365, 
  omega=1/3, 
  psymp=0.5,
  lock_effect_t=x,
  lock_effect_y=yy)


seir_100 <- seir$run(1:t_end, replicate = 100)
### Plot cases
mean_cases <- rowMeans(seir_100[,"cases",])
matplot(seir_100[, "step",],seir_100[, "cases",], xlab = "Days", ylab = "Number of cases",
        type = "l", lty = 1, col="grey")
lines(seir_100[, "step",1],mean_cases,col="purple")
points(case_data$day,case_data$cases,type = "p",col="black", pch=17)
legend("topright", lwd = c(1,NA), col = c("Purple","black"), legend = c("Mean cases","Data"), lty=c(1,NA),pch=c(NA,17),bty = "n")
par(new = TRUE)
plot(x, yy, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(side=4, at = pretty(range(yy)))
mtext("z", side=4, line=3)


## effect 

cumcases<-cumsum(mean_cases) 
cumcases[730]
