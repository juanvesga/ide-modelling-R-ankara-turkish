########################################
############ DAY 5
############ SARS-CoV2 Final project
########################################


library(odin)
library(ggplot2)
library(reshape2)
library(dplyr)
library(here)
#installed.packages("socialmixr") ## Uncomment and install this package
library(socialmixr) 


# Epidemic data 

data<-read.csv(here("_rmd","data","daily_GE_data.csv"))
data$t<-as.factor(data$Days)

# Barplot
ggplot(data, aes(x=t, y=Cases)) + 
  geom_bar(stat = "identity", width=0.2)+
  ylab("reported GE cases") +
  xlab("day")


## Create a NoV moodel 
seir_generator <- odin::odin({
  
  dt <- user(1)
  initial(time) <- 0
  update(time) <- (step + 1) * dt
  
  
  ## Core equations for overall transitions between compartments:
  update(V_tot) <- V_tot + sum(n_SV) + sum(n_RV) - sum(n_VS) - sum(n_VVA) 
  update(S_tot) <- S_tot + sum(n_VS) + sum(n_RS) - sum(n_SE) - sum(n_SV)  
  update(E_tot) <- E_tot + sum(n_SE)  - sum(n_EI) 
  update(I_tot) <- I_tot + sum(n_EI) - sum(n_IA) - sum(n_deathI)
  update(A_tot) <- A_tot + sum(n_IA) - sum(n_AR) - sum(n_deathA)
  update(VA_tot) <- VA_tot +  sum(n_VVA) - sum(n_VAR) - sum(n_deathVA)
  update(R_tot) <- R_tot + sum(n_AR) + sum(n_VAR) - sum(n_RS) - sum(n_RV) 
  
  ## Equations for transitions between compartments by age group
  update(V[]) <- V[i] + n_SV[i] + n_RV[i] - n_VS[i] - n_VVA[i] 
  update(S[]) <- S[i] + n_VS[i] + n_RS[i] - n_SE[i] - n_SV[i]    
  update(E[]) <- E[i] + n_SE[i] - n_EI[i] 
  update(I[]) <- I[i] + n_EI[i] - n_IA[i] - n_deathI[i]
  update(A[]) <- A[i] + n_IA[i] - n_AR[i] - n_deathA[i]
  update(VA[]) <- VA[i] + n_VVA[i] - n_VAR[i] - n_deathVA[i]
  update(R[]) <- R[i] + n_AR[i] + n_VAR[i] - n_RS[i] - n_RV[i]
  
  ## Model outputs
  update(cum_vaccines[]) <-cum_vaccines[i]+ n_SV[i] + n_RV[i]
  update(new_cases[]) <- n_EI[i] 
  update(cum_cases[]) <- cum_cases[i] + n_EI[i] 
  update(cum_deaths[]) <- cum_deaths[i] + n_deathI[i] + n_deathA[i] + n_deathVA[i]
  
  update(cum_vaccines_all) <- cum_vaccines_all+ sum(n_SV) + sum(n_RV)
  update(new_cases_all) <- sum(n_EI)
  update(cum_cases_all) <- cum_cases_all + sum(n_EI) 
  update(new_reported_all) <- sum(new_reports)
  update(cum_deaths_all) <- cum_deaths_all + sum(n_deathI) + sum(n_deathA) + sum(n_deathVA)
  
  new_reports[]<- n_EI[i] * 1/rep_ratio[i]
  dim(new_reports)<-N_age
  
  
  ## Individual probabilities of transition:
  p_VS[] <- 1 - exp(-delta * dt)  # V to S
  p_SE[] <- 1 - exp(-lambda[i] * dt) # S to E
  p_EI   <- 1 - exp(-epsilon * dt) # E to I
  p_IA   <- 1 - exp(-theta * dt) # I to A
  p_AR   <- 1 - exp(-sigma * dt) # A to R
  p_RS   <- 1 - exp(-tau * dt) # R to S
  p_vacc[] <- 1 - exp(-vac_imm*vac_cov[i] * dt)# vaccination
  p_noromu[]<- 1 - exp(- ((p_IA+p_AR) * cfr[i]/(1-cfr[i])) * dt)# vaccination
  
  ## Force of infection
  m[, ] <- user() # age-structured contact matrix
  s_ij[, ] <- m[i, j] * (I[j] + (A[j] * rho )+ (A[j] * rho * vac_eff) )
  lambda[] <- beta * sum(s_ij[i, ])
  
  ## Draws from binomial distributions for numbers changing between
  ## compartments:
  
  # Flowing out of V 
  n_VS[] <- rbinom(V[i],  p_VS[i])
  n_VVA[] <- rbinom(V[i]-n_VS[i], p_SE[i] * (1-vac_eff))
  
  # Flowing out of S
  n_SE[] <- rbinom(S[i], p_SE[i])
  n_SV[] <- rbinom(S[i]-n_SE[i], if (step > t_vacc) p_vacc[i] else 0)
  
  # Flowing out of E
  n_EI[] <- rbinom(E[i], p_EI)
  
  # Flowing out of I
  n_IA[] <- rbinom(I[i], p_IA)
  n_deathI[]<-rbinom(I[i] - n_IA[i], p_noromu[i])
  
  # Flowing out of A
  n_AR[] <- rbinom(A[i], p_AR)
  n_deathA[]<-rbinom(A[i] - n_AR[i], p_noromu[i])
  
  # Flowing out of VA
  n_VAR[] <- rbinom(VA[i], p_AR)
  n_deathVA[]<-rbinom(VA[i] - n_VAR[i], p_noromu[i]* (1-vac_eff))
  
  # Flowing out of R
  n_RS[] <- rbinom(R[i], p_RS)
  n_RV[] <- rbinom(R[i] - n_RS[i], if (step > t_vacc) p_vacc[i] else 0)
  
  ## Initial states:
  initial(V_tot) <- sum(V_ini)
  initial(S_tot) <- sum(S_ini)
  initial(E_tot) <- sum(E_ini)
  initial(I_tot) <- sum(I_ini)
  initial(A_tot) <- sum(A_ini)
  initial(VA_tot) <- sum(VA_ini)
  initial(R_tot) <- sum(R_ini)
  
  initial(V[]) <- V_ini[i]
  initial(S[]) <- S_ini[i]
  initial(E[]) <- E_ini[i]
  initial(I[]) <- I_ini[i]
  initial(A[]) <- A_ini[i]
  initial(VA[]) <- VA_ini[i]
  initial(R[]) <- R_ini[i]
  initial(cum_vaccines[]) <- 0
  initial(new_cases[]) <- 0
  initial(cum_cases[]) <- 0
  initial(cum_deaths[]) <- 0
  initial(cum_vaccines_all) <- 0
  initial(new_cases_all) <- 0
  initial(cum_cases_all) <- 0
  initial(new_reported_all)<-0
  initial(cum_deaths_all) <- 0
  
  
  ## User defined parameters - default in parentheses:
  V_ini[] <- user()
  S_ini[] <- user()
  E_ini[] <- user()
  I_ini[] <- user()
  A_ini[] <- user()
  VA_ini[] <- user()
  R_ini[] <- user()
  
  # Model parameters (values in brackets are default values) 
  beta <- user(0.003)   # transm coefficient
  delta <- user( 1/(365))  # vaccine immunity dur (~5 yras)
  epsilon <- user(1)   # incubation
  theta <- user(0.5)   # duration symptoms
  sigma <- user(0.066) # duration asymp shedding
  tau   <- user(1/365)     # duration immunity
  rho   <- user(0.05) # rel infect asymptomatic 
  cfr[]  <- user() # Noro CFR by age 
  vac_eff<-user(0.9) # Vaccine efficacy for transmission
  vac_cov[]<-user() # vaccine coverage by age group
  vac_imm  <- user(1/5)# time to vaccine seroconversion (days)
  t_vacc   <- user(2) # days after case 0 to intro vaccine
  rep_ratio[]  <-user() # cases in community per case reported 
  # dimensions of arrays
  N_age <- user()
  dim(V_ini) <- N_age
  dim(S_ini) <- N_age
  dim(E_ini) <- N_age
  dim(I_ini) <- N_age
  dim(A_ini) <- N_age
  dim(VA_ini) <- N_age
  dim(R_ini) <- N_age
  dim(vac_cov)<-N_age
  dim(cfr)  <- N_age  
  dim(rep_ratio)  <- N_age  
  dim(V) <- N_age
  dim(S) <- N_age
  dim(E) <- N_age
  dim(I) <- N_age
  dim(A) <- N_age
  dim(VA) <- N_age
  dim(R) <- N_age
  dim(cum_vaccines) <- N_age
  dim(new_cases) <- N_age
  dim(cum_cases) <- N_age
  dim(cum_deaths) <- N_age
  dim(n_SV) <- N_age
  dim(n_RV) <- N_age
  dim(n_VS) <- N_age
  dim(n_VVA) <- N_age
  dim(n_VAR) <- N_age
  dim(n_SE) <- N_age
  dim(n_EI) <- N_age
  dim(n_IA) <- N_age
  dim(n_AR) <- N_age
  dim(n_RS) <- N_age
  dim(n_deathI)<-N_age
  dim(n_deathA)<-N_age
  dim(n_deathVA)<-N_age
  dim(p_VS) <- N_age
  dim(p_SE) <- N_age
  dim(p_vacc)<-N_age
  dim(p_noromu)<-N_age
  dim(m) <- c(N_age, N_age)
  dim(s_ij) <- c(N_age, N_age)
  dim(lambda) <- N_age
}, verbose = FALSE)

# Seed for random numbers
set.seed(1)

# Define total population
N<-68000

## Inter-age contact rates
ages   <-  c(5,15,65,75) 
age.categories <- as.factor(ages)
n_age<-length(ages)

# Load population 
pop_all <- read.csv(here("_rmd","data","population_TUR.csv"), header=TRUE)#, sep=,)
pop_all <- as.numeric(pop_all)
pop_distr<-pop_all/sum(pop_all) # Population age distribution
pop <- round(N * pop_distr)
# Load contact matrix 
cmat<-read.csv(here("_rmd","data","contact_TUR.csv",sep=""))
cmat_sym<-((cmat+t(cmat))/2)

# Find per-capita contact rate to input into transmission formula
transmission <- as.matrix(cmat_sym )/
  rep(c(t(pop)), each = ncol(cmat_sym))

# Noro CFR (see Lindsay et al https://bmcinfectdis.biomedcentral.com/articles/10.1186/s12879-015-1168-5)
mu<-c(
  0.04,
  0.01,
  0.03,
  0.63)/1000

# Reported ratios: cases in the commmunity per case reported in hospital outbreak

rep_ratio<-c(
  40,
  65,
  30,
  15)

# Call SEIR object

seir0 <- seir_generator$new(
  V_ini = c(1:n_age)*0,
  S_ini = as.numeric(round(N*pop_distr - c(1,0,0,0))),
  E_ini = c(1:n_age)*0,
  I_ini = c(1,0,0,0),
  A_ini = c(1:n_age)*0,
  VA_ini = c(1:n_age)*0,
  R_ini = c(1:n_age)*0,
  N_age = n_age,
  cfr= mu,
  m=transmission,
  rep_ratio=rep_ratio,
  vac_cov= c(0,0,0,0),
  beta = 0.06
)


# Multiple runs (100)
t_end<- 365 * 2 # sim time (2 years)

# Run the model
seir0_100 <- seir0$run(0:t_end, replicate = 100)

# Variables index
idx<-rownames(seir0_100[1,,])

# reported cases vs data
t_id<-which(idx=="time")
id<- which(idx=="new_reported_all")
mean <- rowMeans(seir0_100[, id,])
matplot(seir0_100[, t_id,],seir0_100[, id,], 
        xlab = "Days", 
        ylab = "Number of GE reported cases",
        type = "l", lty = 1, col="grey",
        xlim=c(0,30),
        ylim=c(0,max(data$Cases)*1.2))
lines(seir0_100[, 1,1],mean,col="purple")
points(data$Days+5, data$Cases, col = "red", pch = 19)

# Community incidence
id<- which(idx=="new_cases_all")
mean <- rowMeans(seir0_100[, id,])
matplot(seir0_100[, t_id,],seir0_100[, id,], 
        xlab = "Days", 
        ylab = "Number of cases",
        type = "l", lty = 1, col="grey",
        xlim=c(0,30))
lines(seir0_100[, 1,1],mean,col="purple")


# Cumulative NoV deaths
id<- which(idx=="cum_deaths_all")
mean <- rowMeans(seir0_100[, id,])
matplot(seir0_100[, t_id,],seir0_100[, id,], 
        xlab = "Days", 
        ylab = "cumulative of deaths",
        type = "l", lty = 1, col="grey",
        xlim=c(0,365))
lines(seir0_100[, 1,1],mean,col="purple")


# Stacked deaths by age 

time <- (seir0_100[, t_id,1])
t<-rep(time,4)
age <-  c(rep(c("0_4") , length(time)), 
          rep(c("5_14") , length(time)),
          rep(c("15_64") , length(time)), 
          rep(c("65+") , length(time)))
deaths <- c( rowMeans(seir0_100[,which(idx=="cum_deaths[1]") ,]),
             rowMeans(seir0_100[,which(idx=="cum_deaths[2]") ,]),
             rowMeans(seir0_100[,which(idx=="cum_deaths[3]") ,]),
             rowMeans(seir0_100[,which(idx=="cum_deaths[4]") ,]))
df <- data.frame(t,age,deaths)

# Stacked cumulative deaths over time by age
ggplot(df, aes(fill=age, y=deaths, x=t)) + 
  geom_bar(position="stack", stat="identity")+
  xlab("days")+ ylab("Cumulative Deaths")


# Relative stacked cumulative deaths over time by age
ggplot(df, aes(fill=age, y=deaths, x=t)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("days")+ ylab("Proportion of Cumulative Deaths")

# cases by age 
time <- (seir0_100[, t_id,1])
t<-rep(time,4)
age <-  c(rep(c("0_4") , length(time)), 
          rep(c("5_14") , length(time)),
          rep(c("15_64") , length(time)), 
          rep(c("65+") , length(time)))
cases <- c( 1000*(rowMeans(seir0_100[,which(idx=="cum_cases[1]") ,])/pop[1]),
            1000*( rowMeans(seir0_100[,which(idx=="cum_cases[2]") ,])/pop[2]),
            1000*(rowMeans(seir0_100[,which(idx=="cum_cases[3]") ,])/pop[3]),
            1000*(rowMeans(seir0_100[,which(idx=="cum_cases[4]") ,]))/pop[4])
df <- data.frame(t,age,cases)

ggplot(df,aes(color=age, y=cases, x=t)) + 
  geom_line() +
  xlab("days")+ ylab("cumulative incidence rate per 1000 population")






# Analysis output
id<- which(idx=="cum_deaths_all")
base_deaths <- mean(seir0_100[365*2 ,id, ])

id<- which(idx=="cum_cases_all")
base_cases <- mean(seir0_100[365*2 ,id, ])

id<- which(idx=="cum_vaccines_all")
base_doses <- mean(seir0_100[365*2 ,id, ])




## =========== Vaccine scenarios

# Vacc  1: Eff = 0.7 , seroconversion time of 5 days 
# Vacc  2: Eff = 0.95 , seroconversion time of 12 days 
# Change coverage by age, immunogenicity and vaccine efficacy  

## Scenario 1

seir1 <- seir_generator$new(
  V_ini = c(1:n_age)*0,
  S_ini = as.numeric(round(N*pop_distr - c(1,0,0,0))),
  E_ini = c(1:n_age)*0,
  I_ini = c(1,0,0,0),
  A_ini = c(1:n_age)*0,
  VA_ini = c(1:n_age)*0,
  R_ini = c(1:n_age)*0,
  N_age = n_age,
  cfr= mu,
  m=transmission,
  rep_ratio=rep_ratio,
  vac_cov= c(0.2,0.3,0.3,0.3),
  vac_eff = 0.7,
  vac_imm = 1/5,
  beta = 0.06
)

# Run the model
seir1_100 <- seir1$run(0:t_end, replicate = 100)

# Analysis output
id<- which(idx=="cum_deaths_all")
sc1_deaths <- mean(seir1_100[365*2 ,id, ])

id<- which(idx=="cum_cases_all")
sc1_cases <- mean(seir1_100[365*2 ,id, ])

id<- which(idx=="cum_vaccines_all")
sc1_doses <- mean(seir1_100[365*2 ,id, ])

sc1_avdeaths<-base_deaths-sc1_deaths
sc1_avcases<-base_cases-sc1_cases

dose_per_avdeath1<- sc1_doses/sc1_avdeaths
dose_per_avcase1<- sc1_doses/sc1_avcases


## Scenario 1

seir2 <- seir_generator$new(
  V_ini = c(1:n_age)*0,
  S_ini = as.numeric(round(N*pop_distr - c(1,0,0,0))),
  E_ini = c(1:n_age)*0,
  I_ini = c(1,0,0,0),
  A_ini = c(1:n_age)*0,
  VA_ini = c(1:n_age)*0,
  R_ini = c(1:n_age)*0,
  N_age = n_age,
  cfr= mu,
  m=transmission,
  rep_ratio=rep_ratio,
  vac_cov= c(0.2,0.3,0.3,0.3),
  vac_eff = 0.9,
  vac_imm = 1/12,
  beta = 0.06
)

# Run the model
seir2_100 <- seir2$run(0:t_end, replicate = 100)



# Analysis output
id<- which(idx=="cum_deaths_all")
sc2_deaths <- mean(seir2_100[365*2 ,id, ])

id<- which(idx=="cum_cases_all")
sc2_cases <- mean(seir2_100[365*2 ,id, ])

id<- which(idx=="cum_vaccines_all")
sc2_doses <- mean(seir2_100[365*2 ,id, ])

sc2_avdeaths<-base_deaths-sc2_deaths
sc2_avcases<-base_cases-sc2_cases

dose_per_avdeath2<- sc2_doses/sc2_avdeaths
dose_per_avcase2<- sc2_doses/sc2_avcases












# reported cases vs data
t_id<-which(idx=="time")
id<- which(idx=="new_reported_all")
mean <- rowMeans(seir1_100[, id,])#apply(seir_100[, 4,],1,median,na.rm=T)
matplot(seir1_100[, t_id,],seir1_100[, id,], 
        xlab = "Days", 
        ylab = "Number of GE reported cases",
        type = "l", lty = 1, col="grey",
        xlim=c(0,30),
        ylim=c(0,max(data$Cases)*1.2))
lines(seir1_100[, 1,1],mean,col="purple")
points(data$Days+5, data$Cases, col = "red", pch = 19)


# No. vaccine doses
t_id<-which(idx=="time")
id<- which(idx=="cum_vaccines_all")
mean <- rowMeans(seir1_100[, id,])
matplot(seir1_100[, t_id,],seir1_100[, id,], 
        xlab = "Days", 
        ylab = "Number of GE reported cases",
        type = "l", lty = 1, col="grey",
        xlim=c(0,365*2))
lines(seir1_100[, 1,1],mean,col="purple")


# Cumulative NoV deaths
id<- which(idx=="cum_deaths_all")
mean <- rowMeans(seir1_100[, id,])
matplot(seir1_100[, t_id,],seir1_100[, id,], 
        xlab = "Days", 
        ylab = "cumulative of deaths",
        type = "l", lty = 1, col="grey",
        xlim=c(0,365))
lines(seir1_100[, 1,1],mean,col="purple")





















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
