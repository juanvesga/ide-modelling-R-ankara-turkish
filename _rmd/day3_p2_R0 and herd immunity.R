########################################
############ DAY 3
############ PRACTICAL 2
############ R_0 and herd immunity 
########################################


# LOAD THE PACKAGES:
library(deSolve)
library(reshape2)
library(ggplot2)
library(here)

rm(list = ls())
setwd(here())
# Population size:
N <- 100000

# MODEL INPUTS:

# Initial conditions
initial_state_values <- c(
    S = N-500,    # <------------Write initial conditions for S
    E = 0,
    I = 500,    # <----------- Write initial conditions for I
    R = 0,
    M = 0)           

# Parameter values per day  <--------------Complete the following model parameters

  epsilon <- 1/5.8    # onset of symptoms rate  
  gamma   <- 1/13     # Recovery rate 
  R0      <- 2.79      # R0
  beta    <- R0/13       # infection rate
  CFR     <- 0.04       # Case fatality rate
  mu      <- gamma*CFR/(1-CFR)       # Mortality rate [remember competing hazards! gamma*CFR/(1-CFR) ]
  
  
  parameters <- c(beta,epsilon, gamma, mu)


# MODEL FUNCTION: 
times <- seq(from = 0, to = 365, by = 1)   # from 0 to 365 days in daily intervals

seir_model <- function(time, state, parameters) {  
  
  with(as.list(c(state, parameters)), {     
    
    # Calculating the total population size N (the sum of the number of people in each compartment)
    
    N <- S+E+I+R  # <----------------- define the total population
      
      # Calculating the FOI
      
      lambda <- beta*I/N  #  How can we define the force of infection? 
      
      # The differential equations
      dS <-  -lambda*S 
      dE <-   lambda*S - E*epsilon      # Can you write the differential equation for E? 
      dI <-   E*epsilon - I*(gamma+mu)     
      dR <-   gamma * I 
      dM <-   I*mu
      
      return(list(c(dS, dE, dI, dR, dM))) 
  })
  
}


# MODEL OUTPUT (solving the differential equations):

# Solving the differential equations using the ode integration algorithm
output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = seir_model,
                            parms = parameters))



# turn output data set into long format
output_long <- melt(as.data.frame(output), id = "time")                 


# Plot the number of people in the S, I and R compartments over time
ggplot(data = output_long,                                               # specify object containing data to plot
       aes(x = time, y = value, colour = variable, group = variable)) +  # assign columns to axes and groups
  geom_line(size=2) +                                                          # represent data as lines
  xlab("Time (days)")+                                                   # add label for x axis
  ylab("Number") +                                                       # add label for y axis
  labs(colour = "Compartment",                                           # add legend title  
       title = "COVID-19 (SEIR) model")    


########################################
############ 9.2 
########################################

# Plot the proportion of individuals 

output2_long<- output_long
output2_long$value<-output_long$value/N

# Plot the proportion of people in the S, I and R compartments over time
ggplot(data = output2_long,                                               # specify object containing data to plot
       aes(x = time, y = value, colour = variable, group = variable)) +  # assign columns to axes and groups
  geom_line(size=2) +                                                    # represent data as lines
  xlab("Time (days)")+                                                   # add label for x axis
  ylab("Number") +                                                       # add label for y axis
  labs(colour = "Compartment",                                           # add legend title  
       title = "COVID-19(SEIR) model")



## Calculate Reff

Reff<-output2_long[output2_long$variable=="S",] # this is the proportion susceptible over time

Reff$value<- Reff$value * R0 #<------------- Complete the calculation for Reff
  
  # Plot the proportion of people in the S, I and R compartments over time
  ggplot(data = Reff,                                               # specify object containing data to plot
         aes(x = time, y = value, colour = variable, group = variable)) +  # assign columns to axes and groups
  geom_line(size=2) +                                                          # represent data as lines
  xlab("Time (days)")+                                                   # add label for x axis
  ylab("Reff") +                                                       # add label for y axis
  labs(colour = "Compartment",                                           # add legend title  
       title = "COVID-19(SEIR) model")


