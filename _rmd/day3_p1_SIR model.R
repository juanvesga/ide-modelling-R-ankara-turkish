########################################
############ DAY 3
############ PRACTICAL 1
############ The SIR model
########################################


# LOAD THE PACKAGES:
library(deSolve)
library(reshape2)
library(ggplot2)
library(here)

rm(list = ls())
setwd(here())
# Population size:
N <- 1000

# MODEL INPUTS:

# Initial conditions
initial_state_values <- c(
  S = N-1,
  I = 1,
  R = 0,
  M = 0)           

# Parameter values per day
  CFR<- 0.15
   
  gamma <- 1/6 # Recovery rate 
  
  beta <- 0.5  # infection rate
  
  mu <- gamma*CFR/(1-CFR)   # Mortality rate [remember competing hazards! gamma*CFR/(1-CFR) ]
  
  parameters <- c(beta, gamma, mu)


# MODEL FUNCTION: 
times <- seq(from = 0, to = 150, by = 1)   # from 0 to 365 days in daily intervals

sir_model <- function(time, state, parameters) {  
  
  with(as.list(c(state, parameters)), {     
    
    # Calculating the total population size N (the sum of the number of people in each compartment)
    
    N <- S+I+R
    
    # Calculating the FOI
    
    lambda <- beta*I/N  #  How can we define the force of infection? 
      
      # The differential equations
      dS <-  - lambda * S # can you write the differential equation for S? 
      dI <- lambda * S - I*(gamma+mu)     
    dR <- gamma * I
    dM <- I*mu
    
    return(list(c(dS, dI, dR, dM))) 
  })
  
}



# MODEL OUTPUT (solving the differential equations):

# Solving the differential equations using the ode integration algorithm
output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_model,
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
       title = "SIR model")    



# Plot the proportion of individuals 

output2_long<- output_long
output2_long$value<-output_long$value/N


# Plot the proportion of people in the S, I and R compartments over time
ggplot(data = output2_long,                                               # specify object containing data to plot
       aes(x = time, y = value, colour = variable, group = variable)) +  # assign columns to axes and groups
  geom_line(size=2) +                                                          # represent data as lines
  xlab("Time (days)")+                                                   # add label for x axis
  ylab("Number") +                                                       # add label for y axis
  labs(colour = "Compartment",                                           # add legend title  
       title = "SIR model")   
