---
type: assignment
date: 2018-09-26T4:00:00+4:30
title: '2. Günün Çözümü 2. Uygulama: Bilgisayarla ilgili tehlikeler'
pdf: /static_files/assignments/05_day2P2_comp_hazards_sol.pdf
#attachment: /static_files/assignments/asg.zip
#solutions: /static_files/assignments/asg_solutions.pdf
due_event: 
    type: due
    date: 2018-11-13T23:59:00+3:30
    description: 'Assignment #1 due'
---

 
 
## 1. Competing hazards
 
Now that we have our model we can add complexity by examining further compartments and the case of competing hazards. In the next exercise we want to add a mortality compartment, that flows out of I, to describe the probability of death among the infected individuals.
 
For the next steps, imagine that we have gathered information which suggets that the CFR for the disease we are modelling is 30%. Using that information, and the concepts we have reviewed try to incorporate a new M compartment, specify a mortality rate rate mu and run the model.
 
Try and fill out the missing gaps in the script below:
 

{% highlight r %}
# Load some useful packages:
library(deSolve)  # To solve differential equations 
library(reshape2) # To To manipulate our model output
library(ggplot2)  # To produce nice plots!
library(here)     # To ensure that we always are in our working directory
 
 
 
## Add mortality to our cohort model 
 
# Population size:
N <- 1000
 
# MODEL INPUTS:
 
# Initial conditions
initial_state_values <- c(
  I = N,
  M = 0,
  R = 0)           
 
# Parameter values per day
 
# For a CFR of 30% , what is mu? 
 
mu= 0.5*0.3/(1-0.3) # gamma*CFR/(1-CFR)
 
parameters <- c(gamma = 1/2, mu)
 
# TIMESTEPS:
 
# Vector storing the sequence of timesteps to solve the model at
times <- seq(from = 0, to = 50, by = 1)   # from 0 to 365 days in daily intervals
 
# MODEL FUNCTION: 
 
# The model function takes as input arguments (in the following order): time, state and parameters
cohort_model2 <- function(time, state, parameters) {  
  
  with(as.list(c(state, parameters)), {     
    
    # Calculating the total population size N (the sum of the number of people in each compartment)
    N <- I+R
    
    # The differential equations
    dI <- -(gamma+mu) * I     
    dM <- I*mu
    dR <- gamma * I
    
    return(list(c(dI, dM, dR))) 
  })
  
}
 
# MODEL OUTPUT (solving the differential equations):
 
# Solving the differential equations using the ode integration algorithm
output2 <- as.data.frame(ode(y = initial_state_values, 
                             times = times, 
                             func = cohort_model2,
                             parms = parameters))
 
 
# turn output dataset into long format
output2_long <- melt(as.data.frame(output2), id = "time")                 
 
 
# Plot the new output
ggplot(data = output2_long,                                               
       aes(x = time, y = value, colour = variable, group = variable)) +  
  geom_line(size=2) +                                                          
  xlab("Time (days)")+                                                   
  ylab("Number") +                                                       
  labs(colour = "Compartment",                                          
       title = "Cohort model")    
{% endhighlight %}

![plot of chunk unnamed-chunk-1](../_images/unnamed-chunk-1-1.png)
 
### 2.1 Explore CFR from our simulation
 
We have our model now with added mortality, try and check if CFR as estimated from the model corresponds to CFR =30%
 

{% highlight r %}
# For that, we might want to see the proportion of people in each compartment
 
output3_long<- output2_long
output3_long$value<-output2_long$value/N
 
 
# Plot the proportion of people in the S, I and R compartments over time
ggplot(data = output3_long,                                               
       aes(x = time, y = value, colour = variable, group = variable)) +  
  geom_line(size=2) +                                                        
  xlab("Time (days)")+                                                  
  ylab("Number") +                                                      
  labs(colour = "Compartment",                                          
       title = "Cohort model")  
{% endhighlight %}

![plot of chunk unnamed-chunk-2](../_images/unnamed-chunk-2-1.png)

{% highlight r %}
# Can you tell from the graph what proportion of people have died as a result of the disease?
{% endhighlight %}
