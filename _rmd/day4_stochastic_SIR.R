########################################
############ DAY 4
############ PRACTICAL 1
############ Stochasticity 
########################################

#if (!require("drat")) install.packages("drat")
#drat:::add("mrc-ide")
#install.packages("dde")
#install.packages("odin")

library(odin)
library(ggplot2)
library(reshape2)

sir_generator <- odin::odin({
  ## Core equations for transitions between compartments:
  update(S) <- S - n_SI + n_RS
  update(I) <- I + n_SI - n_IR
  update(R) <- R + n_IR - n_RS
  
  ## Individual probabilities of transition:
  p_SI <- 1 - exp(-beta * I / N) # S to I
  p_IR <- 1 - exp(-gamma)        # I to R
  p_RS <- 1 - exp(-delta)        # R to S
  
  ## Draws from binomial distributions for numbers changing between
  ## compartments:
  n_SI <- rbinom(S, p_SI)
  n_IR <- rbinom(I, p_IR)
  n_RS <- rbinom(R, p_RS)  
  ## Total population size
  N <- S + I + R
  
  # Define beta in R0 terms
  beta <- R0*gamma
  
  ## Initial states:
  initial(S) <- S_ini
  initial(I) <- I_ini
  initial(R) <- 0
  
  ## User defined parameters - default in parentheses:
  S_ini <- user(1000)
  I_ini <- user(1)
  R0    <- user(2)
  gamma <- user(0.1)
  delta <- user(0.08)
  
}, verbose = FALSE)



sir <- sir_generator$new(I_ini = 1, R0=2, gamma=1/8, delta=1/90)
sir

set.seed(1)

t_end<- 365 * 2
res <- sir$run(0:t_end)

sir_col <- c("Navyblue", "orangered2", "darkgreen")


par(mar = c(4.1, 5.1, 0.5, 0.5), las = 1)
matplot(res[, 1], res[, -1], xlab = "Time", ylab = "Number of individuals",
        type = "l", col = sir_col, lty = 1)
lines(c(0,t_end),c(sqrt(1001),sqrt(1001)),type="l", col="red") 
legend("topright", lwd = 1, col = sir_col, legend = c("S", "I", "R"), bty = "n")


Y_limit <- sqrt(1001)

# Multiple runs
sir_100 <- sir$run(0:t_end, replicate = 100)
# res_200 <- sir$transform_variables(res_200)
# res_200 <- cbind.data.frame(t = res_200[[1]], res_200[-1])

matplot(sir_100[, 1,],sir_100[, 3,], xlab = "Days", ylab = "Number of infections",
        type = "l", lty = 1, col="grey")
lines(c(0,t_end),c(Y_limit,Y_limit),type="l", col="red") 
legend("topright", lwd = 1, col = "grey", legend = c("I"), bty = "n")


# Find probability of extinction of current model
prob_extinct<-function(results,t_end){
  
  n_extinct<-length(which(results[t_end,3,]==0))
  n_runs   <-length(results[1,1,])
  
  return(prob_extinction=n_extinct/n_runs) 
}

prob_extinct(sir_100,t_end)

