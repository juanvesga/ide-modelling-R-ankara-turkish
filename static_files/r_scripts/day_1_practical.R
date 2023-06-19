# This is a first practical


vector_double <- c(1, 2, 3, 4, 5, 6)  # this is a vector of integers

vector_logic <- c(TRUE, FALSE, FALSE, TRUE)

vector_character<- c("A", "B", "C", "D")

vector_integer <- c(1L, 2L, 3L, 4L)

this_is_a_copy <- vector_double

vector_double

mat1<-matrix(vector_double, nrow=3)

mat2<-matrix(2,nrow=2, ncol=3)

mat2


#use transposition function t
t(mat1)
mat3<- t(mat1) * mat2
mat3

# Data Frames 

# data_example <- data.frame(
#   vector_character,
#   vector_double,
#   vector_logic,
#   vector_integer)

length(vector_character)
length(vector_double)
length(vector_logic)
length(vector_integer)

# Chopping the vector 
vector_double_short<-vector_double[1:length(vector_character)]
vector_double_chopped<-vector_double[-c(5,6)]

data_example <- data.frame(
  vector_character, 
  vector_double_short, 
  vector_logic, 
  vector_integer)


rm(mat1,mat2,mat3)

# Load Data

?read.csv

data <- read.csv("data/data_day1.csv", sep =";")
View(data)

data$weigh_kg

mean(data$weigh_kg)
sd(data$weigh_kg)
hist(data$weigh_kg)

# Functions
x<-2
power_function <- function(base,power){ # function definition and arguments
  
  #body
  x<-base^power
  z<-x
  
  #output
  return(x)
}

power_function(2,3)

power_function(2,3)
power_function(3,3)
power_function(10,3)
power_function(2,3)
power_function(2,2)
power_function(2,3)



# Body mass index
bmi_calculator<- function(weight_kg,height){ 

  if (height > 3) {
    height_mt<- height/100
  } else {
    height_mt<-height 
  }
  
  
bmi<- weight_kg/(height_mt^2)

#output

return(bmi)

}

bmi_calculator(data$weigh_kg[1],data$height_cm[1])

bmi_calculator(data$weigh_kg[1],1.70)


getwd()

































