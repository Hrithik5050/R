#Q1. The iris data-set is a built-in data-set in R that contains measurements on 4 
#different attributes (in centimeters) for 150 flowers from 3 different species. Load 
#this data-set and do the following: 
#(a) Print first few rows of this data-set. 
#(b) Find the structure of this data-set. 
#(c) Find the range of the data regarding the sepal length of flowers. 
#(d) Find the mean of the sepal length. 
#(e) Find the median of the sepal length. 
#(f) Find the first and the third quartiles and hence the interquartile range. 
#(g) Find the standard deviation and variance. 
#(h) Try doing the above exercises for sepal.width, petal.length and petal.width. 
#(i) Use the built-in function summary on the data-set Iris.

data(iris)
head(iris)
str(iris)
# Calculate the range of Sepal.Length
sepal_length_range <- range(iris$Sepal.Length)
# Display the range
print(sepal_length_range)
# Calculate the mean of Sepal.Length
mean_sepal_length <- mean(iris$Sepal.Length)
mean_sepal_length
# Calculate the median of Sepal.Length
median_sepal_length <- median(iris$Sepal.Length)
median_sepal_length
q1<-quantile(iris$Sepal.Length,0.25) 
print(q1) 
q3<-quantile(iris$Sepal.Length,0.75) 
print(q3)
IQR(iris$Sepal.Length)
sd(iris$Sepal.Length)
var(iris$Sepal.Length)
summary(iris)

#Q2. R does not have a standard in-built function to calculate mode. So we 
#create a user function to calculate mode of a data set in R. This function 
#takes the vector as input and gives the mode value as output.
# Function to calculate the mode
calculate_mode <- function(x) {
  # Remove NA values
  x <- na.omit(x)
  # Get the unique values and their frequencies
  unique_values <- unique(x)
  frequencies <- table(x)#table uniquely count the value of vector 
  # Find the maximum frequency
  max_freq <- max(frequencies)
  # Get the values that have the maximum frequency (mode)
  modes <- unique_values[frequencies == max_freq]
  return(modes)
}
# Example usage
data_vector <- c(1, 2, 2, 3, 4, 4, 4, 5, 5)
mode_value <- calculate_mode(data_vector)
# Display the mode
print(mode_value)

#Q3. A room has M people, and each has an equal chance of being born on any of 
#the 365 days of the year. (For simplicity, we will ignore leap years). What is the 
#probability that two people in the room have the same birthday ? 
#(a) Use an R simulation to estimate this for various M. 
#(b) Find the smallest value of M for which the probability of a match is greater 
#than 0.5. 
M<-50  #out of 5000 we consider 50 persons 
N<-5000 #number of simulation total no of persons exhaustive cases 
fav<-0  #favourable cases 
for(val in 1:N){ 
  a<-as.integer(any(duplicated(sample(365,M,replace=TRUE)))) 
  fav<-fav+a 
} 
prob<-fav/N 
print(prob) 
M <- 2 
prob <- 0  # Initialize probability

while (TRUE) { 
  prod <- 1 
  for (i in 0:(M - 1)) { 
    prod <- prod * (1 - i / 365) 
  }
  
  prob <- 1 - prod  # Probability of at least one match
  
  if (prob > 0.5) { 
    break  # Exit the loop if probability exceeds 0.5
  }
  
  M <- M + 1  # Increment M
} 

print(M)  # Print the smallest M for which prob > 0.5

##Q4. Write an R function for computing conditional probability. Call this function to 
#do the following problem: suppose the probability of the weather being cloudy is 
#40%. Also suppose the prob- ability of rain on a given day is 20% and that the 
#probability of clouds on a rainy day is 85%. If it’s cloudy outside on a given day, 
#what is the probability that it will rain that day?
cond_prob<-function(pR,pCR,pC){ #pR=rain, pCR= rain under cloudy, pC=cloudy 
  pRC<-(pR*pCR)/pC 
  return(pRC) 
} 
#Define probability 
pRain<-0.2 
pCloudy<-0.4 
pCloudyRain<-0.85 
#Use function file 
prob<-cond_prob(pRain,pCloudyRain,pCloudy) 
print(prob)

#Q5. Write an R function for computing conditional probability. Call this function to 
#do the following problem: Three urns I, II and III contain 8 red, 4 white; 6 red, 6 
#white; and 5 red, 7 white balls, respectively. If a ball is drawn at random and found 
#to be red, what is the probability that it is a drawn from (i) urn I, (ii) urn III? 

# Define the function for computing conditional probability 
cond_prob<-function(PA, PE_given_A, PE) { 
  PA_given_E <- (PA*PE_given_A)/PE 
  return(PA_given_E) 
} 
# Problem setup 
# Urn probabilities (assuming each urn is equally 
#likely to be chosen) 
P_urn_I<-1/3 
P_urn_II<-1/3 
P_urn_III<-1/3 
# Probability of drawing a red ball given the urn 
P_red_given_urn_I<-8/(8 + 4) 
P_red_given_urn_II<-6/(6 + 6) 
P_red_given_urn_III<-5/(5 + 7) 
# Total probability of drawing a red ball 
P_red <- (P_urn_I*P_red_given_urn_I) + 
  (P_urn_II*P_red_given_urn_II) + 
  (P_urn_III*P_red_given_urn_III) 
# Calculate the conditional probabilities 
P_urn_I_given_red <- cond_prob(P_urn_I, P_red_given_urn_I, P_red) 
P_urn_III_given_red <- cond_prob(P_urn_III, P_red_given_urn_III, P_red) 
# Print the results 
cat("The probability that the red ball was drawn from urn I is:", P_urn_I_given_red, 
    "\n") 
cat("The probability that the red ball was drawn from urn III is:", P_urn_III_given_red, 
    "\n")

#Q6. Write an R function for computing conditional probability. Call this function to 
#do the following problem: Companies A, B and C produces cars. The production 
#capacity of company A is twice that of B while company B and C produces same 
#number of cars in a given period. It is known that 2% of A, 3% of B and 4% of C 
#are defective. All the cars produced are put into one showroom and then one car 
#is chosen at random. 
#(a) Find the probability that the car is defective. 
#(b) Suppose a car chosen is defective, what is the probability that this is produced 
#by company A?
cond_prob<-function(P_A, PE_given_A, PE) { 
  PA_given_E <- (P_A*PE_given_A)/PE 
  return(PA_given_E) 
} 
P_A<-2/4 
P_B<-1/4 
P_C<-1/4 
P_defective_given_A<-0.02 
P_defective_given_B<-0.03 
P_defective_given_C<-0.04 
p_defective<-(P_A*P_defective_given_A)+ 
  (P_B*P_defective_given_B)+ 
  (P_C*P_defective_given_C) 
cat("Probability that car is defective: ",p_defective,"\n") 
P_A_given_defective<-cond_prob(P_A,P_defective_given_A,p_defective) 
cat("Probability that car is defective given car is A : ",P_A_given_defective,"\n")

#Q7. Write an R function for computing conditional probability. Call this function to 
#do the following problem: The LED bulbs producing factories A, B and C supply 
#LED bulbs to the market in the ratio 2:3:5. It is found that 1% of the items 
#produced in factory A, 2% of the items produced in factory B and 3% of the items 
#produced in factory C are defective. If a bulb is selected at random from the 
#market what is the probability that it is a defective one? Also, if a randomly 
#selected bulb is found to be defective then find the probability that it was produced 
#by factory (i) A, (ii) B, (iii) C? 

cond_prob<-function(P_A, PE_given_A, PE) { 
  PA_given_E <- (P_A*PE_given_A)/PE 
  return(PA_given_E) 
} 
P_A<-2/10 
P_B<-3/10 
P_C<-5/10 
P_defective_given_A<-0.01 
P_defective_given_B<-0.02 
P_defective_given_C<-0.03 
p_defective<-(P_A*P_defective_given_A)+ 
  (P_B*P_defective_given_B)+ 
  (P_C*P_defective_given_C) 
cat("Probability that car is defective: ",p_defective,"\n") 
P_A_given_defective<-cond_prob(P_A,P_defective_given_A,p_defective) 
cat("Probability that car is defective given car is A : ",P_A_given_defective,"\n") 
P_B_given_defective<-cond_prob(P_B,P_defective_given_B,p_defective) 
cat("Probability that car is defective given car is B : ",P_B_given_defective,"\n") 
P_C_given_defective<-cond_prob(P_C,P_defective_given_C,p_defective) 
cat("Probability that car is defective given car is C : ",P_C_given_defective,"\n")

