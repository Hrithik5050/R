# Ques-1: Write an R program to generate a sequence of numbers from 1 to 20.
numbers<-1:20
print(numbers)

# Ques-2: Create a vector c = [10, 20, 30, 40, 50, 60] and write a program which returns the 
# maximum and minimum of this vector. 
c<-c(10,20,30,40,50,60)
c_maximum<-max(c)
c_mininmum<-min(c)
cat("Maximum value:",c_maximum," and Minimum value:",c_mininmum)

# Ques-3: Write an R program to plot a simple line graph of the sin function.
x<-seq(0,2*pi,length.out=100)
y<-sin(x)
#plot
plot(x,y,type='l',col='red',lwd=1,
     main="Plot of sine function",
     xlab="x_axis",ylab="y-axis")
grid()

# Ques-4: Write a program in R to find factorial of a number by taking input from user. Please 
# print error message if the input number is negative. 
factorial<-function(n){
  if(n==0)
  {
    return(1)
  }else{
    return(n*factorial(n-1))
  }
}
input_number<-as.integer(readline(prompt = "Enter a number:"))
if (input_number<0)
{
  print("Give a valid number")
}else{
  factorial_result=factorial(input_number)
  cat("Factorial:",factorial_result)
}
# Ques-5: Write an R program to calculate the mean of a given numeric vector.
vec<-c(10,20,30,40,50)
result<-mean(vec)
cat("Result:",result)

# Ques-6: Write a program to write first n terms of a Fibonacci sequence. You may take n as 
# an input from the user. 

fibonacci <- function(n) {
  if (n == 0) {
    return(0)  # Fibonacci(0) is 0
  } else if (n == 1) {
    return(1)  # Fibonacci(1) is 1
  } else {
    return(fibonacci(n - 1) + fibonacci(n - 2))  # Recursive case
  }
}

# Take input from user
number <- as.integer(readline(prompt = "Enter a number: "))

# Check for valid input
if (is.na(number) || number < 0) {
  print("Please enter a valid non-negative integer.")  # Error message for invalid input
} else {
  fibonacci_result <- fibonacci(number)  # Calculate Fibonacci
  cat("Fibonacci of", number, "is:", fibonacci_result, "\n")  # Print the result
}

# Ques-7: Write an R program to make a simple calculator which can add, subtract, multiply 
# and divide. 
n1<-as.integer(readline(prompt="Enter the first number:")) 
oper<-readline(prompt="Enter the operator(+,-,/,*)") 
n2<-as.integer(readline(prompt="Enter the second number:")) 
result <- switch(oper, 
                 "+" = n1 + n2, 
                 "-" = n1 - n2, 
                 "*" = n1 * n2, 
                 "/" = {
                   if (n2 == 0) {
                     return("Error: Division by zero is not allowed.")
                   } else {
                     n1 / n2
                   }
                 },
                 "Invalid operator"  # Default case for invalid operator
)
print(paste("Result:",n1,oper,n2,"=",result))

# Ques-8: Explore plot, pie, barplot etc. (the plotting options) which are built-in functions in 
# R.

x<-seq(0,2*pi,length.out=100) 
y<-sin(x) 
plot(x,y,type="l",col="blue",lwd=3,main="Sin Graph",xlab="x-axis",ylab="y-axis") 

data<-c(10,30,40,9,11) 
label<-c("A","B","C","D","E") 
pie(data,labels = label,main="Pie Chart") 

value<-c(10,30,15,40,20) 
type<-c("A","B","C","D","E") 
barplot(value,names.arg=type,main="Bar Graph",col="green")
