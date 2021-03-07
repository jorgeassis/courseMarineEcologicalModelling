# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 1.1
# The main objetive of this challenge is to develop a function to determine the mean of a numeric vector.

myMean <- function(arg){
  
  result <- sum(arg) / length(arg)
  
  
  return( result )
}
myFunction(arg)

vector <- c(2,3,1,65,3,2)
myMean(vector)

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 1.2
# The main objetive of this challenge is to develop a function using conditional statments to check if a given number is positive or negative. The result should be a text displaying “is positive” or “is negative”

positive <- function(i){
  
  if(i > 0) { print( paste0("The element ",i," is positive")) }
  if(i < 0) { print( paste0("The element ",i," is negative")) }
  
}
positive(2)

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 1.3
# The main objetive of this challenge is to develop a function using more than one argument with conditional statments to check if two given numeric elements are both positive. The result should be “both elements are positive” or “one of the elements is negative”

positiveElements <- function(i,j){
  
  if(i > 0 & j > 0) { print( "both elements are positive") } else { "one of the elements is negative" }
  
}
positiveElements(2,1)

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 1.4
# The main objetive of this challenge is to populate a vector with the number of trials a computer needs to guess a random number (element) given by the user (between 1 and 10000).

guessingNumber <- function(userNumber){
  
  trials <- 0
  repeat {
    myTrial <- sample(1:1000,1)
    if( myTrial == userNumber ) { print(paste0("Found the number in ", trials , " trials")); break } else { trials <- trials + 1 }
  }
}
userNumber <- 23
guessingNumber(userNumber)








