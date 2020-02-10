# ---------------------------------------------------------------
# Challenge 1.4

guessingNumber <- function(userNumber){

  trials <- 0
  repeat {
    myTrial <- sample(1:1000,1)
    if( myTrial == userNumber ) { print(paste0("Found the number in ", trials , " trials")); break } else { trials <- trials + 1 }
  }
}
userNumber <- 23
guessingNumber(userNumber)
