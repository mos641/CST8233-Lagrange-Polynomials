# Lab 5
# Take in xVec and yVec then Xk, calculate value of Yk

# testLagR <- function()
# {
#   # define variables
#   xVec <- vector("numeric", length = numN)
#   yVec <- vector("numeric", length = numN)
#   index <- 1
#   index2 <- 1
#   yK <- 0
#   LTotal <- 1
# 
#   xVec <- c(1, 1.3, 1.6, 1.9, 2.2)
#   yVec <- c(0.144, -0.6878, -0.9962, -0.5507, 0.3115)
# 
#   xK <- (xVec[which.min(xVec)] - 1)
#   # ask for value being interpreted (Xk), loop for range
#   while (xK < xVec[which.min(xVec)] || xK > xVec[which.max(xVec)]){
#     xK <- as.double(readline(prompt = "Enter value to interpolate: "))
#     # verify it is within range
#     if (xK < xVec[which.min(xVec)] || xK > xVec[which.max(xVec)]){
#       print(paste("Enter a value in the valid range of ", xVec[which.min(xVec)], " to ", xVec[which.max(xVec)], ", try again"))
#     }
#   }
# 
#   # Use langrange equation to calculate, 2 loops
#   index <- 1
#   # loop for each Li multiplied by F(Xi)
#   while(index <= numN){
#     # Loop for each Li
#     while(index2 <= numN){
#       # confirm i != j
#       if (index != index2){
#         LTotal <- LTotal * ((xK - xVec[index2])/(xVec[index] - xVec[index2]))
#       }
#       index2 <- index2 + 1
#     }
#     # add the multiplication of Li by Yi then reset
#     yK = yK + (LTotal * yVec[index])
#     LTotal <- 1
#     index2 <- 1
#     index <- index + 1
#   }
# 
#   # after loop return value
#   return(yK)
# }

lagInter <- function()
{
  # ask for input
  numN <- readline(prompt = "Enter how many data points you have (N): ")
  numN <- as.integer(numN)
  
  # define variables
  xVec <- vector("numeric", length = numN)
  yVec <- vector("numeric", length = numN)
  index <- 1
  index2 <- 1
  yK <- 0
  LTotal <- 1
  
  # loop through data set
  while(index <= numN){
    xVec[index] <- as.double(readline(prompt = paste("Enter x value ",index, ": ")))
    yVec[index] <- as.double(readline(prompt = paste("Enter y value ",index, ": ")))
    index <- index + 1
  }
  
  xK <- (xVec[which.min(xVec)] - 1)
  # ask for value being interpreted (Xk), loop for range
  while (xK < xVec[which.min(xVec)] || xK > xVec[which.max(xVec)]){
    xK <- as.double(readline(prompt = "Enter value to interpolate: "))
    # verify it is within range
    if (xK < xVec[which.min(xVec)] || xK > xVec[which.max(xVec)]){
      print(paste("Enter a value in the valid range of ", xVec[which.min(xVec)], " to ", xVec[which.max(xVec)], ", try again"))
    }
  }
  
  # Use langrange equation to calculate, 2 loops
  index <- 1
  # loop for each Li multiplied by F(Xi)
  while(index <= numN){
    # Loop for each Li
    while(index2 <= numN){
      # confirm i != j
      if (index != index2){
        LTotal <- LTotal * ((xK - xVec[index2])/(xVec[index] - xVec[index2]))
      }
      index2 <- index2 + 1
    }
    # add the multiplication of Li by Yi then reset
    yK = yK + (LTotal * yVec[index])
    LTotal <- 1
    index2 <- 1
    index <- index + 1
  }
  
  # after loop return value
  return(yK)
}


lagInter()
#testLagR()
