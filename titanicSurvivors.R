library(tidyr)
library(dplyr)

# Set your working directory to wherever you want it to be
# setwd("C:/cygwin64/home/andrewHill/forFun/kaggle/titanic/")
setwd("C:/cygwin64/home/hill/forFun/kaggle/titanic/titanicKaggle")

getCleanTruth <- function(tindata) {
    Y <- select(tinData, Survived)
    return(Y)
}

featureScaling <- function(xFeature) {
  u <- mean(xFeature)
  sigma <- sd(xFeature)
  
  xScale <- (xFeature - u) / sigma
  
  return(xScale)
}

getCleanFeatures <- function(tinData) {
#    passID <- select(tinData, PassengerId)

    X <- select(tinData, -PassengerId, -Survived, -Ticket, -Cabin)
    
    # X2 <- separate(X, Name, c("last.Name", "first.Name"), ",")
    # X2 <- select(X2, -first.Name)
    # X2$last.Name <- as.factor(X2$last.Name)
    # X2$last.Name <- as.numeric(X2$last.Name)
    X <- select(X, -Name)
    
    #X$Sex <- as.factor(X$Sex)
    #X$Sex <- as.numeric(X$Sex)
    
    X$Age[is.na(X$Age)] <- quantile(X$Age, runif(sum(is.na(X$Age))), na.rm = TRUE, names = FALSE)
    
    X$Embarked[X$Embarked == ""] <- NA
    X$Embarked <- as.factor(X$Embarked)
    X$Embarked <- as.numeric(X$Embarked)
    X$Embarked[is.na(X$Embarked)] <- quantile(X$Embarked, runif(sum(is.na(X$Embarked))),
                                              na.rm = TRUE, names = FALSE, type = 1)
    
    X <- mutate(X, class1 = as.numeric(Pclass == 1), class2 = as.numeric(Pclass == 2), male = as.numeric(Sex == "male"),
                embark.Char = as.numeric(Embarked == 1), embark.Queens = as.numeric(Embarked == 2))
    X <- select(X, -Pclass, -Sex, -Embarked)
    
    X <- cbind(1, X)
    
    X$Age <- featureScaling(X$Age)
    X$Fare <- featureScaling(X$Fare)
    
    return(X)
}

getSigmoid <- function(z) {
    g = 1 / (1 + exp(-z))
    return(g)
}

hypFunction <- function(theta, X) {
  z = t(theta) %*% t(X)
  
  h = getSigmoid(z)
  
  return(h)
}

costFunction <- function(theta, X, Y) {
  m = length(Y)
  
  z = t(theta) %*% t(X)
  
  h = getSigmoid(z)
  
  J = (1 / m) * (-(t(Y) %*% t(log(h))) - ((1 - t(Y)) %*% t(log(1 - h))))
  
  return(J)
}

gradFunction <- function(theta, X, Y) {
  m = length(Y)
  
  z = t(theta) %*% t(X)
  
  h = getSigmoid(z)
  
  grad = (1 / m) * t((h - t(Y)) %*% X)
  
  return(grad)
}

tinData <- data.frame()
tinData <- read.csv("data/train.csv", nrows = 446, stringsAsFactors = FALSE)

YTin <- getCleanTruth(tinData)
XTin <- getCleanFeatures(tinData)

Y <- data.matrix(YTin)
X <- data.matrix(XTin)

initialTheta <- matrix(0, ncol = 1, nrow = dim(X)[2])

# I can't figure out a way to use this, but if I don't, I'll have to calculate h twice,
# which seems like a big waste of resources... But the first argument of costFunction
# has to be theta... Wait--I don't have to USE the first argument... Let's try that.
# Nope, doesn't work--if it optimized over a variable that wasn't in the function,
# It would just be a constant...

# h <- hypFunction(initialTheta, X)

# J <- costFunction(initalTheta, X, Y)

# grad <- gradFunction(initialTheta, X, Y)

correctTheta <- optim(initialTheta, costFunction, gradFunction, X, Y, method = "BFGS")

thetaPar <- correctTheta$par

tinTest <- data.frame()
tinTest <- read.csv("data/train.csv", stringsAsFactors = FALSE)

tinTest <- tinTest[-c(1:445), ]

YIntTest <- getCleanTruth(tinTest)
XIntTest <- getCleanFeatures(tinTest)

YTest <- data.matrix(YIntTest)
XTest <- data.matrix(XIntTest)

h <- hypFunction(thetaPar, XTest)
hTest <- round(h)