library(tidyr)
library(dplyr)

# Set your working directory to wherever you want it to be
setwd("C:/cygwin64/home/andrewHill/forFun/kaggle/titanic")

getCleanTruth <- function(tindata) {
    Y <- select(tinData, Survived)
    return(Y)
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
    
    return(X)
}

getSigmoid <- function(z) {
    g = 1 / (1 + exp(-z))
    return(g)
}

costFunction <- function(theta, X, Y) {
    m = length(Y)
    
    z = t(theta) %*% t(X)
    
    h = getSigmoid(z)
    
    J = (1 / m) * (-(t(Y) %*% t(log(h))) - ((1 - t(Y)) %*% t(log(1 - h))))
    
    grad = (1 / m) * t((h - t(Y)) %*% X)
    
    Jgrad <- list("J" = J, "grad" = grad)
    
    return(Jgrad)
}

tinData <- data.frame()
tinData <- read.csv("data/train.csv", stringsAsFactors = FALSE)

Y <- getCleanTruth(tinData)
X <- getCleanFeatures(tinData)

Y <- data.matrix(Y)
X <- data.matrix(X)

initialTheta <- matrix(0, ncol = 1, nrow = dim(X)[2])

Jgrad <- costFunction(initialTheta, X, Y)

J <- Jgrad$J
grad <- Jgrad$grad