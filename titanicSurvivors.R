library(tidyr)
library(dplyr)

# Set your working directory to wherever you want it to be
setwd("C:/cygwin64/home/andrewHill/forFun/kaggle/titanic")

getMissingAge <- function(Ages) {
    
}

tinData <- data.frame()
tinData <- read.csv("data/train.csv", stringsAsFactors = FALSE)

passID <- select(tinData, PassengerId)
Y <- select(tinData, Survived)
X <- select(tinData, -PassengerId, -Survived)

# X2 <- separate(X, Name, c("last.Name", "first.Name"), ",")
# X2 <- select(X2, -first.Name)
# X2$last.Name <- as.factor(X2$last.Name)
# X2$last.Name <- as.numeric(X2$last.Name)
X <- select(X, -Name)

X$Sex <- as.factor(X$Sex)
X$Sex <- as.numeric(X$Sex)

X$Age[is.na(X$Age)] <- quantile(X$Age, runif(sum(is.na(X$Age))), na.rm = TRUE, names = FALSE)