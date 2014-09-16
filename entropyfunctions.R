# Prashant B. Bhuyan
# is607 Entropy Project
# Due 9.14.2014 (Due 9.16.2014)

##### Problem 1 #####

# Solution
# Import the file into a data frame
data <- read.table("~/Desktop/entropy-test-file.csv", sep = ",", header = TRUE)
# Grab the answer col from the data df and save to ansData
ansData <- data$answer
# Get Frequency with which Various Types (num, factor, char) of Variables Appear in Categories within the Answer Data
ansDataFreq <- table(ansData)/length(ansData)
# Store Frequencies as a data frame with one col or a vec
ansDataVec <- as.data.frame(ansDataFreq)[,2]
# Entropy function measures the disorder of the answer data.  
# The greater the entropy the harder it is to predict what category
# an element from the answer data will fall into. 
# Handle case where target vector length is 0.
entropy <- function(dataVec){
  sum <- 0
  if(length(dataVec)==0){
    sum <- 0
  }
  else{
    sum <- -sum(dataVec * log2(dataVec))
  }
  print(sum)
}

# Results
# > entropy(ansDataVec)
# [1] 0.9832692

##### Problem 2a #####
# Solution:

target <- data$answer
att <- data$attr1

infogain <- function(target, att){
  
  entropy <- function(dataVec){
    sum <- 0
    if(length(dataVec)==0){
      sum <- 0
    }
    else{
      sum <- -sum(dataVec * log2(dataVec))
    }
    # print(sum)
  }
  
 targProb <- table(target)/length(target)
 attProb <- table(att)/length(att)
 targVec <- as.data.frame(targProb)[,2]
 attVec <- as.data.frame(attProb)[,2]
 targEnt <- entropy(targVec)
 sum <- (targEnt-(targEnt*entropy(attVec)))/length(target)
 print(sum)
        
}

# Results: 
# I'm off somewhere.  I need to spend more time on this. My result was:
# > infogain(target,att)
# [1] 2.856339e-05

##### Problem 2b #####

# Solution
# Again, I'm doing something wrong.  Here I took the original vector data$answer as the 
# parent node and then found the probability of picking a 0 or 1 from it respectively. 
# Then I took the attr2 vector as a child node and found the probability of picking an element from each 
# bin in attr2 given the probability in the parent node.  I then found then respective entropies
# and plugged them into the formula; however, I was way off.
# I have to keep moving along so I can't spend more time on this.

target <- data$answer
att <- data$attr2

infogain <- function(target, att){
  
  entropy <- function(dataVec){
    sum <- 0
    if(length(dataVec)==0){
      sum <- 0
    }
    else{
      sum <- -sum(dataVec * log2(dataVec))
    }
    # print(sum)
  }
  
  targetProb <- table(target)/length(target)
  attProb <- table(att)/length(att)
  
  targetVec <- as.data.frame(targetProb)[,2]
  attVec <- c(as.data.frame(attProb)[1,2],as.data.frame(attProb)[2,2],as.data.frame(attProb)[3,2])
  condVec <- c(targetVec,attVec)

  
  targetEnt <- entropy(targetVec)
  attEnt <- entropy(attVec)
  condEnt <- entropy(condVec)
  
  sum <- targetEnt - (targetEnt - (condEnt*attEnt)*((length(att)/length(target))))
  
  print(sum)
}

# Results:

# > sum
# [1] 2.457364

##### Problem 2C #####

target <- data$answer
att <- data$attr3

infogain <- function(target, att){
  
    entropy <- function(dataVec){
      sum <- 0
    if(length(dataVec)==0){
      sum <- 0
    }
    else{
      sum <- -sum(dataVec * log2(dataVec))
    }
    # print(sum)
    }
  
    targetProb <- table(target)/length(target)
    attProb <- table(att)/length(att)
    targetVec <- as.data.frame(targetProb)[,2]
    attVec <- c(as.data.frame(attProb)[1,2],as.data.frame(attProb)[2,2],as.data.frame(attProb)[3,2])
    condVec <- c(targetVec, attVec)
    condEnt <- entropy(condVec)
    targEnt <- entropy(targetProb)
    attEnt <- entropy(attProb)
    sum <- targEnt - (targEnt - sum((condEnt)/length(target)))
    print(sum)
}

# Results
# > infogain(target,att)
# [1] 0.002567797

##### Problem 3 #####

# Solution

infogain1 <- function(target, att){
  
  entropy <- function(dataVec){
    sum <- 0
    if(length(dataVec)==0){
      sum <- 0
    }
    else{
      sum <- -sum(dataVec * log2(dataVec))
    }
    # print(sum)
  }
  
  targProb <- table(target)/length(target)
  attProb <- table(att)/length(att)
  targVec <- as.data.frame(targProb)[,2]
  attVec <- as.data.frame(attProb)[,2]
  targEnt <- entropy(targVec)
  sum <- (targEnt-(targEnt*entropy(attVec)))/length(target)
  #print(sum)
  
}

infogain2 <- function(target, att){
  
  entropy <- function(dataVec){
    sum <- 0
    if(length(dataVec)==0){
      sum <- 0
    }
    else{
      sum <- -sum(dataVec * log2(dataVec))
    }
    # print(sum)
  }
  
  targetProb <- table(target)/length(target)
  attProb <- table(att)/length(att)
  targetVec <- as.data.frame(targetProb)[,2]
  attVec <- c(as.data.frame(attProb)[1,2],as.data.frame(attProb)[2,2],as.data.frame(attProb)[3,2])
  condVec <- c(targetVec, attVec)
  condEnt <- entropy(condVec)
  targEnt <- entropy(targetProb)
  attEnt <- entropy(attProb)
  sum <- targEnt - (targEnt - sum((condEnt)/length(target)))
  # print(sum)
}



targetCol <- data$answer
dataset <- data
decide <- function(dataset, targetCol){

      train <- as.data.frame(dataset)
      trainDF <- 1:nrow(train)
      trainWNames <- cbind(trainDF, train)
      target <- targetCol
        
      att1 <- dataset$attr1
      att2 <- dataset$attr2
      att3 <- dataset$att3
  
      att1Score <- infogain1(target,att1)
      #print(att1Score)
      att2Score <- infogain2(target, att2)
      #print(att2Score)
      att3Score <- infogain2(target, att3)
      #print(att3Score)
      scores <- c(att1Score, att2Score, att3Score)
      print("max")
      print(which(scores == max(scores)))
      gains <- c(attr1 = att1Score, attr2 = att2Score, attr3 = att3Score)
      print("gains")
      print(gains)
      
      
         
      
}

# Results
#
# I'm doing something wrong with the infogain computation so my values
# are off.  I need to review the solutions and re-visit.  
#
# targetCol <- data$answer
# dataset <- data
# decide(dataset, targetCol)
# [1] "max"
# [1] 2
# [1] "gains"
# attr1        attr2        attr3 
# 2.856339e-05 2.564148e-03 9.832692e-04 
   
