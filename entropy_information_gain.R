# Prashant B. Bhuyan
# 9.14.2014
# Entropy and Information Gain


# Import the file into a data frame
data <- read.table("~/Desktop/entropy_data.csv", sep = ",", header = TRUE)
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
 
  val_iso <- vector(mode="numeric")
  isx <- dataVec[!duplicated(dataVec)]
  for(i in 1:length(isx)){
    val_iso[i] <- length(dataVec[dataVec==isx[i]])/length(dataVec)
  }
  e_vals <- (-1) * val_iso * log2(val_iso)
  return(sum(e_vals))
}

information_gain <- function(tgt,attr){
  subset.entropies <- vector(mode="numeric")
  subset.weights <- vector(mode="numeric")
  subsets <- attr[!duplicated(attribute)]
  for(i in 1:length(subsets)){
    subset.entropies[i] <- entropy(tgt[attr==subsets[i]])
    subset.weights[i] <- length(target[attr==subsets[i]])/length(tgt)
  }
  return(entropy(tgt)-sum(subset.entropies*subset.weights))
}

choose <- function(dfrm,target=1){
  # Create vector of infogains
  return.infogains <- vector(mode="numeric",length=length(dfrm[1,])-1)
  number.infogains <- vector(mode="numeric",length=length(dfrm[1,])-1)
  count <- 1
  for(i in seq(1,length(dfrm[1,]))[!seq(1,length(dfrm[1,]))==target]){
    return.infogains[count] <- infogain(target=dfrm[,target],attribute=dfrm[,i])
    number.infogains[count] <- i
    count <- count+1
  }
  return.max <- number.infogains[which.max(return.infogains)]
  names(return.infogains) <- colnames(dfrm[!seq(1,length(dfrm[1,]))==target])
  return.list <- list(max=return.max,gains=return.infogains)
  return(return.list)
}


# Results
entropy(ansDataVec)
# [1] 0.9832692

infogain(data$answer,data$attr1)
# [1] 2.411565e-05

infogain(data$answer,data$attr2)
# [1] 0.2599038

infogain(data$answer,data$attr3)
# [1] 0.002432707

choose(data,4)
# $max
# [1] 2

# $gains
# attr1        attr2        attr3 
# 2.411565e-05 2.599038e-01 2.432707e-03 
