#Math 185 HW2
#Ruoxin Huang A99084753

# PROBLEM 1
# Part A
# We have a total of n samples, each can be assigned a positive sign or a negative sign, giving us 2^n permutations.
# We need to divide the total number of permutations to get the p=value, so we need the factor 2^(-n)

# Part B
flipSignTest1<-function(x, B=999)
{
  count<-0 #number of times Y-epsilon is greater than y-star
  originalMean<-mean(x) # mean of original sample
  
  for(i in 1:B)
  {
    randomSign<-sample(c(-1,1),1,size=B) #vector of random sign
    resampleMean<-mean(x*randomSign) #assign random signs to x and compute mean
    if(resampleMean>=originalMean) #if mean of resample larger than original sample
    {
      count<-count+1 # incremeent count
    }
  }
  pValue <- (count+1)/(B+1) # calculate p value
  return(pValue) #return pvalue
}

# Part C
flipSignTest2<-function(x, B=999)
{
  count<-0 #number of times Y-epsilon is greater than y-star
  originalMean<-mean(x) # mean of original sample
  
  for(i in 1:B)
  {
    randomSign<-sample(c(-1,1),1,size=B) #vector of random sign
    resampleMean<-mean(x*randomSign) #assign random signs to x and compute mean
    if(resampleMean<=(-abs(originalMean))) #if mean of resample smaller than negative abs of original man
    {
      count<-count+1 # incremeent count
    }
  }
  pValue <- (count+1)/(B+1) # calculate p value
  return(pValue) #return pvalue
}

# PROBLEM 2
# Part A
#install and use packages
install.packages("Hmisc") 
install.packages("UsingR")
require("UsingR")
height<-father.son #assign data to height
sonAverageHeight<-(mean(c(height$sheight))) #compute average height of sons
fatherAverageHeight<-(mean(c(height$fheight))) #compute average height of fathers
#plot to compare the avergae height of sons and fathers
barplot(c(sonAverageHeight,fatherAverageHeight),main="father and son average height", names.arg=c("son           father"), col=c("green","yellow"))

# Part B
x<-c(height$fheight) #create vector containing fathers` height
y<-c(height$sheight)#create vector containing sons` height
z<-y-x #create vector containing difference in height
flipSignTest1(z, B=999) #test  for  symmetry  of  the  distribution  of Z about 0
#This is a permutation test because we assign random signs to the difference of heights of fathers and sons and we test if the result is symmetric about zero

# PROBLEM 3
meanDifference<-fatherAverageHeight-sonAverageHeight #comput the mean difference of height of fathers and sons
confidencInterval<-meanDifference-flipSignTest1(z, B=999):meanDifference+flipSignTest1(z, B=999) #compute confidence interval