#Math 185 HW3
#Ruoxin Huang A99084753

# PROBLEM 1
# Part A
permTest<-function(m,n){
  # generate 2 samples with given distribution
  x<-rnorm(m,0,1)
  y<-rnorm(n,0,sqrt(5))
  z<-c(x,y) # combine the samples
  D.perm<-seq(10000) # create sequence of permutation
  for(b in 1:10000){
    permZ<-sample(z,size=(m+n),replace=FALSE) # permute the sequence
    D.perm[b] = mean(permZ[1:m])-mean(permZ[(m+1):(m+n)]) # calculate the difference in mean
  }
  return(D.perm)
}

D.permA<-permTest(1000,3000) # n=1000,m=3000
# create histogram of the resulting data
hist(D.permA,breaks=40,main="permutated compared to not permutated distribution",freq=FALSE,ylim=c(0,10))
# create plot of the unpermerted data
plot(function(x) dnorm(x,0,sqrt(1/1000+5/3000)),add=TRUE,-1,1,col="blue")
lines(density(D.permA),col="red")
# label them
legend("topright",c("before permutation","after permutation"),bty="n",lty=1:1,col=c("blue","red"))

# Part B
D.permA<-permTest(2000,2000) # n=1000,m=3000
# create histogram of the resulting data
hist(D.permA,breaks=40,main="permutated compared to not permutated distribution",freq=FALSE,ylim=c(0,10))
# create plot of the unpermerted data
plot(function(x) dnorm(x,0,sqrt(1/2000+5/2000)),add=TRUE,-1,1,col="blue")
lines(density(D.permA),col="red")
# label them
legend("topright",c("before permutation","after permutation"),bty="n",lty=1:1,col=c("blue","red"))


# PROBLEM 2
# Part A
# the distribution of the test statistic is the same under the null

# Part B
# import package and laod cloudseeding
library(randtests)
load("/Users/julia/Desktop/cloudseeding.rda") 

# sort data according to seeded and unseeded
dat<-sort(c(cloudseeding$unseeded,cloudseeding$seeded))
# create sequence for data
datSeq<-seq(length(dat))
for(i in 1:length(dat)){
  datSeq[i]<-as.numeric(c[i]%in%cloudseeding$unseeded)
}
# apply test to data
runs.test(datSeq)
  