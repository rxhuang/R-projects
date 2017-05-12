#Math 185 HW4
#Ruoxin Huang A99084753

# PROBLEM 1
twowayPermTest<-function(dat,B = 999)
{
  SST.sim<-numeric(B) #create vector for simulation of treatment sum of square
  mean<-mean(dat[,1]) #compute mean
  t.mean<-tapply(dat[,1],dat[,2],mean) #compute mean according to supplement type
  X<-summary(as.factor(dat[,3]))
  n<-X[1]/length(t.mean)
  SST<-sum((mean-t.mean)^2)*n*length(X)#compute the treatment sum of square
  t.mean.sim<-numeric(length(t.mean)) #create vector for simulation of t.mean
  
  for(b in 1:B){
    sim<-tapply(dat[,1],dat[,3],sample)#compute simulation by sampling length with dose
    for(j in 1:length(t.mean)){
      sum<-0
      for(i in 1:length(X)){
        simX<-data.frame(sim[i])#compute simulation of X
        sum<-sum+sum(simX[((j-1)*n+1):(j*n),1])#add the simulation to sum
      }
      t.mean.sim[j]<-sum/(length(X)*n)#fill simulation of t.mean with means of simulations
    }
    SST.sim[b]<-sum((mean-t.mean.sim)^2)*n*length(X)#compute the treatment sum of square using Monte Carlo with means
  }
  p.value<-(sum(SST.sim>=SST)+1)/(B+1)#compute p-value and return
  return(p.value)
}

dat = ToothGrowth # ToothGrowth is readily available in R
twowayPermTest(dat) #apply the test to ToothGrowth data