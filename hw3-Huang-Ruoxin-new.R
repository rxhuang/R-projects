# Problem 1
# Part A
# copy and paste method from answers in HW2 Problem 2
#The function bootCI produces a bootstrap Studentized confidence interval for the mean
bootCI <- function(x, conf, B=1e4){
  #calculate the mean and standard deviation of the original sample
  mean.x <- mean(x)
  sd.x <- sd(x)
  #create a vector to store the value of t test statistic
  t <- numeric(B)
  n <- length(x)
  #use a for loop to bootstrap for B times
  for (b in 1 : B){
    #generate the new sample from the original one
    boot <- sample(x, n, replace = TRUE)
    mean.boot <- mean(boot)
    sd.boot <- sd(boot)
    #calculate the t test statistic
    t[b] <- (mean.boot - mean.x)/(sd.boot/sqrt(n))
  }
  #calculate the confidence interval
  ci <- mean.x + (sd.x/sqrt(n))*quantile(t,c((1-conf)/2,1-(1-conf)/2))
  return(ci)
}


#create vectors of 0s to store coverage for each sample size
student.record <- numeric(10)
boot.record <- numeric(10)
#create vectors of 0s to store CI length for each sample size
student.size <- numeric(10)
boot.size <- numeric(10)
for (n in c(10,20,30,40,50,60,70,80,90,100))
{
  # initilize the # of occurances of inclusion of mean in CI to 0
  student.sum = 0
  boot.sum = 0
  student.length = 0
  boot.length = 0
  for (m in 1 : 1000){
    x <- rnorm(n,0,1) #generare random sample of size n from standard normal distribution
    student.ci=t.test(x, mu=0, conf.level=0.99)$conf.int #generate student CI using t.test
    boot.ci = bootCI(x, conf=0.99, B=1e4) #generate bootstrap student CI using function from HW2
    if(0>student.ci[1]&0<student.ci[2]){ #if student CI contains mean, increment student.sum
      student.sum = student.sum+1 
    }
    if(0>boot.ci[1]&0<boot.ci[2]){ #if bootstrap student CI contains mean, increment boot.sum
      boot.sum = boot.sum+1 
    }
    #sum up the CI lengths
    student.length = student.length + (student.ci[2] - student.ci[1])
    boot.length = boot.length + (boot.ci[2] - boot.ci[1])
    
  }
    #record our findings
    student.record[n/10] = student.sum/1000
    boot.record[n/10] = boot.sum/1000
    student.size[n/10] = student.length/1000
    boot.size[n/10] = boot.length/1000
}
sample.sizes = c(10,20,30,40,50,60,70,80,90,100)
matplot(sample.sizes,ylim=c(0,1),cbind(boot.record,student.record), main="coverage to sample size",xlab="sample sizes", ylab="coverage",type="l",col=c("blue","red"))
matplot(sample.sizes,cbind(boot.size,student.size), main="CI length to sample size",xlab="sample sizes", ylab="CI length",type="l",col=c("blue","red"))



# Part B
#create vectors of 0s to store coverage for each sample size
student.record <- numeric(10)
boot.record <- numeric(10)
#create vectors of 0s to store CI length for each sample size
student.size <- numeric(10)
boot.size <- numeric(10)
for (n in c(10,20,30,40,50,60,70,80,90,100))
{
  # initilize the # of occurances of inclusion of mean in CI to 0
  student.sum = 0
  boot.sum = 0
  student.length = 0
  boot.length = 0
  for (m in 1 : 1000){
    x <- rexp(n) #generare random sample of size n from exponential distribution with mean 1
    student.ci=t.test(x, mu=0, conf.level=0.99)$conf.int #generate student CI using t.test
    boot.ci = bootCI(x, conf=0.99, B=1e4) #generate bootstrap student CI using function from HW2
    if(0>student.ci[1]&0<student.ci[2]){ #if student CI contains mean, increment student.sum
      student.sum = student.sum+1 
    }
    if(0>boot.ci[1]&0<boot.ci[2]){ #if bootstrap student CI contains mean, increment boot.sum
      boot.sum = boot.sum+1 
    }
    #sum up the CI lengths
    student.length = student.length + (student.ci[2] - student.ci[1])
    boot.length = boot.length + (boot.ci[2] - boot.ci[1])
    
  }
  #record our findings
  student.record[n/10] = student.sum/1000
  boot.record[n/10] = boot.sum/1000
  student.size[n/10] = student.length/1000
  boot.size[n/10] = boot.length/1000
}
sample.sizes = c(10,20,30,40,50,60,70,80,90,100)
matplot(sample.sizes,ylim=c(0,1),cbind(boot.record,student.record), main="coverage to sample size",xlab="sample sizes", ylab="coverage",type="l",col=c("blue","red"))
matplot(sample.sizes,cbind(boot.size,student.size), main="CI length to sample size",xlab="sample sizes", ylab="CI length",type="l",col=c("blue","red"))
