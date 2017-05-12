# Problem 1
load("/Users/julia/Desktop/cloudseeding.rda") #get cloud seeding data

perm.ks.test <- function(x,y,alternative=c("two.sided", "less", "greater"),B=999)
{
  z = c(x,y) #put x and y together into 1 vector
  x.length = length(x) #get the length of x and y
  y.length = length(y)
  D = ks.test(x,y,alternative)$statistic #get the original ks test statistics
  D.perm = numeric(B) #create vector storing permutated ks test statistic
  for(i in 1:B){
    z.perm = sample(z) #permutated z
    x.perm = z.perm[1:x.length] #divide z.perm into length x and length y elements
    y.perm = z.perm[(x.length+1):(x.length+y.length)]
    D.perm[i] = ks.test(x.perm,y.perm,alternative)$statistic #calculate ks test stat for the permutated z
  }
  p.value = (sum(D.perm>D)+1)/(B+1) #perform Monte Carlo
  return(p.value) #return p-value
}
perm.ks.test(clouds[,1],clouds[,2],"two.sided") #perform perm.ks.test on cloud seeding data
# result is 0.001

# Problem 2


# Problem 3
qqtest <- function(x,y,alternative=c("two.sided", "less", "greater"),B=999)
{
  x.quant = quantile(x,seq(0,1,by=0.01)) #get the 100% quantiles of x and y
  y.quant = quantile(y,seq(0,1,by=0.01))
  D = max(x.quant-y.quant) #get original qq test stat
  z = c(x,y) #put x and y together into 1 vector
  x.length = length(x) #get the length of x and y
  y.length = length(y)
  D.perm = numeric(B) #create vector storing permutated ks test statistic
  for(i in 1:B){
    z.perm = sample(z) #permutated z
    x.perm = z.perm[1:x.length] #divide z.perm into length x and length y elements
    y.perm = z.perm[(x.length+1):(x.length+y.length)]
    x.quant = quantile(x.perm,seq(0,1,by=0.01)) # get quantiles of permutated x and y
    y.quant = quantile(y.perm,seq(0,1,by=0.01))
    D.perm[i] = max(x.quant-y.quant) #calculate qq test stat for the permutated z
  }
  # use Monte Carlo to get p-value based on alternative
  p.value = NULL
  if(alternative == "less"){
    p.value = (sum(D.perm[i]<=D)+1)/(B+1) 
  }
  else if(alternative == "greater"){
    p.value = (sum(D.perm[i]>=D)+1)/(B+1) 
  }
  else if(alternative == "two.sided"){
    p.value = (sum(abs(D.perm[i])>=abs(D))+1)/(B+1) 
  }
  return(p.value) #return p-value
}