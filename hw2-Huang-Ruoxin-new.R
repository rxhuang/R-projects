# PROBLEM 1
permIndepChisqTest<-function(tab, B = 2000)
{
  D.obs = chisq.test(tab)$stat #get the Pearson test statistic from the table
  D.perm = rep(0, B) #intialize vector of zeros for storing test statistics of B permutations of the table
  perm = r2dtable(B,margin.table(tab,1),margin.table(tab,2)) #create B permutations of the table according to the margins
  for(b in 1:B)
  {
    D.perm[b] = chisq.test(perm[[b]])$stat #calculate the Pearson test statistic for the permutated table
  }
  p.value = (sum(D.perm>=D.obs)+1) / (B+1) #calculate p-value
  return(p.value)
}

table <- read.table("C:/Users/julia/Desktop/admin.txt", sep="\t", header=TRUE) #read admission file
cTable <- xtabs(~table$Ethnicity+table$Admission.Decision, data=table) #create contingency table
# calculate running time of the function
start.time = proc.time()
permIndepChisqTest(cTable, B = 2000)
proc.time()-start.time
#calculate running time for chisq.test
start.time = proc.time()
chisq.test(cTable, B = 1e6)
proc.time()-start.time
#The result is that my function took time 211.47, while chisq.test only took time 0.05, which is much faster.

# PROBLEM 2
bootCI<-function(x, conf=0.99, B=1e4)
{
  n = length(x) #store the length of x
  t.distn = rep(0, B) #initilize vectors of 0 for storing t-ratios
  for(b in 1:B)
  {
    resample = sample(x,n,replace=TRUE) #Generating an iid sample from the empirical distribution
    t.distn[b] = (mean(resample)-mean(x))/(sd(resample)/sqrt(n)) #Compute the corresponding t-ratio
  }
  CI = mean(x)+quantile(t.distn,c((1-conf)/2,(1+conf)/2))*sd(x)/sqrt(n)#compute confidence interval
  return(CI)
}

# PROBLEM 3
library(boot)
bootCIpackage<-function(x, conf=0.99, B=1e4)
{
}