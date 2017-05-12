# PROBLEM 1
# Part A
# The percentages do not sum up to 100% because 1644 is not divisble by 100.
# Therefore, as they were rounded to the nearest percentage, they lose accuracy in the process.
#For example, %46.6, %51.6, and %1.8 add up to 100% but after rouding they add uo to 101%

# Part B
#table the counts for each response
table <- c(1644*0.47,1644*0.52,1644*0.02) 
#create a vector of labels for the graphs
lbls <- c("federal government","each state", "unsure/no answer") 
#barplots the counts for each response
barplot(table,names.arg=lbls,main="who should fund pre-kindergarten programs",xlab="Responsess",ylab="Counts",col=rainbow(3))
#create pie chart for the counts of each response
pie(table,radius=1,labels=lbls,main='who should fund pre-kindergarten programs', col=rainbow(3))

# Part C
#Formalize the hypothesis test:
#H0= Americans have no preference between ¡®federal govern¡¯ and ¡®each state¡¯
#H1= Americans have a preference between ¡®federal govern¡¯ and ¡®each state¡¯
#Choose the chisquare test
chisq.test(table, p=c(0.50,0.50,0.00))
#p-value < 2.2e-16, which is very small, and the sample size (1644) is quite large compared to the number of possible values (3), 
#so there is strong evidence against the null hypothesis H0

# Part D
#My conclusion is based on the assumption that the sample data are independent and identically distributed random variables


# Problem 2
chisqApprox<-function(n, S, M=10^4)
{
  vector <- c() #create empty vector
  for(i in 1:10^4){ #loop 10^4 times
    data <- runif(n) #draw n observations from uniform distribution
    counts <- table(cut(data,breaks=seq(0,1,1/S))) #Table the counts into S categories
    mu <- rep(n/S,S) #expected counts for the S categories
    stat <- 2*sum(counts*log(counts/mu)) #compute likelihood ratio test statistic
    #Add a condition so that we do not have log(0) when S=50, whcih cause errors
    #This for loop makes program slow when S=50, but prevents errors when S=50, n=10,20,50
    for(j in 1:S){ 
      if(counts[j]==0){
        stat[j]=0
      }
    }
    vector <- c(vector, stat) #add the test statitics to the vector created earlier
  }
  hist(vector, breaks = 100, freq=FALSE) #plot the histogram
  lines(0:10^4,dchisq(0:10^4,S-1), col='red', lwd=2) #Overlay the density for the chi-squared distribution
}

#First 3-by-3 panel plot with S=5
par(mfrow=c(3,3))
chisqApprox(10,5)
chisqApprox(20,5)
chisqApprox(50,5)
chisqApprox(100,5)
chisqApprox(200,5)
chisqApprox(500,5)
chisqApprox(1000,5)
chisqApprox(2000,5)
chisqApprox(5000,5)

#Second a 3-by-3 panel plot with S=50
par(mfrow=c(3,3))
chisqApprox(10,50)
chisqApprox(20,50)
chisqApprox(50,50)
chisqApprox(100,50)
chisqApprox(200,50)
chisqApprox(500,50)
chisqApprox(1000,50)
chisqApprox(2000,50)
chisqApprox(5000,50)
#As can be seen on the graphs, when n gets large (aprroxiamtely 10 times larger than S), 
#the graph closely resembles a chi-square distribution with S-1 degress of freedom.

# Problem 3
# Part A
# read in data from file
table <- read.table("C:/Users/julia/Desktop/admin.txt", sep="\t", header=TRUE)
# view summary statitics
summary(table)
# we read from the summary statistics that there are significantly more white applicants(946) than 
# Black/Hispanic applicants(517), which is still more than Asian applicants(292).
# We also read that Accepted applicants(931) are more tan Turned Away applicants(524), and there 300 on the wait-list
# create contingency table
cTable <- xtabs(~table$Ethnicity+table$Admission.Decision, data=table)
cTable
# plot the contingency table
barplot(cTable,legend=rownames(cTable),main="admmision rates",beside=TRUE,xlab="admmision decision", ylab="counts",col=rainbow(3))
# we find that black/hispanic group have high acceptance rate, low turn away rate, and zero wait-list rate.

# Part B
#Formalize the hypothesis test:
#H0= there is no association between Admission and Ethnicity
#H1= there exists an association between Admission and Ethnicity
#Choose the chisquare test
chisq.test(cTable)
#p-value < 2.2e-16, which is very small, and the sample size(explored in PartA) is quite large compared to number of possible values(3)
#Therefore, we conclude that there is strong evidence against the null hypothesis H0

