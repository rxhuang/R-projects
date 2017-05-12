#Math 185 HW1
#Ruoxin Huang A99084753

#1.
#A.

#load the file into RStudio
load("/Users/julia/Desktop/earthquakes-2014.rda") 

#Use the function subset to extract the events of magnitude at least 2 
dat2 <- subset(dat,magnitude>=2)

#produce a table of counts for the number of events in eachmonth
table <- table(dat2$month)
table

#draw a relevant plot using bargraph
barplot(table, main='events of magnitude at least 2',  xlab="Months", ylab="Counts", col=rainbow(12))

#B.

#The Pearson chi-squared goodness-of-fit test
chisq.test(tab) 

#The null is that the events of magnitude at least 2 is evenly distributed among 12 months
#Result of test shows p-value < 2.2e-16, which rejects the null

#2.
#A.

#Yes, we have all the information needed for a test of association

#B

#Create a table for the data
admissions <- matrix(c(3738,8442-3738,1494,4321-1494),ncol=2,byrow=TRUE)
admissions

#The Pearson chi-squared goodness-of-fit test
chisq.test(admissions)

#The null is that the admission of a student is independent from gender
#Result of test shows p-value < 2.2e-16, which rejects the null

#3.
#A.

#Create 'flat' contingency table that groups all departments together
table2 <- ftable(UCBAdmissions[,,1]+UCBAdmissions[,,2]+UCBAdmissions[,,3]+UCBAdmissions[,,4]+UCBAdmissions[,,5]+UCBAdmissions[,,6])
table2

#The Pearson chi-squared goodness-of-fit test
chisq.test(dat3)

#The null is that the admission of a student is independent from gender
#Result of test shows p-value < 2.2e-16, which rejects the null

#B

#Create 'flat' contingency table for UCBAdmissions data
table3 <- ftable(UCBAdmissions)
table3

#Plot a bar graph with the data
barplot(table3, main="Admissions for different genders", xlab="Departments", col=c("green","yellow"), beside=TRUE)

#In the graph, green is male and yellow is female. 
#To see whether gender and admissions is independent, we compare the ratio of the two same colored bars with that of the other color.
#The result is that the ratio is different in department A only.
#Therefore, gender and admission is independent in departments B,C,D,E,F
#Thus, this  graphical investigation is not congruent with your previous ndings?

