#setwd("/Users/Ko/Documents/SDA---Assignment-2")
setwd("C:/Users/timon/SDA---Assignment-2")
edata = read.table("expensescrime.txt", header = TRUE, stringsAsFactors = FALSE) 
#part a
edatanostates = subset(edata, select=-c(state))
head(edatanostates)
pairs(edatanostates)
#b
plot(lawyerrate,edata$crime, xlab="Lawyer rate (per 100,000)", ylab="Crime rate (per 100,000)", main="Scatterplot of lawyer and crime rate")
outliercrimestate_i = which.max(edata$crime)
outliercrimestate = edata$state[outliercrimestate_i]
outliercrimestate

plot(lawyerrate,edata$crime, xlim=c(0,500), xlab="Lawyer rate (per 100,000)", ylab="Crime rate (per 100,000)", main="Scatterplot of lawyer and crime rate", sub="Zoomed in (lawyer rate max limit is 500)")
n = length(lawyerrate)
cor.test(lawyerrate,edata$crime, method="s")
cor.test(lawyerrate,edata$crime, method="k")
