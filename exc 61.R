#setwd("/Users/Ko/Documents/SDA---Assignment-2")
setwd("C:/Users/timon/SDA---Assignment-2")
edata = read.table("expensescrime.txt", header = TRUE, stringsAsFactors = FALSE) 
#part a
edatanostates = subset(edata, select=-c(state))
head(edatanostates)
pairs(edatanostates)
#b
lawyerrate = (edata$lawyers*100)/edata$pop
plot(lawyerrate,edata$crime, xlab="Lawyer rate (per 100,000)", ylab="Crime rate (per 100,000)", main="Scatterplot of lawyer and crime rate")
outliercrimestate_i = which.max(edata$crime)
outliercrimestate = edata$state[outliercrimestate_i]
outliercrimestate

plot(lawyerrate,edata$crime, xlim=c(0,500), xlab="Lawyer rate (per 100,000)", ylab="Crime rate (per 100,000)", main="Scatterplot of lawyer and crime rate", sub="Zoomed in (lawyer rate max limit is 500)")
n = length(lawyerrate)
cor.test(lawyerrate,edata$crime, method="s")
cor.test(lawyerrate,edata$crime, method="k")

B = 1000
T = function(xs,ys) cor.test(xs,ys,method="k")$estimate
t = T(lawyerrate,edata$crime)
permutationtval = numeric(B)

for(i in 1:B)
{
  permutationtval[i] = T(lawyerrate,sample(edata$crime))
}
pl = sum(permutationtval<=t)/B
pr = sum(permutationtval>=t)/B
pperm = 2*min(pl,pr)
