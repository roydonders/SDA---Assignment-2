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

library('mvtnorm')
samplebv = function(n) rmvnorm(n, mean = c(0,0), sigma = matrix(c(1,0.5,0.5,1), 2, 2))

aresimulation=function(B,n)
{
  pvalkendall=numeric(B)
  pvalspearman=numeric(B)
  for(i in 1:B)
  {
    bvdatai=samplebv(n)
    xs = bvdatai[,1]
    ys = bvdatai[,2]
    pvalkendall[i]=cor.test(xs,ys,method="k")$p.value
    pvalspearman[i]=cor.test(xs,ys,method="s")$p.value

  }
  powerk=sum(pvalkendall<=0.05)/B
  powers=sum(pvalspearman<=0.05)/B
  rbind(c("kendall","spearman"),c(powerk,powers))
}
aresimulation(10000,50)[,1]
aresimulation(10000,45)[,2]
aresimulation(10000,50)[,2]
aresimulation(10000,55)[,2]