getwd()
setwd("/Users/Ko/Documents/SDA---Assignment-2")
#setwd("C:/Users/timon/SDA---Assignment-2")
data_t = read.table("t-sample.txt")
source("functions_Ch4.txt")
source("functions_Ch5.txt")
library(e1071)

random_norm = rnorm(10000,mean=0,sd=1)
t_sample = c(data_t$V1,data_t$V2,data_t$V3,data_t$V4,data_t$V5)

par(mfrow=c(1,1),pty="s")
hist(t_sample)
k_t_sample = kurtosis(t_sample,type=1)
kurtosis(random_norm)

n = length(t_sample)
x = t_sample
#kurtosis functie
kurt = function(x) {(n* sum((x-mean(x))^4)/(sum((x-mean(x))^2)^2))-3
}

kurt(x)
kurtosis(x,type=1)

s = var(t_sample) 
#degree of freedom
k = 2*s / (s - 1)

#bootstrap
#B=1000
#empBS_TStarx = numeric(B)
#for(i in 1:B){
 # emp=sample(x[], replace = TRUE)
 # empBS_TStarx[i] = kurtosis(emp)
#}
#hist(empBS_TStarx)

empBS_TStar = bootstrap(x,kurtosis,B=1000)

par(mfrow=c(1,1),pty="s")
hist(empBS_TStar,main = "Histogram of empBS_TStar with excess kurtosis")
abline(v=k_t_sample,col="red")
hist(empBS_TStar,main = "Histogram of empBS_TStar with true kurtosis = 1")
abline(v=1,col="red")
var(empBS_TStar)

B = 1000
parBS_TStar = numeric(B)
for(i in 1:B){
  xstar= rt(x,k)
  parBS_TStar[i] = kurtosis(xstar)
}

hist(parBS_TStar)
par(mfrow=c(1,1),pty="s")
hist(parBS_TStar,main = "Histogram of parBS_TStar with excess kurtosis")
abline(v=k_t_sample,col="red")
hist(parBS_TStar,main = "Histogram of parBS_TStar with true kurtosis = 1")
abline(v=1,col="red")
var(parBS_TStar)
