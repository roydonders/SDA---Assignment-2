getwd()
setwd("/Users/Ko/Documents/SDA---Assignment-2")
data_t = read.table("t-sample.txt")
library(e1071)
library(boot)

random_norm = rnorm(10000,mean=0,sd=1)
t_sample = c(data_t$V1,data_t$V2,data_t$V3,data_t$V4,data_t$V5)

hist(t_sample)
k_t_sample = kurtosis(t_sample,type=1)
kurtosis(random_norm)

x = t_sample
#kurtosis functie
kurt = function(x) {(n* sum((x-mean(x))^4)/(sum((x-mean(x))^2)^2))-3
}
kurt(x)
kurtosis(x,type=1)

n = length(t_sample)
s = var(t_sample) 
k = 2*s^2 / (s^2 - 1)

#emp_function = function(y=x, i){kurt(i)}

#par_function = dt(t_sample, df = k)

#empBS = boot(t_sample,statistic = emp_function(),R=1000)
#parBS = boot(t_sample,statistic = par_function, R=1000)

hist(empBS)
hist(parBS)
