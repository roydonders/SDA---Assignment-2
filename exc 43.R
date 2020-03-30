setwd("C:/Users/timon/SDA---Assignment-2")
source("light.txt")
v1879 = light$`1879`
v1882 = light$`1882`

alpha = 0.05
B = 1000
n1879 = length(v1879)

adj_dnstar = numeric(B)
for(i in 1:B){
  xstar = rnorm(n1879)
  ystar = (xstar-mean(xstar))/sd(xstar)
  adj_dnstar[i] = ks.test(ystar, pnorm, mean = 0, sd = 1)$statistic
}

hist(adj_dnstar, main="Histogram of (Adjusted) DnStar", col="green1", xlim=c(0,0.15))
t1879 = ks.test(v1879, pnorm, mean = mean(v1879), sd = sd(v1879))$statistic
p1879 = sum(adj_dnstar>=t1879)/B
p1879 <= alpha

n1882 = length(v1882)
t1882 = ks.test(v1882, pnorm, mean = mean(v1882), sd = sd(v1882))$statistic
p1882 = sum(adj_dnstar>=t1882)/B
p1882 <= alpha

direct_adjDn1879 = ks.test(v1879, pnorm, mean = mean(v1879), sd = sd(v1879))
direct_adjDn1882 = ks.test(v1882, pnorm, mean = mean(v1882), sd = sd(v1882))