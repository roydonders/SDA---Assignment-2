#setwd("/Users/Ko/Documents/SDA---Assignment-2")
setwd("C:/Users/timon/SDA---Assignment-2")
source("functions_Ch3.txt")
source("functions_Ch5.txt")
newcomb = matrix(scan("newcomb.txt"))
f_newcomb = newcomb[1:20]
s_newcomb = newcomb[21:66]

par(mfrow =c(1,4),pty="s")
summary(newcomb)
hist(newcomb, xlim=c(-50,50), ylim=c(0,0.1), prob=T)
qqnorm(newcomb)
qqline(newcomb)
boxplot(newcomb, main="Boxplot of newcomb")


par(mfrow =c(2,3),pty="s")
hist(f_newcomb, xlim=c(-50,50), ylim=c(0,0.1), prob=T)
hist(s_newcomb, xlim=c(-50,50), ylim=c(0,0.1), prob=T)
boxplot(f_newcomb,s_newcomb, main="Boxplot of f_newcomb and s_newcomb")
qqchisq(f_newcomb, df = 1000, ylab="f_newcomb Sample Quantiles")
qqline(f_newcomb, dist = function(p) qchisq(p, df = 1000))
qqnorm(s_newcomb, ylab="s_newcomb Sample Quantiles")
qqline(s_newcomb)

est = median(f_newcomb) - median(s_newcomb)

data = c(f_newcomb,s_newcomb)
nx = length(f_newcomb)
ny = length(s_newcomb)

myteststat = function(z,m,n) {median(z[1:m])-median(z[(m+1):(m+n)])}
B=1000
permutationtval = numeric(B)
for (i in 1:B) permutationtval[i]=myteststat(sample(data),nx,ny)
t=myteststat(data,nx,ny)
sum(permutationtval<=t)/B

ks.test(f_newcomb,s_newcomb,alternative = "greater",conf.int = "true")
wilcox.test(f_newcomb,s_newcomb,alternative = "less",conf.int = "true")

tt=sum(f_newcomb<=median(data))
tt
1-phyper(t-1,nx,ny,floor((nx+ny+1)/2))

