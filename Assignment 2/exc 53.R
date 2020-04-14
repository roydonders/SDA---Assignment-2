setwd("/Users/Ko/Documents/SDA---Assignment-2")
#setwd("C:/Users/timon/SDA---Assignment-2")
source("functions_Ch3.txt")
source("functions_Ch5.txt")
newcomb = matrix(scan("newcomb.txt"))
f_newcomb = newcomb[1:20]
s_newcomb = newcomb[21:66]

par(mfrow =c(2,2),pty="s")
summary(newcomb)
hist(newcomb)
qqnorm(newcomb)
qqline(newcomb)
boxplot(newcomb)

par(mfrow =c(2,3),pty="s")
hist(f_newcomb)
qqnorm(f_newcomb)
hist(s_newcomb)
qqnorm(s_newcomb)
boxplot(f_newcomb,s_newcomb)

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

