setwd("/Users/Ko/Documents/SDA---Assignment-2")
#setwd("C:/Users/timon/SDA---Assignment-2")
source("functions_Ch3.txt")
source("functions_Ch5.txt")
clouds = read.table("clouds.txt")

seeded = clouds$seeded.clouds
summary(seeded)
par(mfrow =c(2,2),pty="s")
hist(seeded,col="blue",main="Histogram of seeded",xlab="seeded")
boxplot(seeded,col="blue",main="Boxplot seeded",ylab="Sorted Data",xlab="seeded")
qqnorm(seeded,pch=20,col="blue",cex=1,ylab="Sorted Data",main="normal qqplot seeded")
qqline(seeded)
symplot(seeded,pch=20,col="blue",cex=1)
qqt(seeded,df=5,pch=20,col="blue",cex=1)
qqlaplace(seeded,pch=20,col="blue",cex=1)
qqexp(seeded,pch=20,col="blue",cex=1)

sd_seeded = sd(seeded)

B=1000
sd_TStar = numeric(B)
for(i in 1:B){
  sd_xstar= sample(seeded,replace = TRUE)
  sd_TStar[i] = sd(sd_xstar)
}
hist(sd_TStar)
summary(sd_TStar)

mad_s = mad(seeded)
mad_TStar = numeric(B)
for(i in 1:B){
  mad_xstar= sample(seeded,replace = TRUE)
  mad_TStar[i] = sd(mad_xstar)
}
hist(mad_TStar)
summary(mad_TStar)
sd(mad_TStar)

mean(seeded)
median(seeded)

l_seed = length(seeded)
s_seed = sum(seeded>119)
seed2 = seeded[seeded == 119]
l_seed2 = length(seed2)
#data zonder 119
seeded2 = seeded[seeded != 119]
l_s2 = length(seeded2)
s_s2 = sum(seeded2>119)
#signtest met k=m0
binom.test(s_seed,l_seed-l_seed2,alt="g")
#signtest zonder k=m0
binom.test(s_seed,l_seed-l_seed2,alt="g")
binom.test(s_s2,l_s2,alt="g")
#met
t.test(seeded,mu=119,alt="g")
#zonder
t.test(seeded2,mu=119,alt="g")
#met
wilcox.test(seeded,mu=119,alt="g")
#zonder
wilcox.test(seeded2,mu=119,alt="g")
            
binom.test(s_seed,l_seed,alt="g",conf.level = 0.99) 
binom.test(s_seed,l_seed-l_seed2,alt="g",conf.level = 0.99) 
binom.test(s_s2,l_s2,alt="g",conf.level = 0.99) 

t.test(seeded,mu=119,alt="g",conf.level = 0.99)   
t.test(seeded2,mu=119,alt="g",conf.level = 0.99)  

wilcox.test(seeded,mu=119,alt="g",conf.level = 0.99)
wilcox.test(seeded2,mu=119,alt="g",conf.level = 0.99)         
            
            