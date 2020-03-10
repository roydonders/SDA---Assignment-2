getwd()
setwd("/Users/Ko/Documents/SDA---Assignment-2")
source("functions_Ch4.txt")
source("functions_Ch5.txt")
data31 = read.table("sample31.txt")
data32 = read.table("sample32.txt")
data33 = read.table("sample33.txt")

sample31 = c(data31$V1,data31$V2,data31$V3,data31$V4,data31$V5)
sample32 = c(data32$V1,data32$V2,data32$V3,data32$V4,data32$V5)
sample33 = c(data33$V1,data33$V2,data33$V3,data33$V4,data33$V5)

plot(density(c(sample31),bw= multi_bw/1.6))
plot(density(sample32,bw=0.2264368))
plot(density(sample33,bw=0.268657))
par(mfrow=c(1,1),pty="s")
hist(sample31,prob=T,col="blue",xlab = "Observation")
hist(sample32)
hist(sample33)
h_opt(sample31)
CV(0.268657,sample33,"gaussian")

IQR_31 = IQR(sample31)/1.34

min_sd = min(sd(sample31), IQR_31)
multi_bw = (4*pi)^(-1/10)*(3/8*pi^(-1/2))^(-1/5) * min_sd * length(sample31)^(-1/5)

SWvalues=numeric(1000)
for (i in 1:1000)
{
  x=rnorm(90)
  SWvalues[i]=shapiro.test(x)[[1]]
}
par(mfrow=c(1,1),pty="s")
hist(SWvalues, col="deeppink1", main="Histogram of (Simulated) W-Values under H0")

shapiro.test(sample31)
par(mfrow=c(2,3),pty="s")
plot(density(c(sample31),bw= multi_bw), col="red",main="Multimodal, standard R bandwidth")
plot(density(c(sample31),bw= multi_bw/1.2),col="red",main="Multimodal, bandwidth = h_opt/1.2")
plot(density(c(sample31),bw= multi_bw/1.5),col="red",main="Multimodal, bandwidth = h_opt/1.5")
plot(density(c(sample31),bw= multi_bw/1.6),col="red",main="Multimodal, bandwidth = h_opt/1.6")
plot(density(c(sample31),bw= multi_bw/2),col="red",main="Multimodal, bandwidth = h_opt/2")
plot(density(c(sample31),bw= multi_bw/4),col="red",main="Multimodal, bandwidth = h_opt/4")

