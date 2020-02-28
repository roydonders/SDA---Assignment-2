getwd()
setwd("/Users/Ko/Documents/SDA---Assignment-2")
source("functions_Ch4.txt")
source("functions_Ch5.txt")
data31 <- read.table("sample31.txt")
data32 <- read.table("sample32.txt")
data33 <- read.table("sample33.txt")

sample31 = c(data31$V1,data31$V2,data31$V3,data31$V4,data31$V5)
sample32 <- c(data32$V1,data32$V2,data32$V3,data32$V4,data32$V5)
sample33 <- c(data33$V1,data33$V2,data33$V3,data33$V4,data33$V5)

plot(density(c(sample31),bw=1.920362))
plot(density(sample32,bw=0.2264368))
plot(density(sample33,bw=0.268657))
hist(sample31)
hist(sample32)
hist(sample33)
h_opt(sample33)
CV(0.268657,sample33,"gaussian")


min_sd = min(sd(sample31),(IQR(sample31))/1.34)
(4*pi)^(-1/10)*(3/8*pi^(-1/2))^(-1/5) * min_sd * length(sample31)^(-1/5)

