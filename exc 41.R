setwd("/Users/Ko/Documents/SDA---Assignment-2")
source("functions_Ch3.txt")
source("functions_Ch5.txt")
databw = read.table("birthweight.txt")
bw = c(databw$V1)
hist(bw)
median(bw)

#unknown distribution so emperical bootstrap
B=100000
ebw_TStar = numeric(B)
for(i in 1:B){
  exstar= sample(bw,replace = TRUE)
  ebw_TStar[i] = median(exstar)
}
hist(ebw_TStar)
sd(ebw_TStar)

#parametric bootstrap
pbw_TStar = numeric(B)
for(i in 1:B){
  pxstar = rexp(bw,rate = (1/mean(bw)))
  pbw_TStar[i] = median(pxstar)
}
hist(pbw_TStar)
sd(pbw_TStar) #sd is heel laag mss klopt niet(ERROR lecture 5)


