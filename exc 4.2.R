setwd("/Users/Ko/Documents/SDA---Assignment-2")
source("functions_Ch3.txt")
source("functions_Ch5.txt")
source("thromboglobulin.txt")

PRRP = thromboglobulin$PRRP
SDRP = thromboglobulin$SDRP
CTRP = thromboglobulin$CTRP

hist(PRRP)
B=1000
ePRRP_TStar = numeric(B)
for(i in 1:B){
  ePRRP_xstar = sample(PRRP[],replace = TRUE)
  ePRRP_TStar[i] = mean(ePRRP_xstar)
}

hist(ePRRP_TStar)
Tn = mean(ePRRP_xstar)
ePRRP_zstar = ePRRP_TStar - Tn
c(Tn-quantile(ePRRP_zstar,0.95),Tn-quantile(ePRRP_zstar,0.05))
2*Tn-quantile(ePRRP_TStar,c(0.95,0.05))
first=c(Tn-quantile(ePRRP_zstar,0.95))
second=c(Tn-quantile(ePRRP_zstar,0.05))
abline(v=first,col="red")
abline(v=second,col="red")

mPRRP_TStar = numeric(B)
for(i in 1:B){
  mPRRP_xstar = sample(PRRP[],replace = TRUE)
  mPRRP_TStar[i] = median(mPRRP_xstar)
}

hist(mPRRP_TStar)
Tn = median(mPRRP_xstar)
mPRRP_zstar = mPRRP_TStar - Tn
c(Tn-quantile(mPRRP_zstar,0.95),Tn-quantile(mPRRP_zstar,0.05))
2*Tn-quantile(mPRRP_TStar,c(0.95,0.05))
firstt=c(Tn-quantile(mPRRP_zstar,0.95))
secondd=c(Tn-quantile(mPRRP_zstar,0.05))
abline(v=firstt,col="red")
abline(v=secondd,col="red")

