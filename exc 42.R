setwd("/Users/Ko/Documents/SDA---Assignment-2")
source("functions_Ch3.txt")
source("functions_Ch5.txt")
source("thromboglobulin.txt")

PRRP = thromboglobulin$PRRP
SDRP = thromboglobulin$SDRP
CTRP = thromboglobulin$CTRP

B=10000
ePRRP_TStar = numeric(B)
for(i in 1:B){
  ePRRP_xstar = sample(PRRP,replace = TRUE)
  ePRRP_TStar[i] = mean(ePRRP_xstar)
}

hist(ePRRP_TStar)
Tn_e = mean(ePRRP_xstar)
ePRRP_zstar = ePRRP_TStar - Tn_e
c(Tn_e-quantile(ePRRP_zstar,0.95),Tn_e-quantile(ePRRP_zstar,0.05))
2*Tn_e-quantile(ePRRP_TStar,c(0.95,0.05))
first_e=c(Tn_e-quantile(ePRRP_zstar,0.95))
second_e=c(Tn_e-quantile(ePRRP_zstar,0.05))
abline(v=first_e,col="red")
abline(v=second_e,col="red")

mPRRP_TStar = numeric(B)
for(i in 1:B){
  mPRRP_xstar = sample(PRRP,replace = TRUE)
  mPRRP_TStar[i] = median(mPRRP_xstar)
}

hist(mPRRP_TStar)
Tn_m = median(mPRRP_xstar)
mPRRP_zstar = mPRRP_TStar - Tn_m
c(Tn_m-quantile(mPRRP_zstar,0.95),Tn_m-quantile(mPRRP_zstar,0.05))
2*Tn_m-quantile(mPRRP_TStar,c(0.95,0.05))
first_m=c(Tn_m-quantile(mPRRP_zstar,0.95))
second_m=c(Tn_m-quantile(mPRRP_zstar,0.05))
abline(v=first_m,col="red")
abline(v=second_m,col="red")

diff_SDRP_PRRP = numeric(B)
for(i in 1:B){
  mSDRP_xstar = sample(SDRP,replace = TRUE)
  nPRRP_xstar = sample(PRRP,replace = TRUE)
  diff_SDRP_PRRP[i] = mean(mSDRP_xstar) - mean(nPRRP_xstar)
}
hist(diff_SDRP_PRRP)
Tn_d = mean(mSDRP_xstar) - mean(nPRRP_xstar)
diff_zstar = diff_SDRP_PRRP - Tn_d
c(Tn_d-quantile(diff_zstar,0.95),Tn_d-quantile(diff_zstar,0.05))
2*Tn_d-quantile(diff_SDRP_PRRP,c(0.95,0.05))
first_d=c(Tn_d-quantile(diff_zstar,0.95))
second_d=c(Tn_d-quantile(diff_zstar,0.05))
abline(v=first_d,col="red")
abline(v=second_d,col="red")
