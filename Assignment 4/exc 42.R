setwd("/Users/Ko/Documents/SDA---Assignment-2")
#setwd("C:/Users/timon/SDA---Assignment-2")
source("functions_Ch3.txt")
source("functions_Ch5.txt")
source("thromboglobulin.txt")

PRRP = thromboglobulin$PRRP
SDRP = thromboglobulin$SDRP
CTRP = thromboglobulin$CTRP
par(mfrow =c(2,2),pty="s")
B=1000
ePRRP_TStar = numeric(B)
for(i in 1:B){
  ePRRP_xstar = sample(PRRP,replace = TRUE)
  ePRRP_TStar[i] = mean(ePRRP_xstar)
}
summary(ePRRP_TStar)
summary(PRRP)
pvec <- seq(0,1,0.025)
quantile(ePRRP_TStar,pvec)
Tn_e - quantile(ePRRP_zstar,0.025)
hist(ePRRP_TStar, col="dark orange",main = "Emperical Bootstrap of PRRP",xlab = "mean PRRP")
Tn_e = mean(ePRRP_TStar)
ePRRP_zstar = ePRRP_TStar - Tn_e
c(Tn_e-quantile(ePRRP_zstar,0.975),Tn_e-quantile(ePRRP_zstar,0.025))
2*Tn_e-quantile(ePRRP_TStar,c(0.975,0.025))
first_e=c(Tn_e-quantile(ePRRP_zstar,0.975))
second_e=c(Tn_e-quantile(ePRRP_zstar,0.025))
abline(v=first_e,col="red")
abline(v=second_e,col="red")


mPRRP_TStar = numeric(B)
for(i in 1:B){
  mPRRP_xstar = sample(PRRP,replace = TRUE)
  mPRRP_TStar[i] = median(mPRRP_xstar)
}
summary(mPRRP_TStar)
quantile(mPRRP_TStar,pvec)
hist(mPRRP_TStar,col="blue",main = "Emperical Bootstrap of PRRP",xlab = "median PRRP")
Tn_m = median(mPRRP_TStar)
mPRRP_zstar = mPRRP_TStar - Tn_m
c(Tn_m-quantile(mPRRP_zstar,0.975),Tn_m-quantile(mPRRP_zstar,0.025))
2*Tn_m-quantile(mPRRP_TStar,c(0.975,0.025))
first_m=c(Tn_m-quantile(mPRRP_zstar,0.975))
second_m=c(Tn_m-quantile(mPRRP_zstar,0.025))
abline(v=first_m,col="red")
abline(v=second_m,col="red")

diff_SDRP_PRRP = numeric(B)
for(i in 1:B){
  mSDRP_xstar = sample(SDRP,replace = TRUE)
  nPRRP_xstar = sample(PRRP,replace = TRUE)
  diff_SDRP_PRRP[i] = mean(mSDRP_xstar) - mean(nPRRP_xstar)
}
summary(diff_SDRP_PRRP)
quantile(diff_SDRP_PRRP,pvec)
hist(diff_SDRP_PRRP,col = "red",main = "Bootstrap difference of SDRP and PRRP",xlab = "mean(SDRP) - mean(PRRP)")
Tn_d = mean(SDRP) - mean(PRRP) 
diff_zstar = diff_SDRP_PRRP - Tn_d
c(Tn_d-quantile(diff_zstar,0.975),Tn_d-quantile(diff_zstar,0.025))
2*Tn_d-quantile(diff_SDRP_PRRP,c(0.975,0.025))
first_d=c(Tn_d-quantile(diff_zstar,0.975))
second_d=c(Tn_d-quantile(diff_zstar,0.025))
abline(v=first_d,col="dark blue")
abline(v=second_d,col="dark blue")

hist(PRRP, prob=T)
abline(v = c(50.766, 70.407), col="orange")
abline(v = c(39.75, 64.25), col="dark blue")

diff2_SDRP_PRRP = numeric(B)
for(i in 1:B){
  mSDRP2_xstar = sample(SDRP,replace = TRUE)
  nPRRP2_xstar = sample(PRRP,replace = TRUE)
  diff2_SDRP_PRRP[i] = mean(mSDRP2_xstar) - mean(nPRRP2_xstar) -(mean(SDRP) - mean(PRRP))
}
summary(diff2_SDRP_PRRP)
quantile(diff2_SDRP_PRRP,pvec)
hist(diff2_SDRP_PRRP,col = "red",main = "Bootstrap difference of SDRP and PRRP",xlab = "mean difference SDRP and PRRP")
Tn_d = mean(SDRP) - mean(PRRP) 
diff2_zstar = diff2_SDRP_PRRP - Tn_d
c(Tn_d-quantile(diff2_zstar,0.975),Tn_d-quantile(diff2_zstar,0.025))
2*Tn_d-quantile(diff2_SDRP_PRRP,c(0.975,0.025))
first_d=c(Tn_d-quantile(diff_zstar,0.975))
second_d=c(Tn_d-quantile(diff_zstar,0.025))
abline(v=first_d,col="dark blue")
abline(v=second_d,col="dark blue")
