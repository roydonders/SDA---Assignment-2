setwd("C:/Users/timon/SDA---Assignment-2")
source("functions_Ch4.txt")
data33 = read.table("sample33.txt")
sample33 = c(data33$V1,data33$V2,data33$V3,data33$V4,data33$V5)

CV_hs = seq(0.001,1,by=0.001)
CV_n = length(CV_hs)
CV_criterions = numeric(CV_n)

for(i in 1:CV_n){
  h = CV_hs[i]
  CV_criterions[i] = CV(h, sample33, "gaussian")
}

plot(CV_hs,CV_criterions, main= "Graph of CV Criteria for different bandwidths (sample33)", xlim = c(0.0,1.0), xlab="Bandwidth", ylab="CV Criterion")
lines(CV_hs,CV_criterions)
imin = which.min(CV_criterions)
h_CV = CV_hs[imin]


xgamma = seq(1,6,0.1)
ygamma = dgamma(xgamma, shape = 3, scale = 0.4)

par(mfrow=c(1,2))
dCV = density(sample33, bw=h_CV)
plot(dCV, ylim=c(0.0,1.0), main="KDE for sample33, bandwidth = h_CV")

h_OPT = h_opt(sample33)
dOPT = density(sample33, bw=h_OPT)
plot(dOPT, ylim=c(0.0,1.0), main="KDE for sample33, bandwidth = h_OPT", col = "seagreen4")


plot(dCV, xlim=c(3,6), ylim=c(0.0,1.0))
lines(xgamma+2, ygamma, col="red")
plot(dOPT, col="green", xlim=c(3,6), ylim=c(0.0,1.0))
lines(xgamma+2, ygamma, col="red")

# legend("bottomleft", 
#        legend = c("Group 1", "Group 2"), 
#        col = c(,)
#        )