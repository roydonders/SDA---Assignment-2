setwd("C:/Users/timon/SDA---Assignment-2")
source("functions_Ch4.txt")
data33 = read.table("sample33.txt")
sample33 = c(data33$V1,data33$V2,data33$V3,data33$V4,data33$V5)

hs = seq(0,1,by=0.01)
CV_criterions = numeric(length(is))

for(h in hs){
  CV_criterions = CV(h, sample33, "gaussian")
}
