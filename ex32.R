setwd("C:/Users/timon/SDA---Assignment-2")
source("functions_Ch4.txt")
data32 = read.table("sample32.txt")
sample32 = c(data32$V1,data32$V2,data32$V3,data32$V4,data32$V5)

# logdensityestimate = function(data, dens_func){
#   logdata = log(data)
#   logdse = dens_func(logdata)
#   dse = 1/ # dit werkt niet want var t
#   return()
# }