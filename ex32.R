setwd("C:/Users/timon/SDA---Assignment-2")
source("functions_Ch4.txt")
data32 = read.table("sample32.txt")
sample32 = c(data32$V1,data32$V2,data32$V3,data32$V4,data32$V5)

# logkde = function(data, bandw, kernl="gaussian"){
#   logdata = log(data)
#   d = density(logdata, bw=as.numeric(bandw), kernel=kernl)
#   lkde = approxfun(d)
#   return(lkde) 
# }
# 
# kdefunction_applylog = function(data, bw, kernel="gaussian"){
#   fhaty = logkde(data, bandw=bw, kernl=kernel)
#   fhatx = function(t){(1/t)*fhaty(log(t))}
#   return(fhatx)
# }
library(logKDE)

h = h_opt(sample32)
fhat = kdefunction_applylog(sample32,h)
# logdensity werkt prima
# oude methode benaderde te veel gebruikmakend van approxfun

# logdensityestimate = function(data, dens_func){
#   logdata = log(data)
#   logdse = dens_func(logdata)
#   dse = 1/ # dit werkt niet want var t
#   return()
# }