#setwd("/Users/Ko/Documents/SDA---Assignment-2")
setwd("C:/Users/timon/SDA---Assignment-2")
edata = read.table("expensescrime.txt", header = TRUE, stringsAsFactors = FALSE) 
#part a
edatanostates = subset(edata, select=-c(state))
head(edatanostates)
pairs(edatanostates)

