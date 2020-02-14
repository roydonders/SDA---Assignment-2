setwd("C:/Users/timon/SDA---Assignment-2")

data = read.table("body.dat.txt")
maledata = data[1:247,]
calfgirths = maledata[,19]
anklegirths = maledata[,20]
hist(calfgirths, prob=T, main = "Histogram of Calf Girths", xlab = "Calf Girths (in cm)")
hist(anklegirths, prob=T, main = "Histogram of Ankle Girths", xlab = "Ankle Girths (in cm)")
boxplot(calfgirths, main = "Boxplot of Calf Girths", ylab = "Calf Girths (in cm)")
boxplot(anklegirths, main = "Boxplot of Ankle Girths", ylab = "Ankle Girths (in cm)")