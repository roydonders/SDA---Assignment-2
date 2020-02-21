setwd("C:/Users/timon/SDA---Assignment-2")

data = read.table("body.dat.txt")
maledata = data[1:247,]
calfgirths = maledata[,19]
anklegirths = maledata[,20]
par(mfrow=c(2,2))
hist(calfgirths, prob=T, main = "Histogram of Calf Girths", xlab = "Calf Girths (in cm)", col="orange")
hist(anklegirths, prob=T, main = "Histogram of Ankle Girths", xlab = "Ankle Girths (in cm)", col="turquoise1")
boxplot(calfgirths, main = "Boxplot of Calf Girths", ylab = "Calf Girths (in cm)", col="orange")
boxplot(anklegirths, main = "Boxplot of Ankle Girths", ylab = "Ankle Girths (in cm)", col="turquoise1")

par(mfrow=c(1,2), pty="s")
qqplot(calfgirths,anklegirths, xlab = "Calf Girths (in cm)", ylab = "Ankle Girths (in cm)",col ="black", main="Two-Sample QQ-Plot")
qqplot(calfgirths,anklegirths, xlab = "Calf Girths (in cm)", ylab = "Ankle Girths (in cm)",col ="black", main="Two-Sample QQ-Plot", sub="With the line y=0.6x+1")
abline(1,0.6, col="gray60")

source("functions_Ch3.txt")
par(mfrow=c(1,3), pty="s")
qqunif(calfgirths, ylab = "Calf Girths (in cm)", col = "cornflowerblue")
qqline(calfgirths, distribution = qunif)
qqnorm(calfgirths, ylab = "Calf Girths (in cm)", xlab="Quantiles of Normal", col = "blue1")
abline(qqline(calfgirths))
abline(37.3,2.55, col="yellow")
qqlogis(calfgirths, ylab = "Calf Girths (in cm)", col = "darkblue")
qqline(calfgirths, distribution = qlogis)

#hieronder het probleem van abline/qqline evenaren, ingezoomd op normal qq plot
par(mfrow=c(1,1), pty="s") #pty = s zorgt voor een square plot
qqnorm(calfgirths, ylab = "Calf Girths (in cm)", xlab="Quantiles of Normal", col = "blue1")
abline(qqline(calfgirths))
abline(37.3,2.55, col="yellow")
