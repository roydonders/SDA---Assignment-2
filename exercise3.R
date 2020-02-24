custom_qqline <- function(y, datax = FALSE, distribution = qnorm,
                          probs = c(0.25, 0.75), qtype = 7, ...)
{
  stopifnot(length(probs) == 2, is.function(distribution))
  y <- quantile(y, probs, names=FALSE, type=qtype, na.rm = TRUE)
  x <- distribution(probs)
  if (datax) {
    slope <- diff(x)/diff(y)
    int <- x[1L] - slope*y[1L]
  } else {
    slope <- diff(y)/diff(x)
    int <- y[1L]-slope*x[1L]
  }
  abline(int, slope, ...)
}



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
abline(37.225,2.557, col="yellow")
qqlogis(calfgirths, ylab = "Calf Girths (in cm)", col = "darkblue")
qqline(calfgirths, distribution = qlogis)

qqunif(anklegirths, ylab = "Ankle Girths (in cm)", col = "cornflowerblue")
qqline(anklegirths, distribution = qunif)
qqnorm(anklegirths, ylab = "Ankle Girths (in cm)", xlab="Quantiles of Normal", col = "blue1")
abline(qqline(anklegirths))
abline(23.15,1.705, col="yellow")
qqlogis(anklegirths, ylab = "Ankle Girths (in cm)", col = "darkblue")
qqline(anklegirths, distribution = qlogis)

par(mfrow=c(1,1))
diffgirths = calfgirths - anklegirths
qqnorm(diffgirths, ylab = "Difference Calf & Ankle Girths (in cm)", xlab="Quantiles of Normal", col = "blue1")
abline(custom_qqline(diffgirths))

SWvalues=numeric(1000)
for (i in 1:1000)
{
  x=rnorm(247)
  SWvalues[i]=shapiro.test(x)[[1]]
}
hist(SWvalues, col="deeppink1", main="Histogram of (Simulated) W-Values under H0")

shapiro.test(diffgirths)

swtest_samplesize = function(sampl, size){
  dat = sampl[1:size]
  pvalue = shapiro.test(dat)[[2]]
  return(pvalue)
}

plot_sample_size_SW = function(sampl, title="", color="black"){
  n = length(sampl)
  x_samplesize = 50:n
  y_pvalue = numeric(length(x_samplesize))
  for(x in x_samplesize)
  {
    i = x-49
    y_pvalue[i] = swtest_samplesize(sampl,x)
  }
  plot(x_samplesize,y_pvalue, xlab="Sample Size", ylab="P-Value SW-Test", ylim = c(0.0,max(y_pvalue)), main = title, col = color)
}

par(mfrow=c(1,2), pty="m")
plot_sample_size_SW(calfgirths, "SW-Test nog iets moois maken Calf Girths")
plot_sample_size_SW(anklegirths, "blah blah Ankle Girths")