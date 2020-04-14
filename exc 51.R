#setwd("/Users/Ko/Documents/SDA---Assignment-2")
setwd("C:/Users/timon/SDA---Assignment-2")
grades = scan("statgrades.txt")
n = length(grades)
grades==6
adjgrades = grades[-26]
t = sum(adjgrades>6)
binom.test(t,n-1,alternative = "g")

grades==6.5
t = sum(grades>6.5)
binom.test(t,n,alternative = "g")
binom.test(t,n,alternative = "l")

k = sum(grades>=7)
binom.test(k,n,alternative = "l")