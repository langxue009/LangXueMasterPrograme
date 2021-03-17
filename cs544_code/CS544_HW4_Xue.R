
# HW4

# 1a

prob1 = dbinom(0:5, size = 5, prob = 0.5)

# 1b

dbinom(2, size = 5, prob = 0.5)

# 1c

sum(dbinom(2:5, size = 5, prob = 0.5))

# 1d

prob2 = dbinom(0:5, size = 5, prob = 0.75)
dbinom(2, size = 5, prob = 0.75)
sum(dbinom(2:5, size = 5, prob = 0.75))

# 1e

prob3 = dbinom(0:5, size = 5, prob = 0.25)
dbinom(2, size = 5, prob = 0.25)
sum(dbinom(2:5, size = 5, prob = 0.25))

# 1f

# distributions from previous parts

par(mfrow=c(3,2), oma = c(0, 0, 2, 0))

# PMF Plot for p = 0.5
plot(0:5, prob1, type = "h", ylim = c(0, 0.5), col = "blue", xlab = "x", ylab = "PMF", main = "p=0.5")
points(0:5, prob1, pch = 16)

# CDF Plot for p = 0.5
cumprob1 = cumsum(prob1)
plot(stepfun(0:5, c(0,cumprob1)), verticals = FALSE, pch = 16, ylab = "CDF", main = "p=0.5")

# PMF Plot for p = 0.75
plot(0:5, prob2, type = "h", ylim = c(0, 0.5), col = "blue", xlab = "x", ylab = "PMF", main = "p=0.75")
points(0:5, prob2, pch = 16)

# CDF Plot for p = 0.75
cumprob2 = cumsum(prob2)
plot(stepfun(0:5, c(0,cumprob2)), verticals = FALSE, pch = 16, ylab = "CDF", main = "p=0.75")

# PMF Plot for p = 0.25
plot(0:5, prob3, type = "h", ylim = c(0, 0.5), col = "blue", xlab = "x", ylab = "PMF", main = "p=0.25")
points(0:5, prob3, pch = 16)

# CDF Plot for p = 0.25
cumprob3 = cumsum(prob3)
plot(stepfun(0:5, c(0,cumprob3)), verticals = FALSE, pch = 16, ylab = "CDF", main = "p=0.25")

par(mfrow=c(1,1))
mtext("Part1 - Binomial Distributions", 
      outer = TRUE, cex = 1.5)

# 1g

set.seed(1234)
sample1 = rbinom(n = 1000, size = 5, prob = 0.5)
sample2 = rbinom(n = 1000, size = 5, prob = 0.75)
sample3 = rbinom(n = 1000, size = 5, prob = 0.25)

par(mfrow=c(1,3))

barplot(table(sample1), main = 'p=0.5', ylim = c(0, 450), xlab = "x")
barplot(table(sample2), main = 'p=0.75', ylim = c(0, 450), xlab = "x")
barplot(table(sample3), main = 'p=0.25', ylim = c(0, 450), xlab = "x")

par(mfrow=c(1,1))
mtext("Part1 - Barplot of 1000 simulations from Binomial Distributions", 
      outer = TRUE, cex = 1.5)

# 2a

nbprob1 <- round(dnbinom(0:7, size = 3, prob = 0.5), 4)

# 2b
dnbinom(4, size = 3, prob = 0.5)

# 2c
sum(dnbinom(0:4, size = 3, prob = 0.5))

# 2d

nbprob2 <- round(dnbinom(0:7, size = 3, prob = 0.75), 4)
dnbinom(4, size = 3, prob = 0.75)
sum(dnbinom(0:4, size = 3, prob = 0.75))

# 2e

nbprob3 <- round(dnbinom(0:7, size = 3, prob = 0.25), 4)
dnbinom(4, size = 3, prob = 0.25)
sum(dnbinom(0:4, size = 3, prob = 0.25))

# distributions from previous parts

par(mfrow=c(3,2), oma = c(0, 0, 2, 0))

# PMF Plot for p = 0.5
plot(0:7, nbprob1, type = "h", ylim = c(0, 0.5), col = "blue", xlab = "x", ylab = "PMF", main = "p=0.5")
points(0:7, nbprob1, pch = 16)

# CDF Plot for p = 0.5
cumprob1 = cumsum(nbprob1)
plot(stepfun(0:7, c(0,cumprob1)), verticals = FALSE, pch = 16, ylab = "CDF", main = "p=0.5")

# PMF Plot for p = 0.75
plot(0:7, nbprob2, type = "h", ylim = c(0, 0.5), col = "blue", xlab = "x", ylab = "PMF", main = "p=0.75")
points(0:7, nbprob2, pch = 16)

# CDF Plot for p = 0.75
cumprob2 = cumsum(nbprob2)
plot(stepfun(0:7, c(0,cumprob2)), verticals = FALSE, pch = 16, ylab = "CDF", main = "p=0.75")

# PMF Plot for p = 0.25
plot(0:7, nbprob3, type = "h", ylim = c(0, 0.5), col = "blue", xlab = "x", ylab = "PMF", main = "p=0.25")
points(0:7, nbprob3, pch = 16)

# CDF Plot for p = 0.25
cumprob3 = cumsum(nbprob3)
plot(stepfun(0:7, c(0,cumprob3)), verticals = FALSE, pch = 16, ylab = "CDF", main = "p=0.25")

par(mfrow=c(1,1))
mtext("Part2 - Negative Binomial Distributions", 
      outer = TRUE, cex = 1.5)

# 2f

set.seed(4321)
sample1 = rnbinom(n = 1000, size = 3, prob = 0.5)
sample2 = rnbinom(n = 1000, size = 3, prob = 0.75)
sample3 = rnbinom(n = 1000, size = 3, prob = 0.25)

par(mfrow=c(1,3))

barplot(table(sample1), main = 'p=0.5', ylim = c(0, 450), xlab = "x")
barplot(table(sample2), main = 'p=0.75', ylim = c(0, 450), xlab = "x")
barplot(table(sample3), main = 'p=0.25', ylim = c(0, 450), xlab = "x")

par(mfrow=c(1,1))
mtext("Part2 - Barplot of 1000 simulations from Negative Binomial Distributions", 
      outer = TRUE, cex = 1.5)

# 3a

hyprob1 <- data.frame(X = 0:20, `P(X)` = dhyper(0:20, m = 60, n = 40, k = 20))

# 3b

dhyper(10, m = 60, n = 40, k = 20)
choose(60,10)*choose(40,10)/choose(100,20)

# 3c

sum(dhyper(10:20, m = 60, n = 40, k = 20))

# 3d

hyprob2 <- data.frame(X = 0:20, `P(X)` = dhyper(0:20, m = 40, n = 60, k = 20))
dhyper(10, m = 40, n = 60, k = 20)
sum(dhyper(10:20, m = 40, n = 60, k = 20))

# 3e

hyprob3 <- data.frame(X = 0:20, `P(X)` = dhyper(0:20, m = 20, n = 80, k = 20))
choose(20,10)*choose(80,10)/choose(100,20)
dhyper(10, m = 20, n = 80, k = 20)
sum(dhyper(10:20, m = 20, n = 80, k = 20))

# 3f

# distributions from previous parts

par(mfrow=c(3,2), oma = c(0, 0, 2, 0))

# PMF Plot for 60 multiple choices and 40 programming questions
plot(0:20, hyprob1$P.X., type = "h", ylim = c(0, 0.3), col = "blue", xlab = "x", 
     ylab = "PMF", main = "60 multiple choices and 40 programming questions")
points(0:20, hyprob1$P.X., pch = 16)

# CDF Plot for 60 multiple choices and 40 programming questions
cumprob1 = cumsum(hyprob1$P.X.)
plot(stepfun(0:20, c(0,cumprob1)), verticals = FALSE, pch = 16, ylab = "CDF",
     main = "60 multiple choices and 40 programming questions")

# PMF Plot for 40 multiple choices and 60 programming questions
plot(0:20, hyprob2$P.X., type = "h", ylim = c(0, 0.3), col = "blue", xlab = "x", 
     ylab = "PMF", main = "40 multiple choices and 60 programming questions")
points(0:20, hyprob2$P.X., pch = 16)

# CDF Plot for 40 multiple choices and 60 programming questions
cumprob2 = cumsum(hyprob2$P.X.)
plot(stepfun(0:20, c(0,cumprob2)), verticals = FALSE, pch = 16, ylab = "CDF",
     main = "40 multiple choices and 60 programming questions")

# PMF Plot for 20 multiple choices and 80 programming questions
plot(0:20, hyprob3$P.X., type = "h", ylim = c(0, 0.3), col = "blue", xlab = "x", 
     ylab = "PMF", main = "20 multiple choices and 80 programming questions")
points(0:20, hyprob3$P.X., pch = 16)

# CDF Plot for 20 multiple choices and 80 programming questions
cumprob3 = cumsum(hyprob3$P.X.)
plot(stepfun(0:20, c(0,cumprob3)), verticals = FALSE, pch = 16, ylab = "CDF",
     main = "20 multiple choices and 80 programming questions")

par(mfrow=c(1,1))
mtext("Part3 - Hypergeometric Distributions", 
      outer = TRUE, cex = 1.5)

# Simulation

set.seed(2020)
sample1 = rhyper(nn = 1000, m = 60, n = 40, k = 20)
sample2 = rhyper(nn = 1000, m = 40, n = 60, k = 20)
sample3 = rhyper(nn = 1000, m = 20, n = 80, k = 20)

par(mfrow=c(1,3))

barplot(table(sample1), main = "60 multiple choices and 40 programming questions", ylim = c(0, 250), xlab = "x")
barplot(table(sample2), main = "40 multiple choices and 60 programming questions", ylim = c(0, 250), xlab = "x")
barplot(table(sample3), main = "20 multiple choices and 80 programming questions", ylim = c(0, 250), xlab = "x")

par(mfrow=c(1,1))
mtext("Part3 - Barplot of 1000 simulations from Hypergeometric Distributions", 
      outer = TRUE, cex = 1.5)

# 4a

dpois(8, lambda = 10)

# 4b

sum(dpois(0:8, lambda = 10))

# 4c

sum(dpois(6:12, lambda = 10))

# 4d

poisprob1 = data.frame(X = 0:20, prob = dpois(0:20, lambda = 10))

# PMF Plot 
plot(0:20, poisprob1$prob, type = "h", ylim = c(0, 0.15), col = "blue", xlab = "x", 
     ylab = "PMF", main = "PMF for the first 20 questions")
points(0:20, poisprob1$prob, pch = 16)

# 4e

set.seed(2020)
rpois_value = rpois(50, lambda = 10)

par(mfrow=c(1,2))
barplot(table(rpois_value), xlab = "x", ylab = 'PMF', main = 'The number of questions \nthe professor gets per day over 50 days')
boxplot(rpois_value, ylab = 'x', main = "Boxplot of the number of questions \nthe professor gets per day over 50 days")
par(mfrow=c(1,1))

# 5a

x = seq(70, 130, 1)
y = dnorm(x, mean = 100, sd = 10)
plot(x, y, type = "l", col = 'blue', xlab = "x", ylab = 'PDF', main = "Distribution of N(100, 10^2) within 3 sd")

# 5b

1-pnorm(120, mean = 100, sd = 10)

# 5c

pnorm(90, mean = 100, sd = 10) - pnorm(80, mean = 100, sd = 10)

# 5d

pnorm(110, mean = 100, sd = 10) - pnorm(90, mean = 100, sd = 10)
pnorm(120, mean = 100, sd = 10) - pnorm(80, mean = 100, sd = 10)
pnorm(130, mean = 100, sd = 10) - pnorm(70, mean = 100, sd = 10)

# 5e

c(qnorm(0.05, mean = 100, sd = 10), qnorm(0.95, mean = 100, sd = 10))

# 5f

qnorm(0.95, mean = 100, sd = 10)

# 5g

set.seed(2021)
x = rnorm(10000, mean = 100, sd = 10)
hist(x, xlab = "x", ylab = "frequency", main = "Histogram of the spend from 10,000 visitors")

