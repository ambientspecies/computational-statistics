# Problem 1.
# Metropolis-Hastings algorithm for Rayleigh distribution with sigma = 2.
# Proposal distribution Y~X^2(Xt)
library(VGAM)
# Number of initial observations to be discarded (burn-ins), including x_0.
burn.in <- 1000
# Number of observations to be taken. In total, 
# m + burn.in observations will be generated including x_0.
m <- 10000
sigma <- 2
# A vector storing the chain.
x <- double(burn.in + m)
# x_0. df is chosen somewhat arbitrarily. 
x[1] <- rchisq(1, df=1)
# A sequence of u's.
u <- runif(burn.in + m)
# Counts the number of rejections
k <- 0

for(i in 2:(burn.in+m)){
	xt <- x[i-1]
	y <- rchisq(1, df = xt)
	numerator <- drayleigh(y, scale=sigma) * dchisq(xt, df=y)
	denominator <- drayleigh(xt, scale=sigma) * dchisq(y, df=xt)
	x[i]<-xt
	if(u[i] <= numerator/denominator){
		x[i]<-y
		k<-k+1
	}
}

# Final chain after discarding burn-ins
x <- x[-c(1:burn.in)]
ts.plot(x, main="Plot of the Chain", xlab="Chain")

# Comparing the distribution of the chain to the actual distribution.
a <- ppoints(100) ### generates percentage points.
QR <- qrayleigh(a, scale=sigma) ### generate percentiles for Rayleigh distribution.
Q <- quantile(x, a) ### calculate percentiles for the chain.

dev.new()
par(mfrow=c(1,2))

# Q-Q plot to compare the percentiles of the chain to the theoretical one.
qqplot(QR, Q, main="Rayleigh Q-Q Plot", xlab="Rayleigh Quantiles", ylab="Sample Quantiles")
abline(lm(Q~0+QR)) ### gives the best fit line with zero intercept.

# Histogram of the chain with the pdf of the Rayleigh distribution overlaid. 
hist(x, breaks="scott", main="Rayleigh Histogram", xlab="", freq=FALSE)
lines(QR, drayleigh(QR, scale=sigma))

# Problem 2.
# Metropolis-Hastings algorithm for Rayleigh distribution with sigma = 2.
# Proposal distribution Y~Gamma(Xt, 1)
library(VGAM)
# Number of initial observations to be discarded (burn-ins), including x_0.
burn.in <- 1000
# Number of observations to be taken. In total, 
# m + burn.in observations will be generated including x_0.
m <- 10000
sigma <- 2
# A vector storing the chain.
x <- double(burn.in + m)
# x_0. df is chosen somewhat arbitrarily. 
x[1] <- rgamma(1, shape = xt, scale = 1)
# A sequence of u's.
u <- runif(burn.in + m)
# Counts the number of rejections
k <- 0

for(i in 2:(burn.in+m)){
	xt <- x[i-1]
	y <- rgamma(1, shape = xt, scale = 1)
	numerator <- drayleigh(y, scale=sigma) * dgamma(xt, shape = y, scale = 1)
	denominator <- drayleigh(xt, scale=sigma) * dgamma(y, shape = xt, scale = 1)
	x[i]<-xt
	if(u[i] <= numerator/denominator){
		x[i]<-y
		k<-k+1
	}
}

# Final chain after discarding burn-ins
x <- x[-c(1:burn.in)]
ts.plot(x, main="Plot of the Chain", xlab="Chain")

# Comparing the distribution of the chain to the actual distribution.
a <- ppoints(100) ### generates percentage points.
QR <- qrayleigh(a, scale=sigma) ### generate percentiles for Rayleigh distribution.
Q <- quantile(x, a) ### calculate percentiles for the chain.

dev.new()
par(mfrow=c(1,2))

# Q-Q plot to compare the percentiles of the chain to the theoretical one.
qqplot(QR, Q, main="Rayleigh Q-Q Plot", xlab="Rayleigh Quantiles", ylab="Sample Quantiles")
abline(lm(Q~0+QR)) ### gives the best fit line with zero intercept.

# Histogram of the chain with the pdf of the Rayleigh distribution overlaid. 
hist(x, breaks="scott", main="Rayleigh Histogram", xlab="", freq=FALSE)
lines(QR, drayleigh(QR, scale=sigma))
