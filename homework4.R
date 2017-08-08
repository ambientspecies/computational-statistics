# Problem 1
#a) 
B <- 10000
mu0 <- 2
power <- double(6)
mvec <- seq(5,30,5)
for(i in 1:6){
alpha <- 0.05
# generate B random samples of size m from the Gamma(3,1) distribution.
xb <- rgamma(mvec[i]*B, shape=3, scale=1)
# store the b-th random sample in the b-th row of the matrix.
xbmat <- matrix(xb, nrow=B, ncol=mvec[i])
# apply() function calculates the test statistic value for each row.
tb <- apply(xbmat, 1, function(x){t.test(x, mu=mu0)$statistic})
power[i] <- mean(abs(tb) > qt((alpha/2), df=(mvec[i]-1), lower.tail=FALSE))
}
power

#b) 
curve <- predict(loess(power~mvec))
plot(mvec,power,main="sample size vs. power")
lines(mvec, curve, col="red")
abline(h=0.8)

# sample size needed is 22 for power of 0.8

# Problem 2
m <- 10000
# generate a random sample from the Uniform(0, 1) distribution.
u <- runif(m)
v <- 1 - u
# getting estimates based on u and v.
y.i <- exp(-u)/(1+u^2)
ybar <- mean(y.i)
var.y <- sum((y.i-ybar)^2/m^2)
w.i <- exp(-v)/(1+v^2)
t.i <- (y.i+w.i)/2
tbar <- mean(t.i)
var.t <- sum((t.i - tbar)^2/m^2)
var.t
var.y
tbar
var.t/var.y

# Problem 3
#c)
m <- 10000
# Generating m random samples from Laplace(2)
x <- rexp(m,rate=2)-rexp(m,rate=2)
f.x <- (1/sqrt(2*pi))*exp(-x^2/2)
g.x <- exp(-abs(x)+.5*x^2)
phi.x <- exp(-2*abs(x))
expect <- mean((g.x*f.x)/phi.x)
expect

# Problem 4
# Write down

# Problem 5
# a) 
m <- 8
B <- 10000
mu <- 3
stdev <- 4
alpha <- 0.05
# generate B random samples of size m from the Exp(3) distribution.
xb <- rexp(m*B, rate=1/mu)
# store the b-th random sample in the b-th row of the matrix.
xbmat <- matrix(xb, nrow=B, ncol=m)
# apply() function calculates the confidence interval for each row.
CIs <- apply(xbmat, 1, function(x){t.test(x, mu=mu, conf.level=(1-alpha))$conf.int})
# Monte Carlo simulator for the empirical coverage rate.
CI.lower <- CIs[1,]
CI.upper <- CIs[2,]
alpha.sim <- mean(((CI.lower <= mu)*(CI.upper >= mu)))
alpha.sim

# [1] 0.8913
# Not reliable because non-normal distribution and small sample size.

# b) 
m <- 50
B <- 10000
mu <- 3
alpha <- 0.05
# generate B random samples of size m from the Exp(3) distribution.
xb <- rexp(m*B, rate=1/mu)
# store the b-th random sample in the b-th row of the matrix.
xbmat <- matrix(xb, nrow=B, ncol=m)
# apply() function calculates the confidence interval for each row.
CIs <- apply(xbmat, 1, function(x){t.test(x, mu=mu, conf.level=(1-alpha))$conf.int})
# Monte Carlo simulator for the empirical coverage rate.
CI.lower <- CIs[1,]
CI.upper <- CIs[2,]
alpha.sim <- mean(((CI.lower <= mu)*(CI.upper >= mu)))
alpha.sim

# [1] 0.9365
# Reliable because it is close to 0.95 and has large sample size.

# c)
m <- 20
B <- 10000
mu <- 6
alpha <- 0.05
# generate B random samples of size m from the Chisq(6) distribution.
xb <- rgamma(m*B, shape=3,scale=2)
# store the b-th random sample in the b-th row of the matrix.
xbmat <- matrix(xb, nrow=B, ncol=m)
# apply() function calculates the confidence interval for each row.
CIs <- apply(xbmat, 1, function(x){t.test(x, mu=mu, conf.level=(1-alpha))$conf.int})
# Monte Carlo simulator for the empirical coverage rate.
CI.lower <- CIs[1,]
CI.upper <- CIs[2,]
alpha.sim <- mean(((CI.lower <= mu)*(CI.upper >= mu)))
alpha.sim

# [1] 0.9374
# Reliable because close to 0.95 and somewhat normal distribution

# d)
m <- 10
B <- 10000
mu <- 0
alpha <- 0.05
# generate B random samples of size m from the t(50) distribution.
xb <- rt(m*B, df = 50)
# store the b-th random sample in the b-th row of the matrix.
xbmat <- matrix(xb, nrow=B, ncol=m)
# apply() function calculates the confidence interval for each row.
CIs <- apply(xbmat, 1, function(x){t.test(x, mu=mu, conf.level=(1-alpha))$conf.int})
# Monte Carlo simulator for the empirical coverage rate.
CI.lower <- CIs[1,]
CI.upper <- CIs[2,]
alpha.sim <- mean(((CI.lower <= mu)*(CI.upper >= mu)))
alpha.sim

#[1] 0.9512
# Reliable since it is very close to 0.95 and normal distribution
