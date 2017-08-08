# Problem 1
# b)
u <- runif(6)
z1 <- sqrt(-2*log(u[1]))*cos(2*pi*u[2])
z2 <- sqrt(-2*log(u[1]))*sin(2*pi*u[2])
z3 <- sqrt(-2*log(u[3]))*cos(2*pi*u[4])
z4 <- sqrt(-2*log(u[3]))*sin(2*pi*u[4])
z5 <- sqrt(-2*log(u[5]))*cos(2*pi*u[6])
z6 <- sqrt(-2*log(u[5]))*sin(2*pi*u[6])
# x^2(2)
U <- z1^2 + z2^2
# x^(4)
V <- z3^2 + z4^2 + z5^2 + z6^2
# F(2,4)
f <- (U/2)/(V/4)
f

# Problem 3

m <- 1000
# generate a random sample from the Uniform(0, 0.5) distribution.
x <- runif(m, min=0, max=0.5)
y <- exp(-x)
y.bar <- mean(y)
theta <- y.bar * 0.5
theta
# [1] 0.3915233
exp(-0) - exp(-0.5)
# [1] 0.3934693
# variance of theta
sigma2.y <- mean((y - y.bar)^2)
var.theta <- (sigma2.y * (0.5)^2)/m
var.theta
# [1] 3.099595e-06

# generate a random sample from the exponential distribution.
x2 <- rexp(m)
y2 <- exp(-x2)
y2.bar <- mean(y2)
theta2 <- y2.bar * 0.5
theta2
# [1] 0.2482859
# variance of theta 2
sigma2.y2 <- mean((y2 - y2.bar)^2)
var.theta2 <- (sigma2.y2 * (0.5)^2)/m
var.theta2
# [1] 2.109265e-05

# Using Uniform(0,0.5) distribution has a smaller variance of theta.

# Problem 4.
###################################################
### Estimating the cdf of the Beta(3,3) distribution and 
### its (pointwise) 95% confidence intervals

m <- 1000
# generate a random sample from the Beta(3,3) distribution.
x <- rbeta(m,shape1=3,shape2=3)
# generate a sequence of points c to evaluate F(c).
# Here, c in [0.1, 0.9] with an increment of 0.1.
c <- seq(0.1, 0.9, .1)
# F.c stores the F(c) values for different values of c.
F.c <- double(length(c))
# calculate an indicator function value at each point of c.
for (i in 1:length(c)){
	# we do this one by one.
	F.c[i] <- mean(x < c[i])
	}
# Plotting the estimate and 95% confidence intervals on a graph.
alpha <- 0.05
upper.bound <- F.c+qnorm((alpha/2),lower.tail=FALSE)*sqrt(F.c*(1-F.c)/m)
lower.bound <- F.c-qnorm((alpha/2),lower.tail=FALSE)*sqrt(F.c*(1-F.c)/m)
plot(x=c, y=F.c, xlim=c(0.1,0.9), ylim=c(-0.1, 1.1), lwd=2, main="Estimated F(c)", pch=3, col="white")
lines(x=c, y=F.c, lty=1, lwd=2, col="black")
lines(x=c, y=upper.bound, lty=2, col="blue")
lines(x=c, y=lower.bound, lty=2, col="blue")
lines(x=c, y=pbeta(c,3,3), lty=3, col="red")


# Problem 5.
# b)

m <- 30
B <- 10000
r <- 1/4
alpha <- 0.05
# generate B random samples of size m from the Gamma(r,1) distribution.
xb <- rgamma(m,shape=r,scale=1)
# store the b-th random sample in the b-th row of the matrix.
xbmat <- matrix(xb, nrow=B, ncol=m)
# apply() function calculates the test statistic value for each row.
tb <- apply(xbmat, 1, function(x){t.test(x, mu=r)$statistic})
# Monte Carlo simulation for the empirical Type I error rate.
alpha.sim <- mean(abs(tb) > qt((alpha/2), df=(m-1), lower.tail=FALSE))
alpha.sim

# c)
gamma1 <- seq(0.5,4.0,0.5)
type1 <- c(0.7,0.6,0.6,0.6,0.3,0.6,0.7,0.8)
curve <- predict(loess(type1~gamma1))
plot(gamma1,type1, main="Gamma vs. Type 1 error rate")
lines(gamma1, curve, col="red")
