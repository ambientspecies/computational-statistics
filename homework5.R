# Problem 1
# a)
B <- 10000
mu0 <- 3
mu <- seq(1,6,0.5)
power <- double(length(mu))
m = 10
for(i in 1:length(mu)){
alpha <- 0.05
# generate B random samples of size m from the Gamma(mu,1) distribution.
xb <- rgamma(m*B, shape=mu[i], scale=1)
# store the b-th random sample in the b-th row of the matrix.
xbmat <- matrix(xb, nrow=B, ncol=m)
# apply() function calculates the test statistic value for each row.
tb <- apply(xbmat, 1, function(x){t.test(x, mu=mu0)$statistic})
power[i] <- mean(abs(tb) > qt((alpha/2), df=(m-1), lower.tail=FALSE))
}
power

# b)
curve <- predict(loess(power~mu))
plot(mu,power,main="mu versus power")
lines(mu,curve,col="red")
abline(h=0.8)

# c)
# To get a power of the test = 0.8, you would need mu_a to be around 1.7 and mu_b to be 5.2

# Problem 2.
# d)
# Semi-parametric bootstrap for median estimation of variance for Exp(3)
m <- 10
# create B = 10000 resamples.
B <- 10000
thetas <- replicate(B, expr = {
	x <- rexp(m, rate = 1/3)
	median(x)
	})
# variance estimation
var(thetas)

# e)
# Semi-parametric bootstrap for mean estimation of variance for Exp(3)
m <- 10
# create B = 10000 resamples.
B <- 10000
thetas <- replicate(B, expr = {
	x <- rexp(m, rate = 1/3)
	mean(x)
	})
# variance estimation
var(thetas)

# bias estimation
mean(thetas) - 3

# Problem 3.
# 95% nonparametric percentile bootstrap-based CI.
install.packages("boot")
library(boot)
sample <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)
theta <- mean(sample)
B <- 1000
mean.fun <- function(x, i){return(mean(x[i]))}
results <- boot(data=sample, statistic=mean.fun, R=B)
thetas <- results$t
quantile(thetas, probs=c(0.025, 0.975))

#      2.5%     97.5% 
# 44.82708 186.01458 

# 95% semiparametric percentile bootstrap-based CI.
sample <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)
lambda <- mean(sample)
m <- 10
# create B = 10000 resamples.
B <- 10000
thetas <- replicate(B, expr = {
	x <- rexp(m, rate = 1/lambda)
	mean(x)
	})
quantile(thetas, probs=c(0.025, 0.975))

#     2.5%     97.5% 
# 52.59618 185.19365 


# 95% bootstrap t CI.

### Example 7.12 (Bootstrap t confidence interval)
    boot.t.ci <-
    function(x, B = 1000, R = 200, level = .95, statistic){
        #compute the bootstrap t CI
        x <- as.matrix(x);  n <- nrow(x)
        stat <- numeric(B); se <- numeric(B)

        boot.se <- function(x, R, f) {
            #local function to compute the bootstrap
            #estimate of standard error for statistic f(x)
            x <- as.matrix(x); m <- nrow(x)
            th <- replicate(R, expr = {
                i <- sample(1:m, size = m, replace = TRUE)
                f(x[i, ])
                })
            return(sd(th))
        }

        for (b in 1:B) {
            j <- sample(1:n, size = n, replace = TRUE)
            y <- x[j, ]
            stat[b] <- statistic(y)
            se[b] <- boot.se(y, R = R, f = statistic)
        }
        stat0 <- statistic(x)
        t.stats <- (stat - stat0) / se
        se0 <- sd(stat)
        alpha <- 1 - level
        Qt <- quantile(t.stats, c(alpha/2, 1-alpha/2), type = 1)
        names(Qt) <- rev(names(Qt))
        CI <- rev(stat0 - Qt * se0)
    }

stat <- function(dat) {
	mean(dat) }
x <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)
a <-
    boot.t.ci(x, B = 1000, R = 200, level = .95, statistic=stat)

#     2.5%     97.5% 
# 43.79429 287.11852 