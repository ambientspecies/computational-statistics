# Problem 1
# i)
# The traditional two-sample t-test that assumes equal variances 
ttest1 <- function(x,y){
return(t.test(x,y,mu=0,var.equal=TRUE)$p.value)
}

# ii)
# The traditional two-sample t-test that does not assume equal variances 
ttest2 <- function(x,y){
return(t.test(x,y,mu=0,var.equal=FALSE)$p.value)
}

# iii)
# The permutation-based two-sample t-test that assumes equal variances
ttest3 <- function(x,y){
m.x <- length(x)
m.y <- length(y)
abs.tstat <- abs(t.test(x, y, mu=0, var.equal=TRUE)$statistic)
# Student's t-distribution based p-value
t.test(x, y, mu=0, var.equal=TRUE)$p.value
xy<-c(x,y) # combines two samples
B <- 1000
tstats.permutation <- replicate(B, expr = {
  xy.b <- sample(xy, (m.x + m.y), replace=FALSE)
  x.b <- xy.b[1:m.x]
  y.b <- xy.b[(m.x + 1):(m.x + m.y)]
  abs(t.test(x.b, y.b, mu=0, var.equal=TRUE)$statistic)
	})
# Permutation p-value calculation
p.value <- mean(tstats.permutation >= abs.tstat)
return(p.value)
}

# iv)
# The permutation-based two-sample t-test that does not assume equal variances
ttest4 <- function(x,y){
m.x <- length(x)
m.y <- length(y)
abs.tstat <- abs(t.test(x, y, mu=0, var.equal=FALSE)$statistic)
# Student's t-distribution based p-value
t.test(x, y, mu=0, var.equal=FALSE)$p.value
xy<-c(x,y) # combines two samples
B <- 1000
tstats.permutation <- replicate(B, expr = {
  xy.b <- sample(xy, (m.x + m.y), replace=FALSE)
  x.b <- xy.b[1:m.x]
  y.b <- xy.b[(m.x + 1):(m.x + m.y)]
  abs(t.test(x.b, y.b, mu=0, var.equal=FALSE)$statistic)
	})
# Permutation p-value calculation
p.value <- mean(tstats.permutation >= abs.tstat)
return(p.value)
}

# 1.a.i)
B <- 10000
alpha <- 0.05
pvec <- double(B)
for(i in 1:B){
	# Random samples from t(6) with mx = 7
	x <- rt(7, df = 6)
	# Random samples from t(7) with my = 8
	y <- rt(8, df = 7)
	pvec[i] <- ttest1(x,y)
}
alpha.sim <- mean(pvec <= alpha)
alpha.sim
# [1] 0.0495

# 1.a.ii)
B <- 10000
alpha <- 0.05
pvec <- double(B)
for(i in 1:B){
	# Random samples from t(6) with mx = 7
	x <- rt(7, df = 6)
	# Random samples from t(7) with my = 8
	y <- rt(8, df = 7)
	pvec[i] <- ttest2(x,y)
}
alpha.sim <- mean(pvec <= alpha)
alpha.sim
# [1] 0.0429

# 1.a.iii)
B <- 10000
alpha <- 0.05
pvec <- double(B)
for(i in 1:B){
	# Random samples from t(6) with mx = 7
	x <- rt(7, df = 6)
	# Random samples from t(7) with my = 8
	y <- rt(8, df = 7)
	pvec[i] <- ttest3(x,y)
}
alpha.sim <- mean(pvec <= alpha)
alpha.sim
# [1] 0.0471

# 1.a.iv)
B <- 10000
alpha <- 0.05
pvec <- double(B)
for(i in 1:B){
	# Random samples from t(6) with mx = 7
	x <- rt(7, df = 6)
	# Random samples from t(7) with my = 8
	y <- rt(8, df = 7)
	pvec[i] <- ttest4(x,y)
}
alpha.sim <- mean(pvec <= alpha)
alpha.sim
# [1] 0.0517

# 1.b.i)
B <- 10000
alpha <- 0.05
pvec <- double(B)
for(i in 1:B){
	# Random samples from t(4) with mx = 7
	x <- rt(7, df = 4)
	# Random samples from t(20) with my = 8
	y <- rt(8, df = 20)
	pvec[i] <- ttest1(x,y)
}
alpha.sim <- mean(pvec <= alpha)
alpha.sim
# [1] 0.049

# 1.b.ii)
B <- 10000
alpha <- 0.05
pvec <- double(B)
for(i in 1:B){
	# Random samples from t(4) with mx = 7
	x <- rt(7, df = 4)
	# Random samples from t(20) with my = 8
	y <- rt(8, df = 20)
	pvec[i] <- ttest2(x,y)
}
alpha.sim <- mean(pvec <= alpha)
alpha.sim
# [1] 0.0453

# NEED TO RUN THIS
# 1.b.iii)
B <- 10000
alpha <- 0.05
pvec <- double(B)
ptm <- proc.time()
for(i in 1:B){
	# Random samples from t(4) with mx = 7
	x <- rt(7, df = 4)
	# Random samples from t(20) with my = 8
	y <- rt(8, df = 20)
	pvec[i] <- ttest3(x,y)
}
alpha.sim <- mean(pvec <= alpha)
alpha.sim
proc.time() - ptm
# [1] 0.0541
#   user  system elapsed 
# 2926.87    1.11 3006.53 

# NEED TO RUN THIS
# 1.b.iv)
B <- 10000
alpha <- 0.05
pvec <- double(B)
ptm <- proc.time()
for(i in 1:B){
	# Random samples from t(4) with mx = 7
	x <- rt(7, df = 4)
	# Random samples from t(20) with my = 8
	y <- rt(8, df = 20)
	pvec[i] <- ttest4(x,y)
}
alpha.sim <- mean(pvec <= alpha)
alpha.sim
proc.time() - ptm
# [1] 0.0515
#   user  system elapsed 
# 3008.52    1.92 3175.06 


# 1.c.i)
B <- 10000
alpha <- 0.05
pvec <- double(B)
for(i in 1:B){
	# Random samples from laplace(0.5) with mx = 7
	x <- rexp(7,rate=0.5) - rexp(7,rate=0.5)
	# Random samples from laplace(2) with my = 8
	y <- rexp(8,rate=2) - rexp(8,rate=2)
	pvec[i] <- ttest1(x,y)
}
alpha.sim <- mean(pvec <= alpha)
alpha.sim
# [1] 0.0731
# Laplace(0.5) CDF looks much different than Laplace(2) so the tests are not very robust


# 1.c.ii)
B <- 10000
alpha <- 0.05
pvec <- double(B)
for(i in 1:B){
	# Random samples from laplace(0.5) with mx = 7
	x <- rexp(7,rate=0.5) - rexp(7,rate=0.5)
	# Random samples from laplace(2) with my = 8
	y <- rexp(8,rate=2) - rexp(8,rate=2)
	pvec[i] <- ttest2(x,y)
}
alpha.sim <- mean(pvec <= alpha)
alpha.sim
# [1] 0.04

# NEED TO RUN THIS
# 1.c.iii)
B <- 10000
alpha <- 0.05
pvec <- double(B)
ptm <- proc.time()
for(i in 1:B){
	# Random samples from laplace(0.5) with mx = 7
	x <- rexp(7,rate=0.5) - rexp(7,rate=0.5)
	# Random samples from laplace(2) with my = 8
	y <- rexp(8,rate=2) - rexp(8,rate=2)
	pvec[i] <- ttest3(x,y)
}
alpha.sim <- mean(pvec <= alpha)
alpha.sim
proc.time() - ptm
# [1] 0.0843
#   user  system elapsed 
# 2921.51    1.00 3005.91 

# NEED TO RUN THIS
# 1.c.iv)
B <- 10000
alpha <- 0.05
pvec <- double(B)
ptm <- proc.time()
for(i in 1:B){
	# Random samples from laplace(0.5) with mx = 7
	x <- rexp(7,rate=0.5) - rexp(7,rate=0.5)
	# Random samples from laplace(2) with my = 8
	y <- rexp(8,rate=2) - rexp(8,rate=2)
	pvec[i] <- ttest4(x,y)
}
alpha.sim <- mean(pvec <= alpha)
alpha.sim
proc.time() - ptm
# [1] 0.0712
#    user  system elapsed 
# 2728.11    0.95 2787.37 

# 1.d.i)
B <- 10000
alpha <- 0.05
pvec <- double(B)
for(i in 1:B){
	# Random samples from t(6) with mx = 7
	x <- rt(7, df = 6)
	# Random samples from laplace(0.5) with my = 8
	y <- rexp(8,rate=0.5) - rexp(8,rate=0.5)
	pvec[i] <- ttest1(x,y)
}
alpha.sim <- mean(pvec <= alpha)
alpha.sim
# [1] 0.0425

# 1.d.ii)
B <- 10000
alpha <- 0.05
pvec <- double(B)
for(i in 1:B){
	# Random samples from t(6) with mx = 7
	x <- rt(7, df = 6)
	# Random samples from laplace(0.5) with my = 8
	y <- rexp(8,rate=0.5) - rexp(8,rate=0.5)
	pvec[i] <- ttest2(x,y)
}
alpha.sim <- mean(pvec <= alpha)
alpha.sim
# [1] 0.0427

# NEED TO RUN THIS
# 1.d.iii)
B <- 10000
alpha <- 0.05
pvec <- double(B)
ptm <- proc.time()
for(i in 1:B){
	# Random samples from t(6) with mx = 7
	x <- rt(7, df = 6)
	# Random samples from laplace(0.5) with my = 8
	y <- rexp(8,rate=0.5) - rexp(8,rate=0.5)
	pvec[i] <- ttest3(x,y)
}
alpha.sim <- mean(pvec <= alpha)
alpha.sim
proc.time() - ptm
# [1] 0.047
#   user  system elapsed 
# 2538.99    0.48 2554.54 

# NEED TO RUN THIS
# 1.d.iv)
B <- 10000
alpha <- 0.05
pvec <- double(B)
ptm <- proc.time()
for(i in 1:B){
	# Random samples from t(6) with mx = 7
	x <- rt(7, df = 6)
	# Random samples from laplace(0.5) with my = 8
	y <- rexp(8,rate=0.5) - rexp(8,rate=0.5)
	pvec[i] <- ttest4(x,y)
}
alpha.sim <- mean(pvec <= alpha)
alpha.sim
proc.time() - ptm
# [1] 0.0584
#    user  system elapsed 
# 2552.35    0.38 2564.37

# Problem 2)
brcadata <- read.table("brcadata2.txt", header = TRUE, sep = "")
brcadatamat <- as.matrix(brcadata)
# ttest1 was most robust
B <- 3170
alpha <- 0.05
pvec <- double(B)
for(i in 1:B){
	brcadata17 <- brcadatamat[i,1:7]
	brcadata815 <- brcadatamat[i,8:15]
	pvec[i] <- ttest1(brcadata17, brcadata815)
}
#pvec

#b)
hist(pvec, freq = FALSE, breaks = seq(0,1,1/14))

#c)
# The minimum value of the x-axis for the which the density seems flat is 0.3.

#d)
lambda <- 0.3
m <- 3170
p.null <- sort(pvec)[c(round(m*lambda):m)]
ks.test(p.null, "punif", lambda, 1)
#hist(p.null, freq = FALSE)

#e)
p0 <- double(m)
for(i in 1:m){
	p0[i] <- as.numeric(pvec[i] > lambda)
}
sum(p0)/(m*(1-lambda))

# Problem 3)
#b)
qvec <- p.adjust(pvec, method = "fdr")

#c)
# NEED TO DO THIS FROM 2.e)
lambda2 <- 0.1
q0 <- double(m)
for(i in 1:m){
	q0[i] <- as.numeric(qvec[i] < lambda2)
}
sum(q0)
