# Problem 1.

# d
n <- 1000
u <- runif(n)
x <- sqrt(u+1)-1
hist(x, prob=TRUE)

# e
mean(x)
var(x)

# Problem 2.

# b
n <- 1000
u <- runif(n)
x <- (-1/5)*log(u)

# c
hist(x, prob=TRUE)
y <- seq(0, 1, 0.1)
# density curve f_X(x)
lines(y, 5*exp(-5*y))

# e
m <- 1000
v <- double(m)
for(i in 1:m){
	count <- 0
	total <- 0
	while(total <= 1){
		t <- rexp(1,rate=5)
		total <- total + t
		count <- count + 1
	}
v[i] <- count
}
hist(v,prob=TRUE)

# f
par(mfrow=c(1,2))
hist(v,prob=TRUE)
hist(rpois(1000,5),prob=TRUE)

# Problem 3.

# b
n <- 1000
v <- double(n)
for(i in 1:n){
	u <- runif(1)
	if(u > 0.5){
	v[i] <- (-1/2)*log(2*(1-u))
	}else{
	v[i] <- (1/2)*log(2*u)
	}
}
hist(v, prob=TRUE)

# c
par(mfrow=c(1,2))
hist(v,prob=TRUE)
hist(rexp(1000,rate=2)-rexp(1000,rate=2),prob=TRUE)

# Problem 4.

# c
n <- 1000
# check the number of accepted observations by using
# the counter variable called numaccept.
numaccept <- 0
c <- sqrt(2/pi)*exp(1/2)
# check the number of iterations by using
# the counter variable called numiteration.
numiteration <- 0
# double(n) provides a vector of n zeros.
x <- double(n)
while (numaccept < n){ 
	# we do this one by one.
	#u <- rexp(n, rate=1)-rexp(n, rate=1) 
	u <- runif(1)
	# generate a random observation from g.
	y <- rexp(n, rate=1)-rexp(n, rate=1) 
	# calculate f(y)/[cg(y)] as in Step 4.
	fy <- (exp(-y^2/2)/sqrt(2*pi))
	gy <- (1/2)*exp(-y)
	fcg <- fy/(c*gy)
	if(u < fcg){ 
	# increment the number of accepted observations by 1.
	numaccept <- numaccept + 1
	# we accept y and store in x.
	x[numaccept] <- y
	}
	# increment the number of iterations by 1.
	numiteration <- numiteration + 1
	}
# visual confirmation using histogram
hist(x, prob = TRUE)

# d
# efficiency (probability of acceptance)
efficiency <- numaccept/numiteration
efficiency