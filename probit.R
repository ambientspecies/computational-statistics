x <- c(rep(1,30), rep(2,30), rep(3,30))
y <- c(rep(1,14), rep(2,10), rep(3,6), rep(1,10), rep(2,10), rep(3,10), rep(3,6), rep(2,10), rep(3,14))
r <- cov(x, y)
msq <- 89*r^2
msq

x <- c(rep(7.5,76), rep(20,108), rep(32.5,89))
y <- c(rep(1,9), rep(2,44), rep(3,13), rep(4,10), rep(1,11), rep(2,52), rep(3,23), rep(4,22), rep(1,9), rep(2,4), rep(3,12), rep(4,27))
r <- cov(x, y)
msq <- 272*r^2
msq

fred <- read.table("crab.txt", header = TRUE)
attach(fred)
lm(y~weight)
mylogit <- glm(y~weight, data = fred, family = "binomial")
myprobit <- glm(y~weight, family = binomial(link="probit"), data = fred)
mylog <- glm(y~weight, family = "poisson", data = fred)
summary(myprobit)

install.packages("DAAG")
library(DAAG)
data(anesthetic)
data(fred)
fredlm <- lm(nomove~conc, data = anesthetic)
fredlogistic <- glm(y~weight, family = binomial(link = "identity"))
fredlogistic

cat("pi(5.20)=",myprobit$coefficient[1],"+",myprobit$coefficient[2],"* (5.20) =",(myprobit$coefficient[1]+myprobit$coefficient[2]*5.20))

pnorm((myprobit$coefficient[1]+myprobit$coefficient[2]*2000))
pnorm((myprobit$coefficient[1]+myprobit$coefficient[2]*2850))
pnorm((myprobit$coefficient[1]+myprobit$coefficient[2]*5200))
