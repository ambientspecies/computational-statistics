fred <- read.table("crab.txt", header = TRUE)
attach(fred)
mylogistic <- glm(y~weight, family = binomial)
summary(mylogistic)
pi0 <- 0.642;
pihat <- exp(mylogistic$fit)/(1+exp(mylogistic$fit));
yhat <- rep(0, length(weight));
for(i in (1:length(weight)))
  {
    if(pihat[i]> pi0){yhat[i] <- 1;}
  }

cbind(pihat,yhat,weight);

#sensitivity = P(yhat = 1|y=1)
sens <- length(yhat[yhat == 1& y == 1])/length(yhat[y == 1]);
sensvec <- 

#spec <- P(yhat = 0|y = 0);
spec <- length(yhat[yhat == 0& y == 0])/length(yhat[y == 0]);

sens;
spec;

install.packages("Epi")
library(Epi)
ROC(form = y~weight, plot = "ROC")

weightsq <- weight*weight
biglogistic <- glm(y~weight + weightsq, family = binomial)
summary(biglogistic)
anova(mylogistic, biglogistic)
anova(biglogistic, mylogistic)


#############5.14
install.packages("bbmle")
library(bbmle)

#Grouped data
table <- matrix(c(4,4,4,1,2,4), nc = 2)
logistic2M0 <- glm(table ~ 1, family = binomial)
logistic2M1 <- glm(table ~ c(0,1,2), family = binomial)
logLik(logistic2M0)
logLik(logistic2M1)
summary(logistic2M0)
summary(logistic2M1)
anova(logistic2M0,logistic2M1)


#Ungrouped data
data <- c(rep(1, 4+4+4), rep(0, 1+2+4))
x <- c(rep(0, 4), rep(1, 4), rep(2, 4), rep(0, 1), rep(1, 2), rep(2, 4))
logistic1M0 <- glm(data~1, family = binomial)
logistic1M1 <- glm(data~x, family = binomial)
logLik(logistic1M0)
logLik(logistic1M1)
summary(logistic1M0)
summary(logistic1M1)
anova(logistic1M0,logistic1M1)

