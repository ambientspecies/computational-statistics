# Problem 1.
sampleMean<-function(n){
x <- sample(1:100, size=n, replace=TRUE)
return(mean(x))
}

outcome<-double(100)
for(i in 1:100){
outcome[i] <- sampleMean(100)
}

hist(outcome)
mean(outcome)
var(outcome)
sd(outcome)
median(outcome)
summary(outcome)

# Problem 2.

row1 <- c(6.5,-0.4,1.1,-0.5)
row2 <- c(3.4,-4.5,1.6,1.1)
row3 <- c(2.3,0.5,2.7,-2.1)
matrix.a <- rbind(row1, row2, row3)
matrix.a

#a)
atrans <- t(matrix.a)
atrans

#b)
matrix.b <- matrix.a%*%atrans
matrix.b

matrix.c <- atrans%*%matrix.a
matrix.c

#c)
library(MASS)
solve(matrix.b)
solve(matrix.c)
ginv(matrix.b)
ginv(matrix.c)

#e)
apply(matrix.a,1,median)
apply(matrix.a,2,sd)

# Problem 3.
data<-read.table("sleep.txt",header=TRUE)
y<-data[,3]
y
length(y)
sum(!is.na(y))
# Sum(!is.na(y)) is not counting the N/As in the vector. There are 14 NAs, so 48+14=62.

# b)
w<-y[!is.na(y)]
w

# c)
sleep17<-data[,1:7]
sleep17mat<-as.matrix(sleep17)
colMeans(sleep17mat,na.rm=TRUE)

# d)
sleep35<-data[,c(3,4,5)]
boxplot(sleep35, main="Box Plot of the Sleep Data")

# e)
data
tapply(data$Sleep,data$Danger,mean,na.rm=TRUE)

# Problem 4.
v<-sample(1:6,500000,replace=TRUE)
v

# b)
vmat <- matrix(v, nrow=10000, ncol=50)
vmat

# c)
row<-rowSums(vmat)

# d)
mean(row)
var(row)

# e)
hist(row,freq=FALSE,breaks="Scott",xlab="sum",main="Histogram of X")

# f)
dnorm(
