f = 7:241
tags = double(241)
u = double(length(f))
x = double(length(f))
d = double(241 * 1000)
mat = matrix(d, nrow = 1000, ncol = 241)

for(j in 1:1000){
	for(i in 1:235){
		u[i] = runif(1,0,f[i])
		x[i] = ceiling(u[i])
		tags[ceiling(u[i])] = tags[ceiling(u[i])] + 1
	}
mat[j,] <- tags/1000
}

w <- colMeans(mat)
hist(w)$mids
hist(w)$counts
mids <- hist(w)$mids
counts <- hist(w)$counts

# Checking scale-free 2x
g = 14:482
tags2 = double(482)
u2 = double(length(g))
x2 = double(length(g))
d2 = double(482 * 1000)
mat2 = matrix(d2, nrow = 1000, ncol = 482)

for(j in 1:1000){
	for(i in 1:469){
		u2[i] = runif(1,0,g[i])
		x2[i] = ceiling(u2[i])
		tags2[ceiling(u2[i])] = tags2[ceiling(u2[i])] + 1
	}
mat2[j,] <- tags2/1000
}

w2 <- colMeans(mat2)
hist(w2)$mids
hist(w2)$counts

# Checking scale-free 4x
h = 28:964
tags3 = double(964)
u3 = double(length(h))
x3 = double(length(h))
d3 = double(964 * 1000)
mat3 = matrix(d3, nrow = 1000, ncol = 964)

for(j in 1:1000){
	for(i in 1:937){
		u3[i] = runif(1,0,h[i])
		x3[i] = ceiling(u3[i])
		tags3[ceiling(u3[i])] = tags3[ceiling(u3[i])] + 1
	}
mat3[j,] <- tags3/1000
}

w3 <- colMeans(mat3)
hist(w3)$mids
hist(w3)$counts