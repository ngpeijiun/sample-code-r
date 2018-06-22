source("util.R")

z <- 1:1500
dim(z) <- c(3, 5, 100)
# z[2, , ]
# z[, 2, ]
# z[, 2, 2]
# z[,,]
# z[]
#
# dim(z)
#
# z[2]
# z[1:10]
# z[1:length(z)]

# Z <- array(h, dim = c(3,4,2))
# is same as
# Z <- h; dim(Z) <- c(3,4,2)

x <- array(1:20, dim = c(4, 5))
i <- array(c(1:3,3:1), dim = c(3,2))
# x[i]
x[i] <- 0

# ???
# N <- table(blocks, varieties)

a <- 1:5
b <- 2:6

op <- a %o% b
# outer(a, b, "*")

f <- function(x, y) cos(y) / (1 + x ^ 2)
of <- outer(a, b, f)

d <- outer(0:9, 0:9)
fr <- table(outer(d, d, "-"))
plot(fr, xlab = "Determinant", ylab = "Frequency")

# Transpose
dt <- aperm(d, c(2, 1))
# dt <- t(d)

# Matrix Operations
# t(m) nrow(m) ncol(m)

A <- magic(3)
B <- magic(3)

M1 <- A * B
M2 <- A %*% B

# Vector Product
# v %*% v ## produce smaller matrix
# crossprod(v) ## v^Tv
# a %o% a ## vv^T
# diag(5) diag(v) diag(m)

# Linear Equation
# solve(A, y)
# Inverse, i.e. A^-1
# solve(A)
# Formally solve(A) %*% b

# Eigen
# eigen(m) eigen(m)$values eigen(m, only.values = TRUE)$values

# SVD
# absdet <- function(M) prod(svd(M)$d)

# Concatenate
# cbind(m1, m2) rbind(m1, m2)
