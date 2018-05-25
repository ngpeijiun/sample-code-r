x <- c(1, 2, 3, 4)
y <- c(x, 0, x)
v <- 2*x + y + 1

# Arithmetic Operators
# + - * / ^

# Arithmetic Functions
# log(x) exp(x) sin(x) cos(x) tan(x) sqrt(x)

# Statistical Functions
# min(v) max(v) length(v) sum(v) prod(v) mean(v) median(v)
# var(v) cov(m) cor(m)
# which.min(v) which.max(v) pmin(v1, v2) pmax(v1, v2)

# Vector Manipulation Functions
# match(v, lookup) tabulate(v)

c <- sqrt(-17+0i)

s1 <- seq(-5, 5, by=.2)
s2 <- seq(from=-5, length=51, by=.2)

s3 <- rep(x, times=5)
s4 <- rep(x, each=5)

l1 <- x > 3

# Comparison Operators
# < <= > >= == !=

# Logical Operators
# & | !

# Missing Values
# NA NaN Inf

l2 <- is.na(x)
l3 <- is.nan(x)

labs <- paste(c("X", "Y"), 1:10, sep="")
