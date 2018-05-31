# Mode
# logical, numeric, complex, character, raw
# list
#
# mode(v) length(v)

z <- 0:9
digits <- as.character(z)
d <- as.integer(digits)

e <- numeric()
e[3] <- 17

alpha <- 1:10
alpha <- alpha[2 * 1:5]
length(alpha) <- 3

# attributes(v)
# attr(m, "dim") <- c(10, 10)

# Class
# class(v) unclass(v)
