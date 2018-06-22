bslash <- function(X, y) {
	X <- qr(X)
	qr.coef(X, y)
}

# normalizedCoef <- bslash(X, y)

"%!%" <- function(X, y) {
	X <- qr(X)
	qr.coef(X, y)
}

# normalizedCoef <- X %!% y

fun1 <- function(data, data.frame, graph = TRUE, limit = if (graph) 20 else 10) {
	# here it goes
}

# Positional arguments
# ans <- fun1(d, df, TRUE, 20)

# Positional arguments follow by named arguments
# ans <- fun1(d, df, graph = TRUE, limit = 20)

# Named arguments can be in arbitrary position
# ans <- fun1(data = d, limit = 20, graph = TRUE, data.frame = df)

# Optional arguments which have default values can be omitted
# ans <- fun1(d, df, graph = FALSE)

fun2 <- function(a, b, ...) str(...)

# > fun2(1, 2, 3, 4)
# num [1:2] 4 5

fun3 <- function(a, b, ...) {
	str(..1)
	str(..2)
}

# > fun2(1, 2, 3, 4)
# num 4
# num 5

no.dim <- function(X) {
	temp <- X
	dimnames(temp) <- list(rep("", nrow(X)), rep("", ncol(X)))
	temp
}

# methods(class = "data.frame")
# methods(coef)

# getAnywhere("coef.aov")
# getS3method("coef", "aov")
