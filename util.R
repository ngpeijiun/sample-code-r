randInt <- function() {
  sample(.Machine$integer.max, 1)
}

modes <- function(v) {
  u <- unique(v)
  u[which.max(tabulate(match(v, u)))]
}

normalEquation <- function(X, y) {
  as.vector(solve(t(X) %*% X) %*% t(X) %*% y)
}

featureNormalize <- function(X) {
  if (is.vector(X)) {
    mu <- mean(X)
    sigma <- sd(X)
    norm <- (X - mu) / sigma
  } else {
    mu <- apply(X, 2, mean)
    sigma <- apply(X, 2, sd)
    norm <- sweep(sweep(X, 2, mu, "-"), 2, sigma, "/")
  }

  list(X = norm, conf = list(mu = mu, sigma = sigma))
}

featureScaling <- function(X, conf) {
  if (is.vector(X)) {
    (X - conf$mu) / conf$sigma
  } else {
    sweep(sweep(X, 2, conf$mu, "-"), 2, conf$sigma, "/")
  }
}

featureDescaling <- function(X, conf) {
  if (is.vector(X)) {
    X * conf$sigma + conf$mu
  } else {
    sweep(sweep(X, 2, conf$sigma, "*"), 2, conf$mu, "+")
  }
}

computeCost <- function(X, y, theta) {
  m <- length(y)
  prediction <- as.vector(X %*% theta)
  1 / (2 * m) * sum((prediction - y) ^ 2)
}

computeDelta <- function(X, y, theta) {
  m <- length(y)
  prediction <- as.vector(X %*% theta)
  as.vector(1 / m * t(X) %*% (prediction - y))
}

gradientDescent <- function(X, y, alpha = 0.1, momentum = 0, accelerated = FALSE, numIter = 10, numCost = 10) {
  n <- ncol(X)
  theta <- numeric(n)
  velocity <- numeric(n)

  costHistory <- numeric(numCost)
  j <- 0

  for (i in 1:numIter) {
    if (accelerated) {
      theta <- theta - momentum * velocity
    }

    velocity <- momentum * velocity + alpha * computeDelta(X, y, theta)
    theta <- theta - velocity

    if (floor(i * numCost / numIter) > j) {
      j <- floor(i * numCost / numIter)
      costHistory[j] = computeCost(X, y, theta)
    }
  }

  list(theta = theta, costHistory = costHistory, velocity = norm(matrix(momentum * velocity)))
}

steepestDescent <- function(X, y, maxIter = 10, tolerance = 1e-12) {
  n <- ncol(X)
  theta <- numeric(n)

  ySym <- t(X) %*% y
  XSym <- t(X) %*% X
  costHistory <- numeric(0)

  for (i in 1:maxIter) {
    residual <- as.vector(ySym - XSym %*% theta)
    alpha <- as.numeric(t(residual) %*% residual) / as.numeric(t(residual) %*% XSym %*% residual)
    theta <- theta + alpha * residual

    costHistory[i] <- computeCost(X, y, theta)

    if (norm(matrix(residual)) < tolerance) {
      break
    }
  }

  list(theta = theta, costHistory = costHistory, error = norm(matrix(residual)))
}
