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

featureTable <- function(df, key) {
  t <- table(as.numeric(rownames(df)), BreastCancer[[key]])
  df <- as.data.frame.matrix(t)
  names(df) <- paste(key, levels(BreastCancer[[key]]), sep = ".")
  df
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

computeCostLinear <- function(theta, X, y) {
  m <- length(y)
  prediction <- as.vector(X %*% theta)
  cost <- (prediction - y) ^ 2
  J <- 1 / (2 * m) * sum(cost)
}

computeCostLogistic <- function(theta, X, y) {
  m <- length(y)
  prediction <- as.vector(sigmoid(X %*% theta))
  cost <- -1 * (y * log(prediction) + (1 - y) * log(1 - prediction))
  J <- 1 / m * sum(cost)
}

computeCost <- function(theta, X, y, model = "linear") {
  f <- switch(model,
              linear = computeCostLinear,
              logistic = computeCostLogistic)
  f(theta, X, y)
}

computeGradLinear <- function(theta, X, y) {
  m <- length(y)
  prediction <- as.vector(X %*% theta)
  grad <- as.vector(1 / m * t(X) %*% (prediction - y))
}

computeGradLogistic <- function(theta, X, y) {
  m <- length(y)
  prediction <- as.vector(sigmoid(X %*% theta))
  grad <- as.vector(1 / m * t(X) %*% (prediction - y))
}

computeGrad <- function(theta, X, y, model = "linear") {
  f <- switch(model,
              linear = computeGradLinear,
              logistic = computeGradLogistic)
  f(theta, X, y)
}

gradientDescent <- function(X, y,
                            alpha = 0.1, momentum = 0, accelerated = FALSE,
                            numIter = 10, numCost = 10,
                            model = "linear") {

  n <- ncol(X)
  theta <- numeric(n)
  velocity <- numeric(n)

  numCost <- min(numIter, numCost)
  costHistory <- numeric(numCost)
  j <- 0

  for (i in 1:numIter) {
    if (accelerated) {
      theta <- theta - momentum * velocity
    }

    velocity <- momentum * velocity + alpha * computeGrad(theta, X, y, model)
    theta <- theta - velocity

    if (floor(i * numCost / numIter) > j) {
      j <- floor(i * numCost / numIter)
      costHistory[j] <- computeCost(theta, X, y, model)
    }
  }

  list(theta = theta, costHistory = costHistory, velocity = norm(matrix(momentum * velocity)))
}

steepestDescent <- function(X, y,
                            tolerance = 1e-12,
                            maxIter = 10,
                            model = "linear") {

  n <- ncol(X)
  theta <- numeric(n)

  costHistory <- numeric(0)
  last <- 0

  for (i in 1:maxIter) {
    grad <- computeGrad(theta, X, y, model)
    theta <- theta - grad

    costHistory[i] <- computeCost(theta, X, y, model)

    error <- norm(matrix(grad))
    if (abs(error - last) < tolerance) {
      break
    }
    last <- error
  }

  list(theta = theta, costHistory = costHistory, error = abs(error - last))
}
