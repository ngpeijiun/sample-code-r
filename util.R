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

concatHeader <- function(dataset, header, asName = NA, withBlank = FALSE) {
  if (is.data.frame(header)) {
    header = levels(header[, 1])
  }
  if (withBlank) {
    numCol <- length(header) + 1
    uniques <- sort(c(header, ""))
  } else {
    numCol = length(header)
    uniques <- sort(header)
  }
  a <- data.frame(matrix(0, nrow = nrow(dataset), ncol = numCol))
  if (is.na(asName)) {
    names(a) <- uniques
  } else {
    names(a) <- paste(asName, uniques, sep = ".")
  }
  b = cbind(dataset, a)
}

discreteTable <- function(dataSet, key, asName = key) {
  t <- table(as.numeric(rownames(dataSet)), dataSet[[key]])
  df <- as.data.frame.matrix(t)
  if (ncol(df) > 0) {
    names(df) <- paste(asName, levels(dataSet[[key]]), sep = ".")
  }
  df
}

discreteMerge <- function(d1, d2) {
  for (name in names(d2)) {
    if (name %in% names(d1)) {
      d1[name] = as.numeric(d1[name] | d2[name])
    } else {
      d1 <- cbind(d1, d2[name])
    }
  }
  d1
}

discreteRound <- function(prediction) {
  index = apply(prediction, 1, which.max)
  prediction[col(prediction) == index] = 1
  prediction[col(prediction) != index] = 0
  prediction
}

featureNormalize <- function(X) {
  if (is.vector(X)) {
    mu <- mean(X)
    sigma <- sd(X)
    norm <- (X - mu) / ifelse(sigma > 0, sigma, 1)
  } else {
    mu <- apply(X, 2, mean)
    sigma <- apply(X, 2, sd)
    norm <- sweep(sweep(X, 2, mu, "-"), 2, ifelse(sigma > 0, sigma, 1), "/")
  }

  list(X = norm, conf = list(mu = mu, sigma = sigma))
}

featureScaling <- function(X, conf) {
  if (is.vector(X)) {
    (X - conf$mu) / ifelse(conf$sigma > 0, conf$sigma, 1)
  } else {
    sweep(sweep(X, 2, conf$mu, "-"), 2, ifelse(conf$sigma > 0, conf$sigma, 1), "/")
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

init.momentum <- function(momentum) {
  if (missing(momentum) || !is.list(momentum)) {
    momentum <- list(value = 0, auto = FALSE, accelerated = FALSE)
  }
  if (is.null(momentum$value) || !is.numeric(momentum$value)) {
    momentum$value <- 0
  }
  if (is.null(momentum$auto) || !is.logical(momentum$auto)) {
    momentum$auto <- FALSE
  }
  if (is.null(momentum$accelerated) || !is.logical(momentum$accelerated)) {
    momentum$accelerated <- FALSE
  }

  momentum
}

gradientDescent <- function(X, y,
                            alpha = 0.1,
                            momentum = init.momentum(),
                            tolerance = NA,
                            numIter = 10, numCost = 10,
                            model = "linear") {

  n <- ncol(X)
  theta <- numeric(n)
  velocity <- numeric(n)

  momentum <- init.momentum(momentum)
  last <- 0

  gradHistory <- numeric(0)
  costHistory <- numeric(0)
  j <- 0

  for (i in 1:numIter) {
    if (momentum$accelerated) {
      theta <- theta - momentum$value * velocity
    }

    delta <- computeGrad(theta, X, y, model)
    grad <- norm(matrix(delta))

    if (momentum$auto) {
      curv <- abs(grad - last)
      momentum$value <- max(0, 1 - curv / grad)
      last <- grad
    }

    velocity <- momentum$value * velocity + alpha * delta
    theta <- theta - velocity

    gradHistory[i] = grad

    if (floor(i * numCost / numIter) > j) {
      j <- floor(i * numCost / numIter)
      costHistory[j] <- computeCost(theta, X, y, model)
    }

    if (is.numeric(tolerance)) {
      if (is.na(grad)) {
        message("Warn: grad is NA")
        break
      }
      if (grad < tolerance) {
        break
      }
    }
  }

  list(theta = theta, costHistory = costHistory, gradHistory = gradHistory, grad = grad)
}
