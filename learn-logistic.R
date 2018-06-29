library(pracma)
library(dbsml)

# Linear Regression Using Steepest Descent (SD) (self-made)

prediction = matrix(0, nrow(Y), ncol(Y))

for (i in 1:ncol(Y)) {
  y <- Y[, i]
  
  X <- data.matrix(cbind(1, featureNorm$X))
  model_SD <- steepestDescent(X, y, tolerance = 1e-2, maxIter = 200, model = "logistic")
  X_new <- data.matrix(cbind(1, featureScaling(feature, featureNorm$conf)))
  prediction[, i] <- as.vector(sigmoid(X %*% model_SD$theta))
  
  plot(model_SD$costHistory, type = "o", main = names(Y)[i], xlab = "Iterations", ylab = "Costs")
}

prediction <- discreteRound(prediction)
ties = apply(Y == prediction, 1, all)

#mlc[which(!ties),]
