library(pracma)
library(dbsml)

options(digit = 30)

now <- Sys.time()

# Logistic Regression Using Gradient Descent

alpha = c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01)
tolerance = c(1e-1, 1e-1, 1e-1, 1e-1, 1e-1, 1e-1)

if (!exists("model_All")) {
  model_All = list()
}

if (!exists("prediction")) {
  prediction = matrix(0, nrow(Y), ncol(Y))
}

X <- data.matrix(cbind(1, feature_norm$X))

for (i in 1:ncol(Y)) {
  y <- Y[, i]
  
  model <- gradientDescent(X, y,
                           alpha = alpha[i],
                           momentum = list(auto = TRUE, accelerated = TRUE),
                           tolerance = tolerance[i],
                           lambda = list(auto = TRUE),
                           numIter = 2000,
                           model = "logistic")
  
  print(length(model$gradHistory))
  prediction[, i] <- as.vector(sigmoid(X %*% model$theta))
  
  plot(model$gradHistory, type = "o", main = names(Y)[i], xlab = "Iterations", ylab = "Gradients")
  
  model_All[[i]] <- model
}

prediction_round <- discreteRound(prediction)
ties = apply(Y == prediction_round, 1, all)

#head(which(!ties))
#dataset[123,]
#prediction_round[123,]
#prediction[123,]
#write.table(dataset[which(!ties),], file = "wrong_prediction.csv", row.names = FALSE, sep = ",")

# Actually no need to achieve very high precision, because it may not be able to generalize well to new examples
low_precision <- which(prediction[which(prediction_round == 1)] < 0.8)
#which(prediction_round == 1)[low_precision] %% m

then <- Sys.time()
print(then - now)
