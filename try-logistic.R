library(pracma)
library(gradDescent)
library(dbsml)

data(BreastCancer, package = "mlbench")

y <- ifelse(BreastCancer[, "Class"] == "malignant", 1, 0)

#feature <- BreastCancer[, c("Cl.thickness", "Cell.size", "Cell.shape")]
feature <- BreastCancer[, c("Cl.thickness", "Cell.size")]
feature <- cbind(feature, discreteTable(BreastCancer, "Cell.shape"))

feature[] <- lapply(feature[], function(x) as.numeric(as.character(x)))
featureNorm <- featureNormalize(feature)

# Logistic Regression Using Generalized Linear Model (R)

X <- featureNorm$X
model_GLM_R <- glm(y ~ ., data = X, family = "binomial")
X_new <- featureScaling(feature, featureNorm$conf)
predict_GLM_R <- predict(model_GLM_R, X_new, type = "response")
predict_GLM_R <- as.numeric(round(predict_GLM_R))
mean_GLM_R <- mean(y == predict_GLM_R)

# Logistic Regression Using Gradient Descent (GD) (self-made)

X <- data.matrix(cbind(1, featureNorm$X))
model_GD <- gradientDescent(X, y, alpha = 0.3, numIter = 400, numCost = 40, model = "logistic")
X_new <- data.matrix(cbind(1, featureScaling(feature, featureNorm$conf)))
predict_GD <- as.vector(sigmoid(X %*% model_GD$theta))
predict_GD <- as.numeric(round(predict_GD))
mean_GD <- mean(y == predict_GD)

plot(model_GD$costHistory, type = "o", main = "Gradient Descent", xlab = "Iterations", ylab = "Costs", xaxt = "n")
axis(side = 1, at = c(seq(0, 40, by = 5)))

# Logistic Regression Using Momentum Gradient Descent (MGD) (self-made)

X <- data.matrix(cbind(1, featureNorm$X))
model_MGD <- gradientDescent(X, y, alpha = 0.3 * 0.4,
                             momentum = list(value = 0.5),
                             numIter = 400, numCost = 40, model = "logistic")
X_new <- data.matrix(cbind(1, featureScaling(feature, featureNorm$conf)))
predict_MGD <- as.vector(sigmoid(X %*% model_MGD$theta))
predict_MGD <- as.numeric(round(predict_MGD))
mean_MGD <- mean(y == predict_MGD)

plot(model_MGD$costHistory, type = "o", main = "Momentum Gradient Descent", xlab = "Iterations", ylab = "Costs", xaxt = "n")
axis(side = 1, at = c(seq(0, 40, by = 5)))

# Logistic Regression Using Accelerated Gradient Descent (AGD) (self-made)

X <- data.matrix(cbind(1, featureNorm$X))
model_AGD <- gradientDescent(X, y, alpha = 0.3 * 0.4 * 0.6,
                             momentum = list(value = 0.5, accelerated = TRUE),
                             numIter = 400, numCost = 40, model = "logistic")
X_new <- data.matrix(cbind(1, featureScaling(feature, featureNorm$conf)))
predict_AGD <- as.vector(sigmoid(X %*% model_AGD$theta))
predict_AGD <- as.numeric(round(predict_AGD))
mean_AGD <- mean(y == predict_AGD)

plot(model_AGD$costHistory, type = "o", main = "Accelerated Gradient Descent", xlab = "Iterations", ylab = "Costs", xaxt = "n")
axis(side = 1, at = c(seq(0, 40, by = 5)))

# Logistic Regression Using Auto-Momentum Gradient Descent (AMGD) (self-made)

X <- data.matrix(cbind(1, featureNorm$X))
model_SD <- gradientDescent(X, y, alpha = 1,
                            momentum = list(auto = TRUE, accelerated = TRUE),
                            tolerance = 1e-3,
                            numIter = 200, model = "logistic")
X_new <- data.matrix(cbind(1, featureScaling(feature, featureNorm$conf)))
predict_SD <- as.vector(sigmoid(X %*% model_SD$theta))
predict_SD <- as.numeric(round(predict_SD))
mean_SD <- mean(y == predict_SD)

plot(model_SD$gradHistory, type = "o", main = "AMGD", xlab = "Iterations", ylab = "Gradients")
