library(pracma)
library(gradDescent)
library(dbsml)

data(airquality)

# To fill NA data with the mean of monthly air quality

for (i in 1:nrow(airquality)) {
	if (is.na(airquality[i, "Ozone"])) {
		airquality[i, "Ozone"] <- mean(airquality[which(airquality[, "Month"] == airquality[i, "Month"]), "Ozone"], na.rm = TRUE)
	}

	if (is.na(airquality[i, "Solar.R"])) {
		airquality[i, "Solar.R"] <- mean(airquality[which(airquality[, "Month"] == airquality[i, "Month"]), "Ozone"], na.rm = TRUE)
	}
}

y <- airquality[, "Ozone"]

feature <- airquality[, c("Solar.R", "Wind")]
featureNorm <- featureNormalize(feature)

# Linear Regression Using Linear Model (R)

X <- featureNorm$X
model_LM_R <- lm(y ~ ., data = X)
X_new <- featureScaling(data.frame(Solar.R = 10, Wind = 5), featureNorm$conf)
predict_LM_R <- predict(model_LM_R, X_new)
predict_LM_R <- as.numeric(predict_LM_R)

# Linear Regression Using Normal Equation (self-made)

X <- data.matrix(cbind(1, feature))
theta <- normalEquation(X, y)
X_new <- c(1, 10, 5)
predict_normal <- X_new %*% theta
predict_normal <- as.numeric(predict_normal)

# Linear Regression Using Gradient Descent (GD) (R)

X <- cbind(feature, y)
model_GD_R <- gradDescentR.learn(X, control = list(alpha = 0.3, maxIter = 100), seed = randInt())
X_new <- data.frame(Solar.R = 10, Wind = 5, V1 = NA)
predict_GD_R <- predict(model_GD_R, X_new)
predict_GD_R <- as.numeric(predict_GD_R$V1)

# Linear Regression Using Gradient Descent (GD) (self-made)

X <- data.matrix(cbind(1, featureNorm$X))
model_GD <- gradientDescent(X, y, alpha = 0.3, numIter = 100, numCost = 40)
X_new <- c(1, featureScaling(c(10, 5), featureNorm$conf))
predict_GD <- X_new %*% model_GD$theta
predict_GD <- as.numeric(predict_GD)

plot(model_GD$costHistory, type = "o", main = "Gradient Descent", xlab = "Iterations", ylab = "Costs", xaxt = "n")
axis(side = 1, at = c(seq(0, 40, by = 5)))

# Linear Regression Using Momentum Gradient Descent (MGD) (self-made)

X <- data.matrix(cbind(1, featureNorm$X))
model_MGD <- gradientDescent(X, y, alpha = 0.3 * 0.4,
                             momentum = list(value = 0.5),
                             numIter = 100, numCost = 40)
X_new <- c(1, featureScaling(c(10, 5), featureNorm$conf))
predict_MGD <- X_new %*% model_MGD$theta
predict_MGD <- as.numeric(predict_MGD)

plot(model_MGD$costHistory, type = "o", main = "Momentum Gradient Descent", xlab = "Iterations", ylab = "Costs", xaxt = "n")
axis(side = 1, at = c(seq(0, 40, by = 5)))

# Linear Regression Using Accelerated Gradient Descent (AGD) (self-made)

X <- data.matrix(cbind(1, featureNorm$X))
model_AGD <- gradientDescent(X, y, alpha = 0.3 * 0.4 * 0.6,
                             momentum = list(value = 0.5, accelerated = TRUE),
                             numIter = 100, numCost = 40)
X_new <- c(1, featureScaling(c(10, 5), featureNorm$conf))
predict_AGD <- X_new %*% model_AGD$theta
predict_AGD <- as.numeric(predict_AGD)

plot(model_AGD$costHistory, type = "o", main = "Accelerated Gradient Descent", xlab = "Iterations", ylab = "Costs", xaxt = "n")
axis(side = 1, at = c(seq(0, 40, by = 5)))

# Linear Regression Using Auto-Momentum Gradient Descent (AMGD) (self-made)

X <- data.matrix(cbind(1, featureNorm$X))
model_SD <- gradientDescent(X, y, alpha = 1,
                            momentum = list(auto = TRUE, accelerated = TRUE),
                            tolerance = 1e-12,
                            numIter = 100, numCost = 40)
X_new <- c(1, featureScaling(c(10, 5), featureNorm$conf))
predict_SD <- X_new %*% model_SD$theta
predict_SD <- as.numeric(predict_SD)

plot(model_SD$gradHistory, type = "o", main = "AMGD", xlab = "Iterations", ylab = "Gradients")
