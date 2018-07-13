library(pracma)
library(dbsml)

now <- Sys.time()

# Logistic Regression Testing

if (!exists("prediction")) {
  prediction = matrix(0, nrow(Y), ncol(Y))
}

X_new <- data.matrix(cbind(1, featureScaling(feature, norm)))

for (i in 1:ncol(Y)) {
  prediction[, i] <- as.vector(sigmoid(X_new %*% model_All[[i]]$theta))
}

prediction_round <- discreteRound(prediction)
ties = apply(Y == prediction_round, 1, all)

#head(which(!ties))
#dataset[123,]
#prediction_round[123,]
#prediction[123,]
#write.table(dataset[which(!ties),], file = "wrong_prediction.csv", row.names = FALSE, sep = ",")

then <- Sys.time()
print(then - now)
