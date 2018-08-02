library(pracma)
library(dbsml)

score <- matrix(0, 4, ncol(Y))
colnames(score) <- names(Y)
rownames(score) <- c("precision", "recall", "F1 Score", "F1 Score (All-True)")

for (i in 1:ncol(Y)) {
  score["precision", i] <- precision(prediction_round[, i], Y[, i])
  score["recall", i] <- recall(prediction_round[, i], Y[, i])
  score["F1 Score", i] <- F1Score(prediction_round[, i], Y[, i])
  score["F1 Score (All-True)", i] <- F1Score(matrix(1, nrow(Y), 1), Y[, i])
}
