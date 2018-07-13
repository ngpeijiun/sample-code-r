library(pracma)
library(dbsml)

dataset <- read.table("data", header = TRUE, sep = ",")
dataset[] <- lapply(dataset, factor)

Y <- discreteTable(dataset, "Output")

feature <- discreteTable(dataset, "Feature.1")
feature <- discreteMerge(feature, discreteTable(dataset, "Feature.2.1", asName = "Feature.2"))
feature <- discreteMerge(feature, discreteTable(dataset, "Feature.2.2", asName = "Feature.2"))
feature <- discreteMerge(feature, discreteTable(dataset, "Feature.2.3", asName = "Feature.2"))
feature <- cbind(feature, discreteTable(dataset, "Feature.3"))
feature <- cbind(feature, discreteTable(dataset, "Feature.4"))
feature <- cbind(feature, discreteTable(dataset, "Feature.5"))

feature_norm <- featureNormalize(feature)
