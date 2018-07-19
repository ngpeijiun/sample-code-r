library(pracma)
library(dbsml)

dataset <- read.table("data", header = TRUE, sep = ",")
#dataset[] <- lapply(dataset, factor)

m <- nrow(dataset)

Y <- discreteTable(dataset, "Output")

feature <- data.frame(matrix(0, nrow = m, ncol = 0))

header <- read.table("data.header.feature.1.txt", header = TRUE, sep = ",")
feature <- concatHeader(feature, header = header, asName = "Feature.1")
header <- read.table("data.header.feature.2.txt", header = TRUE, sep = ",")
feature <- concatHeader(feature, header = header, asName = "Feature.2")
feature <- concatHeader(feature, header = "Feature.3")
header <- read.table("data.header.feature.4.txt", header = TRUE, sep = ",")
feature <- concatHeader(feature, header = header, asName = "Feature.A")
header <- read.table("data.header.feature.5.txt", header = TRUE, sep = ",")
feature <- concatHeader(feature, header = header, asName = "Feature.B")
header <- read.table("data.header.feature.6.txt", header = TRUE, sep = ",")
feature <- concatHeader(feature, header = header, asName = "Feature.C")

feature <- discreteMerge(feature, discreteTable(dataset, "Feature.1"))
feature <- discreteMerge(feature, discreteTable(dataset, "Feature.2.1", asName = "Feature.2"))
feature <- discreteMerge(feature, discreteTable(dataset, "Feature.2.2", asName = "Feature.2"))
feature <- discreteMerge(feature, discreteTable(dataset, "Feature.2.3", asName = "Feature.2"))
feature <- discreteMerge(feature, dataset[, c("Feature.3"), drop = FALSE])
feature <- discreteMerge(feature, discreteTable(dataset, "Feature.4", asName = "Feature.A"))
feature <- discreteMerge(feature, discreteTable(dataset, "Feature.5", asName = "Feature.B"))
feature <- discreteMerge(feature, discreteTable(dataset, "Feature.6", asName = "Feature.C"))
