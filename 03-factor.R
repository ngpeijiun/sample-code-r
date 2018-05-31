state <- c("tas", "sa", "qld", "nsw", "nsw", "nt", "wa", "wa", "qld", "vic", "nsw", "vic", "qld", "qld", "sa", "tas", "sa", "nt", "wa", "vic", "qld", "nsw", "nsw", "wa", "sa", "act", "nsw", "vic", "vic", "act")
statef <- factor(state)
statel <- levels(statef)

incomes <- c(60, 49, 40, 61, 64, 60, 59, 54, 62, 69, 70, 42, 56, 61, 61, 61, 58, 51, 48, 65, 49, 49, 41, 48, 52, 46, 59, 46, 58, 43)
incmeans <- tapply(incomes, statef, mean)
# incmeans <- tapply(incomes, state, mean)

stdError <- function(x) sqrt(var(x)/length(x))
incster <- tapply(incomes, state, stdError)

confidence95 <- function(x) qt(1 - 0.05 / 2, df = length(x) - 1) * sd(x) / sqrt(length(x))
incconf95 <- tapply(incomes, state, confidence95)

incttest95 <- tapply(incomes, state, t.test)

mons = c("March","April","January","November","January","September","October","September","November","August","January","November","November","February","May","August","July","December","August","August","September","November","February","April")
monsf = factor(mons, levels=c("January","February","March","April","May","June","July","August","September","October","November","December"), ordered = TRUE)
