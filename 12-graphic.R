library(ggplot2)
library(dplyr)

a <- sample(1:10)
b <- sample(1:10)

# Plot one vector
plot(a)
# Plot two vectors
plot(b, a)

# Plot a matrix with two columns
m <- cbind(b, a)
plot(m)

# Plot a list with two values
l <- list(x = b, y = a)
plot(l)

# Plot a data frame with two columns
df <- data.frame(b, a)
plot(df)

# Plot with R expr
plot(~ b + a)
plot(a * b ~ b + a)

# Plot with factors
e <- c("a", "a", "a", "b", "c", "b")
f <- factor(e)
fn <- 1:6
plot(f)
plot(f, fn)
pair

# Categorical x 2
d <- dataset %>% count(Field1,Field2)
p <- d %>% ggplot(aes(x = Field1, y = Field2)) + theme(legend.position = "top", axis.text = element_text(size = 6))
print(p + geom_point(aes(size = n)))

# Categorical x binary
d <- dataset %>% count(Field1, Field2)
d[which(d$Field2 == 1), "Field2"] = "Yes"
d[which(d$Field2 == 0), "Field2"] = "No"
p <- d %>% ggplot(aes(x = Field1, y = n, fill = Field2)) + theme(legend.position = "top", axis.text = element_text(size = 6))
print(p + geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0) + theme_minimal())
