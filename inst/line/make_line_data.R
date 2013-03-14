set.seed(1234)
sigma <- 1
beta <- c(0, 1)
N <- 30
maxx <- 5
x <- seq(0, 5, length.out=N)
y <- rnorm(N, beta[2] * x, sigma)
dump(c("x", "y", "N"), file="line_data.R")
