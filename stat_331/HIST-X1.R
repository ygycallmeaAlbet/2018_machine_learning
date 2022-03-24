# simulate X2
N <- 1e6
X1 <- rnorm(N) # N(0,1)
B <- rbinom(N, size = 1, prob = .5) # Bernoulli(1/2)
X2 <- ifelse(B == 0, X1, -X1) # if B==0 X2 <- X1, else X2 <- -X1
# histogram of X2
hist(X2, breaks = 100, freq = FALSE) # histogram
curve(dnorm, add = TRUE, col = "red") # superimpose PDF of N(0,1)
Y <- X1 +X2
hist(X1+X2)
