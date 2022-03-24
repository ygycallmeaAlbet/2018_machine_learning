# a2q3

# part a ------------------------------

samples <- list()
count <- 1
for(i in 1:7){
  n <- factorial(8) / (factorial(i) * factorial(8 - i))     
  # find the number of combination in this i (8 choose i without put back)
  for(ncol in 1:n){
    samples[[count]] <- combn(8,i)[,ncol]
    #selet the nth colum is one of our samples
    count <- count + 1
  }
}
head(samples,20)                             #head 20: 1 2 3 4 5 6 7 8 (1 2) (1 3)
                                             #       (1 4) (1 5) (1 6) (1 7) (1 8)
                                             #       (2 3) (2 4) (2 5) (2 6) (2 7)


# part b ------------------------------

op = c(1,0,0,1,1,0,1,0)
my_N = length(samples)
my_out = c()

for(i in 1:my_N){
  my_out <- c(my_out, sum(op[samples[[i]]]))
}

y_mu = sum(my_out / 8) / my_N                            # 0.25 = 0.25
# Since the expected value of y is same as the sum then divid y, thus
# the estimator is UNBIASED

# part 3 ------------------------------
y_mu_sq <- y_mu ^ 2
y_sq_mu <- sum(my_out^2 / 8) / my_N 
var <- y_mu_sq - y_sq_mu
var                                                      # -0.5595472
# the variance won't change since there is NO linear relationship between x and y!
