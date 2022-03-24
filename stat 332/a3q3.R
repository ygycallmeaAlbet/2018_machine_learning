#Q3

dataQ1 = matrix(NA, nrow=2, ncol=10)
dataQ1[1,] = c(100, 95, 125, 105, 100, 90, 135, 120, 85, 101) 
dataQ1[2,] = c(90, 110, 85, 90, 95, 110, 115, 110, 105, 120) 
rownames(dataQ1) = c("Goose Bar", "$9 Dinner")
dataQ1

# a
# resposnse: total profits are made in 1 week
# factor: 2 kind of promotionals
# factor levels:
# 1."Goose Bar"
# 2."$9 Dinner"

# b
# treatments: 2     replications: 10
n <- 10

# c
# i
l1 <- dataQ1[1,]
l2 <- dataQ1[2,]
l_total <- c(t1,t2)
mu_hat <- mean(l_total)
mu_hat                                   # mu_hat = 104.3

# ii
tua1 <- mean(l1) - mu.hat
tua1                                     # tua1 = 1.3

# iii
tua2 <- mean(l2) - mu.hat
tua2                                     # tua2 = -1.3

# iv
var1 <- var(t1)
var1
var2 <- var(t2)
var2
sigma_sq <- (9 * var1 + 9 * var2) / 18
sigma_sq                                 # sigma.square = 201.24

# d
a <- 0.975
se_tua <- sqrt(sigma_sq * (1/n + 1/n))
se_tua
diff_low <- tua1 - tua2 - qt(a , 2 * n - 2) * se_tua
diff_high <- tua1 - tua2 + qt(a , 2 * n - 2) * se_tua
diff_low                                # diff_low = -10.72867
diff_high                               # diff_high = 15.92867
t.test(l1,l2,var.equal = T)
