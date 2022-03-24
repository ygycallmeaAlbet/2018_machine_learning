
#qusetion 1 

# part a
dataQ1 =matrix(NA, nrow=3, ncol=10)
dataQ1[1,] =c(100, 95, 125, 105, 100, 90, 135, 120, 85, 101)
dataQ1[2,] =c(90, 110, 85, 90, 95, 110, 115, 110, 105, 120)
dataQ1[3,] =c(250, 175, 140, 200, 195, 165, 145, 180, 210, 180)
rownames(dataQ1) =c("Goose Bar", "$9 Dinner", "Carbwich")
dataQ1

boxplot(t(dataQ1),xlab='Group Name',ylab='contribution')

#part c
contribution = as.numeric(t(dataQ1))
treatments = c(rep('Goose Bar',10), rep('$9 Dinner', 10),rep('Carbwich', 10))
summary( aov(contribution~treatments))

#part g
mu_hat = mean(dataQ1)
tau_hat = apply(dataQ1,1,mean)
sigma_hat = 480
a = c(1,-1,0)
theta = sum(a * tau_hat)
se_theta = sqrt(sigma_hat/10*sum(sigma_hat^2))
tobs = theta / se_theta
2 * pt(tobs, df = 27, lower.tail = FALSE)
