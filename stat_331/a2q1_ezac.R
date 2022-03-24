#A 2
#Q 1
# a
air <- read.csv("airfare.csv")
summary(air)
pairs(~fare + pass + lead_share + lead_fare + low_share + low_fare, data = air)

# b
#sq_lead_fare <- I(lead_fare ^ 2)
m1_fare <-lm(fare ~ dist + pass + lead_fare + I(lead_fare ^ 2), data = air)
summary(m1_fare)
#OR
m2_fare <- lm(fare ~ dist + pass + lead_fare, data = air)
summary(m2_fare)

anova(m2_fare, m1_fare)
#c
f1 <- data.frame(dist=100,pass=150,lead_fare = 160)
f2 <- data.frame(dist = 100, pass = 100,lead_fare = 120)  #change the fisrt 2 item
ef1 <- predict(m1_fare,newdata=f1)
ef2 <- predict(m1_fare,newdata=f2)
ef1-ef2



#b
# ii
m2 <- lm(fare ~ pass + lead_rate, data=air)
summary(m2)

#c
#i
m_full <- lm(fare ~ pass + lead_rate + pass * lead_rate, data = air)
summary(m_full)
#or
m_full <- lm(fare ~ pass + lead_rate + pass:lead_rate, data = air)
summary(m_full)
mred <- lm(fare ~ pass + lead_rate, data = air)
summary(mred)

dff <- m_full$df
dfr <-mred$df
ssf <- sum(residuals(m_full)^2)
ssr <- sum(residuals(mred)^2)
fobs <- (ssr -ssf)/(dfr - dff)/(ssf/dff)
fobs
p <- pf(fobs,df1=(dfr-dff),df2=dff,lower.tail = FALSE)
p
#ii
anova(mred,m_full)

#Q3
#a
m3 <- lm(log(fare) ~ log(dist) + log(pass),data = air)
summary(m3)

#b
Y <- log(air$fare)
X <- model.matrix(m3)
beta.hat <- solve(t(X)%*%X)%*%t(X)%*%Y
beta.hat

#rhol1_MLE = 0.34814808
#rhol2_MLE = -0.05643079
#log(rho0) -0.5 * sigma^2 = 3.02929430
# e^(3.02929430 + 0.5 * sigma^2 )
sigma2.hat <- sum(resid(m3)^2)/m3$df
sigma2.hat
rhol0_MLE <- exp(beta.hat[1] + 0.5 * sigma2.hat)
rhol0_MLE