#Assigment 2 
#30613017 Yuan Gnegyao

#Qusetion 1
##a
air <- read.csv("airfare.csv")
pairs(~fare + pass + lead_share + lead_fare + low_share + low_fare,data = air)


##b
q1a_fare <-lm(formula = fare ~ dist + pass + lead_fare, data = air)
summary(q1a_fare)

#c
f1 <- data.frame(dist = 200, pass = 200,lead_fare = 160)
f2 <- data.frame(dist = 200, pass = 150,lead_fare = 120)
est_f1 <- predict(object = q1a_fare,newdata=f1)
est_f2 <- predict(object = q1a_fare,newdata=f2)
est_f1 - est_f2


#Q2

#a

lead_rate <- cut(air$lead_fare/air$dis,
                 breaks=c(0,0.14,0.21,100),
                 labels=c("low","med","high"))

head(lead_rate)         
tapply(air$fare,lead_rate,sd)

#b
#i


# ii
q2b <- lm(fare ~ pass + lead_rate, data=air)
summary(q2b)

#c
#i
full <- lm(formula = fare ~ pass + lead_rate + pass:lead_rate, data = air)
summary(full)
red <- lm(formula = fare ~ pass + lead_rate, data = air)
summary(red)

fobs <- (sum(residuals(red)^2) - sum(residuals(full)^2))/(red$df - full$df)/(sum(residuals(full)^2)/full$df)
fobs
p <- pf(fobs,df1=(red$df-full$df),df2=full$df,lower.tail = FALSE)
p
#ii
anova(mred,m_full)


#Q3
#a
q3a <- lm(formula = log(fare) ~ log(dist) + log(pass),data = air)
summary(q3a)

