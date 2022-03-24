mercury <- read.csv("fishermen_mercury.csv")
summary(mercury)
M1 <- lm(MeHg ~ fishpart, data = mercury)
summary(M1)
Mpred <- predict(M1, newdata = data.frame(fishpart = 0:2))
c(diff10 = Mpred[2]-Mpred[1], diff21 = Mpred[3]-Mpred[2])
fishpart <- mercury$fishpart
fishpart
length(fishpart)
head(fishpart,5)
fpnames <- c("none", "tissue_only", "tissue+whole", "whole") # new values
fishpart2 <- rep(NA, length(fishpart)) # allocate space
for(ii in 0:3) {
  fishpart2[fishpart == ii] <- fpnames[ii+1]
}
fishpart
fishpart2
fishpart2 <- factor(fishpart2, levels = fpnames)
fishpart2

M1 <- lm(MeHg ~ fishpart, data = mercury)
summary(M1)
mercury$fishpart <- fishpart2
M <- lm(MeHg ~ fishpart, data = mercury)
summary(M)
coef(M)
M2 <- lm(MeHg ~ fishpart - 1, data = mercury)
coef(M2)
Mfull <- lm(MeHg ~ fishmlwk + fishpart, data = mercury) # full model
Mred <- lm(MeHg ~ fishmlwk, data = mercury) # reduced model
coef(Mfull)
null.ind <- 3:5
null.ind
anova(Mred, Mfull)
