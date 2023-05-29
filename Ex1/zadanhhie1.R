library("lmtest")
library("GGally")

data = swiss
help(swiss)

data
summary(data)
ggpairs(data)

mean(swiss$Fertility)
var(swiss$Fertility)
sd(swiss$Fertility)

mean(swiss$Examination)
var(swiss$Examination)
sd(swiss$Examination)

model <- lm(Examination~Fertility, data = swiss)
summary(model)

model1 <- lm(Education~Fertility + Examination, data = swiss)
summary(model1)


plot(model1)
ggpairs(model1)


summary(model1)$r.squared


summary(model1)$coefficients


