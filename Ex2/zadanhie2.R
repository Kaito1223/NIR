library("lmtest")
library("GGally")
library("car")

data = Seatbelts
help(Seatbelts)

summary(data)


model1 <- lm(rear ~ law + kms + PetrolPrice, data)
summary(model1)

#1
vif(model1) 
# The value of VIF is 1.40, 1.40 and 1.23 accordingly 
# Therefore we can say the regressors are not linear dependent


#2
model_check1 <- lm(law ~ kms + PetrolPrice, data)
summary(model_check1)
#R square is approximately 0.28, therefore there are no connection between regressor law and 2 regressors kms and PetrolPrice

model_check2 <- lm(kms ~ law + PetrolPrice, data)
summary(model_check2)
#R square is approximately 0.28, therefore there are no connection between regressor kms and 2 regressors law and PetrolPrice

model_check3 <- lm(PetrolPrice ~ kms + law, data)
summary(model_check3)
#R square is approximately 0.2, therefore there are no connection between regressor PetrolPrice and 2 regressors law and kms

# Exclude regressor law
model2 <- lm(rear ~ kms + PetrolPrice, data)
summary(model2)
#the model above has the value of R^2 is 0.18, which implies a bad model


#3
model_log1 <- lm(rear ~ law + kms + PetrolPrice + I(log(kms)),data)
summary(model_log1)
#R square is approximately 0.18

model_log2 <- lm(rear ~ law + kms + PetrolPrice + I(log(PetrolPrice)),data)
summary(model_log2)
#R square is approximately 0.18


#4
model_sq1 <- lm(rear ~ law + kms + PetrolPrice + I(PetrolPrice^2),data)
summary(model_sq1)
#R square is approximately 0.18

model_sq2 <- lm(rear ~ law + kms + PetrolPrice + I(law^2),data)
summary(model_sq2)
#R square is approximately 0.18

model_sq3 <- lm(rear ~ law + kms + PetrolPrice + I(kms^2),data)
summary(model_sq3)
#R square is approximately 0.18

model_pr1 <- lm(rear ~ law + kms + PetrolPrice + I(PetrolPrice * kms),data)
summary(model_pr1)
#R square is approximately 0.18

model_pr2 <- lm(rear ~ law + kms + PetrolPrice + I(law * kms),data)
summary(model_pr2)
#R square is approximately 0.19

model_pr3 <- lm(rear ~ law + kms + PetrolPrice + I(PetrolPrice * law),data)
summary(model_pr3)
#R square is approximately 0.18

#Student's t-test
t_crictical = qt(0.975,df = 188)
t_crictical # ~ 1.973

#Confidence interval
#[estimated weight - t_critical * Std.Error, estimated weight - t_critical * Std.Error]

#Free parameter
#Interval = [391.5 - 1.973 * 53.33, 391.5 + 1.973 * 53.33] = [286.28, 496.72]

#law
#Interval = [-26 - 1.973 * 19.7, -26 + 1.973 * 19.7] = [-64.87, 12.87]
#The interval show that the weight of law can be 0, therefore we can exclude this parameters

#kms
#Interval = [0.01389 - 1.973 *0.002184, 0.01389 + 1.973 *0.002184] = [9.58e-03, 1.82e-02]

#PetrolPrice
#Interval = [-1.920e+03 - 1.973 * 4.989e+0.2, -1.920e+03 + 1.973 * 4.989e+0.2] = [-2904.3279, -935.6703]

#newdata
new.data = data.frame(law = 0.420, kms = 15213, PetrolPrice = 0.09864)
predict(model1, new.data, interval = "confidence")
