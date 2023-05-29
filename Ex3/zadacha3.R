install.packages("devtools")
devtools::install_github("https://github.com/bdemeshev/rlms")

library("lmtest")
library("rlms")
library("dplyr")
library("GGally")
library("car")
library("sandwich")
library("foreign")
library("haven")
library("devtools")

df <- read_sav("r11i_os26b.sav")

data = select(df, gj13.2, gh5, g_educ, status, gj6.2, g_marst)
names(df)
data2 = na.omit(data)#remove missing values
#gh5-gender, g_educ education, status city status, gj13.2 salary, gj6.2 work time in a week,g_marst marital status

data$wed1 <- as.numeric(data$g_marst == 2)
data$wed2 <- as.numeric(data$g_marst %in% c(4, 5))
data$wed3 <- as.numeric(data$g_marst == 1)
data$city_status <- as.numeric(data$status == 2)
data2$status[which(data2$status!=2)] <- 0
data2$status[which(data2$status==2)] <- 1
data$sex <- as.numeric(data$gh5 == 1)
data2$gh5[which(data2$gh5!=1)] <- 0
data2$gh5[which(data2$gh5==1)] <- 1
data$higher_education <- as.numeric(data$g_educ >= 21)

#1 Build linear regression and adding VIF
lm_model <- lm(gj13.2 ~ gh5+g_educ+status+gj6.2+g_marst, data = data2)
vif(lm_model)

#2 Experiment with the functions of real parameters: use logarithms, powers
data$log_worktime <- log(data$gj6.2)

powers <- seq(0.1, 2.0, by = 0.1)

for (power in powers)
{
  data$gh_power <- data$gh5 ^ power
  data$g_educ_power <- data$g_educ ^ power
  lm_model2_0 <- lm(gj13.2 ~ log_worktime + gh5_power + wed1 + wed2 + wed3 + g_educ_power, data = data)
  vif(lm_model2_0)
  summary(lm_model2_0)
}

data$gh5_power_0.1 <- data$gh5^0.1
data$g_educ_power_0.1 <- data$g_educ^0.1
data$gh5_power_0.2 <- data$gh5^0.2
data$g_educ_power_0.2 <- data$g_educ^0.2
data$gh5_power_0.3 <- data$gh5^0.3
data$g_educ_power_0.3 <- data$g_educ^0.3
data$gh5_power_0.4 <- data$gh5^0.4
data$g_educ_power_0.4 <- data$g_educ^0.4
data$gh5_power_0.5 <- data$gh5^0.5
data$g_educ_power_0.5 <- data$g_educ^0.5
data$gh5_power_0.6 <- data$gh5^0.6
data$g_educ_power_0.6 <- data$g_educ^0.6
data$gh5_power_0.7 <- data$gh5^0.7
data$g_educ_power_0.7 <- data$g_educ^0.7
data$gh5_power_0.8 <- data$gh5^0.8
data$g_educ_power_0.8 <- data$g_educ^0.8
data$gh5_power_0.9 <- data$gh5^0.9
data$g_educ_power_0.9 <- data$g_educ^0.9
data$gh5_power_1.0 <- data$gh5^1.0
data$g_educ_power_1.0 <- data$g_educ^1.0
data$gh5_power_1.1 <- data$gh5^1.1
data$g_educ_power_1.1 <- data$g_educ^1.1
data$gh5_power_1.2 <- data$gh5^1.2
data$g_educ_power_1.2 <- data$g_educ^1.2
data$gh5_power_1.3 <- data$gh5^1.3
data$g_educ_power_1.3 <- data$g_educ^1.3
data$gh5_power_1.4 <- data$gh5^1.4
data$g_educ_power_1.4 <- data$g_educ^1.4
data$gh5_power_1.5 <- data$gh5^1.5
data$g_educ_power_1.5 <- data$g_educ^1.5
data$gh5_power_1.6 <- data$gh5^1.6
data$g_educ_power_1.6 <- data$g_educ^1.6
data$gh5_power_1.7 <- data$gh5^1.7
data$g_educ_power_1.7 <- data$g_educ^1.7
data$gh5_power_1.8 <- data$gh5^1.8
data$g_educ_power_1.8 <- data$g_educ^1.8
data$gh5_power_1.9 <- data$gh5^1.9
data$g_educ_power_1.9 <- data$g_educ^1.9
data$gh5_power_2.0 <- data$gh5^2.0
data$g_educ_power_2.0 <- data$g_educ^2.0

lm_model2_0.1 <- lm(gj13.2 ~ log_worktime + gh5_power_0.1 + wed1 + wed2 + wed3 + g_educ_power_0.1, data = data)
vif(lm_model2_0.1)
summary(lm_model2_0.1)
#Multiple R-squared:  0.005756
#Adjusted R-squared:  0.004048

lm_model2_0.2 <- lm(gj13.2 ~ log_worktime + gh5_power_0.2 + wed1 + wed2 + wed3 + g_educ_power_0.2, data = data)
vif(lm_model2_0.2)
summary(lm_model2_0.2)
#Multiple R-squared:  0.00578
#Adjusted R-squared:  0.004072

lm_model2_0.3 <- lm(gj13.2 ~ log_worktime + gh5_power_0.3 + wed1 + wed2 + wed3 + g_educ_power_0.3, data = data)
vif(lm_model2_0.3)
summary(lm_model2_0.3)
#Multiple R-squared:  0.005792
#Adjusted R-squared:  0.004085

lm_model2_0.4 <- lm(gj13.2 ~ log_worktime + gh5_power_0.4 + wed1 + wed2 + wed3 + g_educ_power_0.4, data = data)
vif(lm_model2_0.4)
summary(lm_model2_0.4)
#Multiple R-squared:  0.005796
#Adjusted R-squared:  0.004089

lm_model2_0.5 <- lm(gj13.2 ~ log_worktime + gh5_power_0.5 + wed1 + wed2 + wed3 + g_educ_power_0.5, data = data)
vif(lm_model2_0.5)
summary(lm_model2_0.5)
#Multiple R-squared:  0.005797
#Adjusted R-squared:  0.00409

lm_model2_0.6 <- lm(gj13.2 ~ log_worktime + gh5_power_0.6 + wed1 + wed2 + wed3 + g_educ_power_0.6, data = data)
vif(lm_model2_0.6)
summary(lm_model2_0.6)
#Multiple R-squared:  0.005797
#Adjusted R-squared:  0.00409

lm_model2_0.7 <- lm(gj13.2 ~ log_worktime + gh5_power_0.7 + wed1 + wed2 + wed3 + g_educ_power_0.7, data = data)
vif(lm_model2_0.7)
summary(lm_model2_0.7)
#Multiple R-squared:  0.005797
#Adjusted R-squared:  0.00409

lm_model2_0.8 <- lm(gj13.2 ~ log_worktime + gh5_power_0.8 + wed1 + wed2 + wed3 + g_educ_power_0.8, data = data)
vif(lm_model2_0.8)
summary(lm_model2_0.8)
#Multiple R-squared:  0.005797
#Adjusted R-squared:  0.00409 

lm_model2_0.9 <- lm(gj13.2 ~ log_worktime + gh5_power_0.9 + wed1 + wed2 + wed3 + g_educ_power_0.9, data = data)
vif(lm_model2_0.9)
summary(lm_model2_0.9)
#Multiple R-squared:  0.005797
#Adjusted R-squared:  0.00409 

lm_model2_1.0 <- lm(gj13.2 ~ log_worktime + gh5_power_1.0 + wed1 + wed2 + wed3 + g_educ_power_1.0, data = data)
vif(lm_model2_1.0)
summary(lm_model2_1.0)
#Multiple R-squared:  0.005797
#Adjusted R-squared:  0.00409 

lm_model2_1.1 <- lm(gj13.2 ~ log_worktime + gh5_power_1.1 + wed1 + wed2 + wed3 + g_educ_power_1.1, data = data)
vif(lm_model2_1.1)
summary(lm_model2_1.1)
#Multiple R-squared:  0.005797
#Adjusted R-squared:  0.00409 

lm_model2_1.2 <- lm(gj13.2 ~ log_worktime + gh5_power_1.2 + wed1 + wed2 + wed3 + g_educ_power_1.2, data = data)
vif(lm_model2_1.2)
summary(lm_model2_1.2)
#Multiple R-squared:  0.005797
#Adjusted R-squared:  0.00409 

lm_model2_1.3 <- lm(gj13.2 ~ log_worktime + gh5_power_1.3 + wed1 + wed2 + wed3 + g_educ_power_1.3, data = data)
vif(lm_model2_1.3)
summary(lm_model2_1.3)
#Multiple R-squared:  0.005797
#Adjusted R-squared:  0.00409 

lm_model2_1.4 <- lm(gj13.2 ~ log_worktime + gh5_power_1.4 + wed1 + wed2 + wed3 + g_educ_power_1.4, data = data)
vif(lm_model2_1.4)
summary(lm_model2_1.4)
#Multiple R-squared:  0.005797
#Adjusted R-squared:  0.00409 

lm_model2_1.5 <- lm(gj13.2 ~ log_worktime + gh5_power_1.5 + wed1 + wed2 + wed3 + g_educ_power_1.5, data = data)
vif(lm_model2_1.5)
summary(lm_model2_1.5)
#Multiple R-squared:  0.005797
#Adjusted R-squared:  0.00409 

lm_model2_1.6 <- lm(gj13.2 ~ log_worktime + gh5_power_1.6 + wed1 + wed2 + wed3 + g_educ_power_1.6, data = data)
vif(lm_model2_1.6)
summary(lm_model2_1.6)
#Multiple R-squared:  0.005797
#Adjusted R-squared:  0.00409

lm_model2_1.7 <- lm(gj13.2 ~ log_worktime + gh5_power_1.7 + wed1 + wed2 + wed3 + g_educ_power_1.7, data = data)
vif(lm_model2_1.7)
summary(lm_model2_1.7)
#Multiple R-squared:  0.005797
#Adjusted R-squared:  0.00409 

lm_model2_1.8 <- lm(gj13.2 ~ log_worktime + gh5_power_1.8 + wed1 + wed2 + wed3 + g_educ_power_1.8, data = data)
vif(lm_model2_1.8)
summary(lm_model2_1.8)
#Multiple R-squared:  0.005797
#Adjusted R-squared:  0.00409 

lm_model2_1.9 <- lm(gj13.2 ~ log_worktime + gh5_power_1.9 + wed1 + wed2 + wed3 + g_educ_power_1.9, data = data)
vif(lm_model2_1.9)
summary(lm_model2_1.9)
#Multiple R-squared:  0.005797
#Adjusted R-squared:  0.00409 

lm_model2_2.0 <- lm(gj13.2 ~ log_worktime + gh5_power_2.0 + wed1 + wed2 + wed3 + g_educ_power_2.0, data = data)
vif(lm_model2_2.0)
summary(lm_model2_2.0)
#Multiple R-squared:  0.005797
#Adjusted R-squared:  0.00409 

#3 Choose the best model from the last 2 models
summary(lm_model)
summary(lm_model2)#this model is better

#4 Make a conclusion about which individuals receive the highest salary.
coef(lm_model)#If the coef of the lm_model2 is positive, depending on education and age the salary increases.

#5 Subset the data for urban, unmarried/divorced women with higher education
subset_data <- filter(data, city_status == 1 & wed1 == 0 & wed2 == 1 & higher_education == 1)
lm_subset_data <- lm(gj13.2 ~ city_status + g_educ + g_marst, data=subset_data)
coef(lm_subset_data)
summary(lm_subset_data)
