# 5.29 table
library(readr)
body = read_table("C:/Users/ds3/Downloads/body.dat.txt", col_names = FALSE)
View(body)
library(dplyr)
body <- rename(body, Weight = X23)
body <- rename(body, Height = X24)
View(body)
lm.fit = lm(body$Weight ~ body$Height, data = body)
summary(lm.fit)

lm.fit=lm(medv~lstat+age ,data=Boston )
summary(lm.fit)

lm.fit=lm(medv~.,data=Boston) # all the variables are taken as predictor
summary(lm.fit)

install.packages("car")
library(car)
vif(lm.fit) # less VIF shows less multicol which is good

lm.fit1=lm(medv~.-age ,data=Boston )
summary(lm.fit1)
# OR
lm.fit1=update(lm.fit , ~.-age)
summary(lm.fit1)

summary (lm(medv~lstat*age ,data=Boston))
lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

lm.fit=lm(medv~lstat)
anova(lm.fit ,lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

lm.fit5=lm(medv~poly(lstat ,5))
summary (lm.fit5)

summary (lm(medv~log(rm),data=Boston))

fix(Carseats) # views the dataset too
names(Carseats)

lm.fit=lm(Sales~.+Income :Advertising +Price:Age ,data=Carseats )
summary (lm.fit) # if there is a qualitative variable then it separates by itself

attach(Carseats)
contrasts(ShelveLoc) # shows how the dummy is using R code for the diff categories

# 6.1
# a)
# y-cap = 123.05 - 8.94x 
# b)
# if the mother smokes then the average birth weight of baby decreases by 8.94 units
123.05 - (8.94*1) # smoker
123.05 # non-smoker

# c)
# Since, p-value is less than 0.05 we can say they have statistically sig relationship b/w avg wt and mother being smoker

# 6.2 
# a)
# y-cap = 120.07 - 1.93 x

# b)
# if the baby is not first born, then weight decreases by 1.93 units
120.07 - 1.93*1 # not first born
120.07 # first born

# c)
# since, the p-value is more than 0.05, we can say that parity and average birth wt does not have a statistically sig relationship

# 6.3
# a)
# y-cap = -80.41 + 0.44 beta_1 -3.33 beta_2 -0.01 beta_3 + 1.15 beta_4 + 0.05 beta_5 -8.40 beta_6

# b)
# For every unit change in gestation, the average weight of the baby increases by 0.44 units keeping all the other variables constant
# For every unit change in age, the avg weight of the baby decreases by 0.01 units keeping all th eother variables constant

# c) The estimate is different in 6.2 and 6.3 for parity as there can be the effect of a confounding variable which is an unknown factor that can affect the dependent and independent varaibles
# there can also be a possibility of multicol

# d) 
-80.41 + 0.44*284 -3.33*0 -0.01*27 + 1.15*62 + 0.05*100 -8.40*0
# Residual = 120 - 120.58 = -0.58

# e) R^2 = 1 - 249.28 / 332.57
# 1- 249.28 / 332.57 * (1236 - 1)/ (1236 - 6 - 1) 

# 6.1
library(readr)
baby <- read.table("week3/babyweights.txt")
View(baby)
lm.fit1 = lm(bwt~smoke,data=baby )
summary(lm.fit1)

# 6.2
lm.fit1 = lm(bwt~parity,data=baby )
summary(lm.fit1)

# 6.3
lm.fit1 = lm(bwt~.,data=baby )
summary(lm.fit1)
