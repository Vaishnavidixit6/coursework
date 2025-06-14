# 9.1
magnets = read.csv("C:/Users/ds3/Downloads/magnets.csv")
summary(magnets)
head(magnets)
# Q1
# Mean = 3.5

# Q2
str(magnets)
# Active is character or factor

# Q3
mean(magnets$change[1:29])
mean(magnets$change[30:50])

# Q4
sd(magnets$change[1:29])
sd(magnets$change[30:50])

# Q5
boxplot(magnets$change[1:29]) # no outliers
boxplot(magnets$change[30:50]) # 4 outliers
table(magnets$change[30:50])
# Value of 3, 4 and 5 have outliers with 1,2,1 observations respectively 

# 10.1
# Q1
mu <- 3
std <- sqrt(2)
X.var <- rep(0,10^2)
X_bar <- rep(0,10^2)
med <- rep(0, 10^2)
for(i in 1:10^2){
    X <- rnorm(100,mu,std) # rnorm() used to generate random numbers
    X.var[i] <- var(X)
    X_bar[i] <- mean(X)
    med[i] <- median(X)
}
X_bar
med

mean(X_bar)
var(X_bar)

mean(med)
var(med)
# mean squared error for sample median is higher than mse of sample mean 

# Q2 ASK JAKE
a <- 0.5
b <- 5
X.var <- rep(0,10^2)
X_bar <- rep(0,10^2)
med <- rep(0, 10^2)
for(i in 1:10^2){
    X <- runif(100,a,b) # rnorm() used to generate random numbers
    X.var[i] <- var(X)
    X_bar[i] <- mean(X)
    med[i] <- median(X)
}
X_bar
med

mean(X_bar)
var(X_bar)

mean(med)
var(med)

# 10.2
# Q1
pop2 <- read.csv("C:/Users/ds3/Downloads/pop2.csv")
head(pop2)
ex2 <- read.csv("C:/Users/ds3/Downloads/ex2.csv")
head(ex2)
sum(ex2$group == 'HIGH') / nrow(ex2)

# Q2
sum(pop2$group == 'HIGH') / nrow(pop2)

# Q3
P_hat = rep(0,10^5)
for (i in 1:10^5){
  X <- sample(pop2$group, 150, replace = TRUE)
  X
  P_hat[i] <- sum(X == 'HIGH') / length(X)
}
P <- sum(P_hat) / length(P_hat)
P

# Q4
var(P_hat)

# Q5

(P*(1-P)) / 150

# 2.2
# a
45/69
30/34
# b
# H_0: p_C = p_T
# H_a: p_C > p_T
# We write alive on 27 cards representing patients who were alive at
# the end of the study, and dead on 75 cards representing patients
# who were not. Then, we shuffle these cards and split them into two groups:
# one group of size 69 representing treatment, and another group of
# size 34 representing control. We calculate the difference between
# the proportion of dead cards in the treatment and control groups (treatment -
# control) and record this value. We repeat this many times to build a distribution
# centered at p_C - p_T = 0. Lastly, we calculate the fraction of simulations where
# the simulated differences in proportions are p_C - p_T greater than 0. If this fraction is low,
# we conclude that it is unlikely to have observed such an outcome by chance and
# that the null hypothesis should be rejected in favor of the alternative.

# c
41/99 
# Since the p-value is so big we fail to reject null hypothesis
# because we don't have enough evidence to reject the null hypothesis, treatment is not effective

# 2.6
# Hypothesis
# H_0: p_T = p_C
# H_a: p_C < p_T

a <- 10/34
b <- 4/16
a-b

0.4955 / 1.0
# Since the p-value is so big we fail to reject null hypothesis
# because we don't have enough evidence to reject the null hypothesis, yawning is not contagious

# 9.2
# 1
exp <- 3.5
sd_a <- 3
sd_ia <- 1.5
test = rep(0,10^2)
for (i in 1:10^2){
  X_a <- rnorm(29, exp, sd_a)
  X_bar_a <- mean(X_a)
  S_a <- sd(X_a)
  X_ia <- rnorm(21, exp, sd_ia)
  X_bar_ia <- mean(X_ia)
  S_ia <- sd(X_ia)
  test[i] <- (X_bar_a - X_bar_ia) / sqrt((S_a**2 / 29) + (S_ia**2 / 21))
}

mean(test)
sd(test)
qnorm(0.025, mean(test), sd(test))
qnorm(0.975, mean(test), sd(test))

# 2
magnets = read.csv("C:/Users/ds3/Downloads/magnets.csv")
nrow(magnets)
X_bar_a = mean(magnets$change[1:29])
X_bar_b = mean(magnets$change[30:50])
Var_a = var(magnets$change[1:29])
Var_b = var(magnets$change[30:50])
test = (X_bar_a - X_bar_b) / sqrt((Var_a/29) + (Var_b/21))
test
# Not in the 95% interval

# 12.1
# 1
# H_0: E(X) = 0
# H_a: E(X) =! 0

# 2
# Only 30:50 as those are inactive placebo

# 3
exp <- mean(magnets$change[30:50])
sd_a <- sd(magnets$change[30:50])
X_bar_a = rep(0,10^2)
for (i in 1:10^2){
  X_a <- rnorm(21, exp, sd_a)
  X_bar_a[i] <- mean(X_a)
}
mean(X_bar_a)
sd(X_bar_a)
qnorm(0.025, mean(X_bar_a), sd(X_bar_a))
qnorm(0.975, mean(X_bar_a), sd(X_bar_a))
t.test(magnets$change[30:50])
# 95% interval doesn't have the null hypothesis so, reject the null hypothesis meaning that there is a difference in the mean values of the score of pain before and after the treatment for patients that were treated with the inactive placebo


# 13.1
# 1
head(magnets)
t.test(magnets$score1 ~ magnets$active) # failed to reject null

#2
var.test(magnets$score1 ~ magnets$active) # failed to reject null

#3
t.test(magnets$score2 ~ magnets$active) # reject null at 1% los

#4
var.test(magnets$score2 ~ magnets$active) # failed to reject null at 1% los

# 5.20
#1
# m = slope = r * std y-axis / std x-axis
# y-bar = m*x-bar + b
m <- 0.67*(9.41 / 10.37)
m
b <- 171.14 - m*108.20
b
# y = 0.6078749 x + 105.3571

#2
# Intercept = If shoulder is 0 units then height will be 105.3571 units
# Slope: For every unit change in shoulder girth, the height will increase by 0.6078749 units

#3
coeff_of_det <- 0.67**2
coeff_of_det
# The predictor variable / shoulder girth explains 44.89% of the changes in the dependent variable / height in inches

#4 
0.6078749*100 + 105.3571

#5
160 - (0.6078749*100 + 105.3571)
# The predicted was -6.14459 units from the actual value

#6
# This linear model is not good for x = 56 cam because of the scale in x-axis starts at 80cm

# 5.29
# 1
# For every unit change in height, the weight increases by 1.0176 units

# 2
# y-cap = 1.0176 x-cap - 105.0113
# When height = 0 units then the weight is going to be 105.0113

# 3
# H_0: Beta_1 = 0
# H_a: Beta_1 =! 0
# p-value = 0.000
# Since, p-value is less than 0.05, we can reject the null hypothesis meaning that height is a good predictor of height

# 4
coeff_of_det <- 0.72^2
coeff_of_det
# The predictor variable / height explains 51.84% of the changes in the dependent variable / weight in units


# 3.6.2
library(MASS)
install.packages("ISLR")
library(ISLR)

fix(Boston)
names(Boston)

lm.fit=lm(medv ~ lstat)
lm.fit=lm(medv ~ lstat, data = Boston)
attach(Boston)
lm.fit=lm(medv ~ lstat)
lm.fit
summary(lm.fit)

names(lm.fit)
coef(lm.fit)
confint(lm.fit)

predict(lm.fit ,data.frame(lstat=c(5,10 ,15)), interval = "confidence")
predict (lm.fit ,data.frame(lstat=c(5,10 ,15)),interval ="prediction")
# A confidence interval gives a range for the mean response at a given value of the predictor.
# A prediction interval gives a range where you expect a new individual observation to fall, given the predictor value.
plot(lstat ,medv)
abline(lm.fit)
abline(lm.fit ,lwd =3)
abline(lm.fit ,lwd=3,col ="red") # line colored
plot(lstat ,medv ,col="red") # points colored
plot(lstat ,medv ,pch =20)
plot(lstat ,medv ,pch ="+")
plot(1:20,1:20,pch =1:20) # axis 1 to 20 scale and shapes at pch

par(mfrow=c(2,2)) # it makes 2*2 format of the graphs
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit)) # gives leverage value
which.max(hatvalues (lm.fit)) # this shows the observation which has the highest leverage which can have potential to be influential

