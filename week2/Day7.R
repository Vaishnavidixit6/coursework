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
estimate_coin_bias <- function(n, p) {
  mean(rbinom(n,1,p))
}

n <- nrow(ex2)
p <- propor
p_hat <- replicate(1e5, estimate_coin_bias(n, p))

