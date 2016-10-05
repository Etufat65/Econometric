library("psych")
library("ggplot2")
library("GGally")
library("dplyr")
describe(icecream)
glimpse(icecream)
boxplot(temp ~ Sales, data = icecream)
boxplot(Sales ~ temp, data = icecream)
qplot(data = icecream, temp)
qplot(data = icecream, Sales)
model <- lm(data = icecream, Sales~temp)
model
beta_hat <- coef(model)
beta_hat
eps_hat <- residuals(model)
eps_hat
y <- icecream$Sales
y_hat <- fitted(model)
y
y_hat
RRS <- deviance(model)
RRS
TSS <- sum((y - mean(y))^2)
TSS
ESS <- TSS - RRS
R2 <- ESS/TSS
R2
ESS
cor(y,y_hat)
