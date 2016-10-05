library("psych")
library("ggplot2")
library("GGally")
library("dplyr")
describe(American_Express)
glimpse(American_Express)
boxplot(Costs ~ Miles, data = American_Express)
boxplot(Miles ~ Costs, data = American_Express)
qplot(data = American_Express, Miles)
qplot(data = American_Express, Miles, xlab = "Мили", ylab = "Расходы путешественников")
qplot(data = American_Express, Costs)
qplot(data = American_Express, Costs, xlab = "Расходы")
qplot(data = American_Express, Miles, Costs)
model <- lm(data = American_Express, Costs~Miles)
model
beta_hat <- coef(model)
beta_hat
eps_hat <- residuals(model)
eps_hat
y <- American_Express$Costs
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
