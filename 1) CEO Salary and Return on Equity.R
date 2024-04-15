### Simple OLS Regression
### Studies the relationship between firm performance and CEO compensation
### salary = B0 + B1_roe + u

library(wooldridge)
head(ceosal1)
cov(ceosal1$roe, ceosal1$salary)
var(ceosal1$roe)
mean(ceosal1$salary)
mean(ceosal1$roe)

#manual calculation of OLS coefficients
(b1hat <- cov(ceosal1$roe, ceosal1$salary)/var(ceosal1$roe))

(b0hat <- mean(ceosal1$salary) - b1hat*mean(ceosal1$roe))

# OLS regression
lm(salary ~ roe, data=ceosal1)

CEOregres <- lm(salary ~ roe, data=ceosal1)

# Scatter plot (restrict y axis limits)
plot(ceosal1$roe, ceosal1$salary, ylim=c(0,4000))
abline(CEOregres) # Add OLS regression line

sal.hat <- fitted(CEOregres) #predicted values
u.hat <- resid(CEOregres) #residuals
roe <- ceosal1$roe
sal <- ceosal1$salary
cbind(roe, sal, sal.hat, u.hat)[1:15,]

summary(CEOregres)


shapiro.test(ceosal1$salary) #CEO salary distribution is not normal
hist(ceosal1$salary, breaks=100)

shapiro.test(ceosal1$sales) #sales distribution is not normal
hist(ceosal1$sales, breaks=100)

log_log_lm <- lm(log(salary)~log(sales), data=ceosal1)
summary(log_log_lm)
# This model shows if sales increase by 1%, the salary of the CEO
# tends to increase by 0.257%. 