### Simple OLS Regression
### studies the relationship between education and wage
### wage = B0 + B1*education + u

library(wooldridge)
head(wage1)
plot(wage1$wage, wage1$educ)

model <- lm(wage ~ educ, data=wage1)
summary(model)
# One additional year of education is associated with an increase
# of the typical wage by about 54 cents an hour on this data and model. 

# regression plot
plot(wage~educ, data=wage1)
abline(model)

# residual plot
plot(residuals(model))


shapiro.test(wage1$wage) #wage distribution is not normal
hist(wage1$wage)

plot(log(wage1$wage), wage1$educ)
model2 <- lm(log(wage) ~ educ, data=wage1)
summary(model2)
# The semi-logarithmic specification implies that wages are higher by 
# about 8.3% for individuals with an additional year of education. 


### Multiple Regression Analysis
### studies the relationship of log wage with education, experience and tenure
### log(wage) = B0 + B1*educ + B2*exper + B3*tenure + u
model3 <- lm(log(wage) ~ educ+exper+tenure, data=wage1)
summary(model3)


### Standard Errors, Multicollinearity, and VIF
summary(model3)
library(car)
# calculate VIF
vif(model3)


### Reporting Regression Results
library(stargazer)
stargazer(list(model,model2,model3),type="text",keep.static=c("n","rsq"))


### Multiple Regression Analysis with Qualitative Regressors
### studies the relationship of log wage with education, experience and tenure
### log(wage) = B0 + B1*educ + B2*exper + B3*tenure + u
wage1$female <- as.logical(wage1$female)
model4 <- lm(wage ~ female+educ+exper+tenure, data=wage1)
summary(model4)
