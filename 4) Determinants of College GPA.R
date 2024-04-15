### Multiple Regression Analysis
### studies the relationship of college GPA (colGPA)
### with high school GPA (hsGPA) and achievement test score (ACT)

### colGPA = B0 + B1*hsGPA + B2*ACT + u

library(wooldridge)
head(gpa1)

res <- lm(colGPA ~ hsGPA + ACT, data=gpa1)
summary(res)

# Multiple R-squared is 0.17, suggesting 17.6% of the variance 
# in GPA are explained by the regressors.



## Ceteris Paribus Interpretation and Omitted Variable Bias
beta.hat <- coef(lm(colGPA ~ ACT + hsGPA, data=gpa1))
beta.hat

# Relation between regressors
delta.tilde <- coef(lm(hsGPA ~ ACT, data=gpa1))
delta.tilde

#Ommitted variables formula for beta1.tilde
beta.hat["ACT"] + beta.hat["hsGPA"]*delta.tilde["ACT"]

# Actual regression with hsGPA omitted
lm(colGPA ~ ACT, data=gpa1)


### Standard Errors, Multicollinearity, and VIF
summary(res)

# Extract (SER) standard error of regression
(SER <- summary(res)$sigma)

# regressing hsGPA on ACT for calculation of R2 & VIF
(R2.hsGPA <- summary(lm(hsGPA ~ ACT, data=gpa1))$r.squared)

(VIF.hsGPA <- 1/(1-R2.hsGPA))

# manual calculation of SE of hsGPA coefficient
n <- nobs(res)

(SE.hsGPA <- 1/sqrt(n-1)*SER/sd(gpa1$hsGPA)*sqrt(VIF.hsGPA))
