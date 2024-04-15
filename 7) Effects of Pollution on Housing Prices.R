### Multiple Regression Analysis

### log(price) = B0+B1*log(nox)+B2*log(dist)+B3*rooms+B4*rooms^2+B5*stratio+u

library(wooldridge)
head(hprice2)

res <- lm(log(price)~log(nox)+log(dist)+rooms+I(rooms^2)+stratio,data=hprice2)
summary(res)


### A one percentage increase in pollution results in a decrease of 0.9 percentage 
### decrease in housing price
### The negative coefficient for rooms and the positive coefficient for rooms^2
### imply that the price decreases for few number of rooms and increases for large number of rooms

# Predictions: values of the regressors:
# rooms = 4-8, all others at the sample mean:
X <- data.frame(rooms=seq(4,8), nox=5.5498, dist=3.7958, stratio=18.4593) 

# Calculate predictions and confidence interval:
pred <- predict(res, X, interval="confidence")

# Table of regressor values, predictions and CI:
(table <- cbind(X, pred))

plot(table$rooms, table$fit, type='l')
