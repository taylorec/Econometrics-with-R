### Multiple Regression Analysis: F Tests
### tests whether the performance measures batting (bavg), home runs per year (hrunsyr),
### and runs batted in per year (rbisyr) have an impact on the salary
### H0: B3=0, B4=0, B5=0 versus H1: at least one of these measures matters. 

library(wooldridge)
library(car)
head(mlb1)
res <- lm(log(salary)~years+gamesyr+bavg+hrunsyr+rbisyr, data=mlb1)

myH0 <- c("bavg", "hrunsyr", "rbisyr")
linearHypothesis(res, myH0)

myH0 <- c("bavg", "hrunsyr=2*rbisyr")
linearHypothesis(res, myH0)