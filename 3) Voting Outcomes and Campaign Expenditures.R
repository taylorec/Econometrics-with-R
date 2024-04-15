### Simple OLS Regression
### studies the relationship between % of 
### campaign expenditures (share) and election outcomes (vote)
### voteA = B0 + B1_shareA + u

library(wooldridge)
head(vote1)

(VOTEres <- lm(voteA ~ shareA, data=vote1))
summary(VOTEres)
cor(vote1$voteA, vote1$shareA)

# Scatter plot with regression line
plot(vote1$shareA, vote1$voteA)
abline(VOTEres)


