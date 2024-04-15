### Multiple Regression Analysis: Confidence Intervals
### studies the relationship between R&D expenditures of a firm, it's size, 
### and the profit margin for a sample of 32 firms in the chemical industry
### log(rd) = B0 + B1*log(sales) + B2*projmarg + u

library(wooldridge)
head(rdchem)
res <- lm(log(rd) ~ log(sales)+profmarg, data=rdchem)
summary(res)
confint(res) #95% CI
confint(res, level=0.99) #99% CI

### Heteroscedasticity Test
library(lmtest)
bptest(res)
predictions <- (predict(res, rdchem))
df_e <- data.frame(resid=residuals(res), predictions)
library(ggplot2)
ggplot(df_e, aes(predictions, abs(resid))) + geom_point() + geom_smooth()
