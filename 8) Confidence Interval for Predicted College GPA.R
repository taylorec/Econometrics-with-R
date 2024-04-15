### Confidence Intervals for Prediction
### studies the college GPA prediction to support the admission decisions. 
### colgpa = B0+B1*sat+B2*hsperc+B3*hsize+B4*hsize^2+u

library(wooldridge)
head(gpa2)

reg <- lm(colgpa~sat+hsperc+hsize+I(hsize^2), data=gpa2)

#Define three sets of regressor variables
cvalues <- data.frame(sat=c(1200, 900, 1400), hsperc=c(30, 20, 5), hsize=c(5, 3, 1))

# Point estimates and 95% prediction intervals
predict(reg, cvalues, interval="prediction")


### Heteroscedasticity Test
library(lmtest)
bptest(reg)

