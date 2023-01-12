#let's load the library and the data and take a look at it

library(faraway)
require(faraway)
data(sat)
head(sat)

#now we have to make some changes to the data to check our assumptions

by.math <- sat[order(sat$math),]
by.verbal <- sat[order(sat$verbal),]
by.salary <- sat[order(sat$salary),]
attach(sat)
out <- lm(total ~ expend + ratio + salary + takers)
summary(out)

#let's start by taking a look at some histograms and boxplots of the residuals
windows()
par(mfrow=c(1,1))
qqnorm(residuals(out), ylab="Residuals", main="Normal Probability Plot of Residuals")
qqline(residuals(out))
hist(residuals(out), xlab="Residuals")
boxplot(residuals(out))

#there are some points out of alignment with the qqline on the outer portions of the qqnorm graph
#The histogram appears to be a skewed slightly
#The boxplot seems normal, with one extreme outlier

#let's run the Shapiro test and Anderson-Darling Test to verify what we think we are seeing in the plots
shapiro.test(residuals(out))
install.packages(nortest)
library(nortest)
ad.test(residuals(out))

#The data appear normal based on all the above results (p-values not below .05 level for the shapiro and Anderson-darling tests)

