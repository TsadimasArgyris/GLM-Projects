#exercise 3

year<-c(1970:1983)
crushes<-c(3,6,4,7,6,2,2,4,1,7,3,5,6,1)
distance<-c(281,276,268,269,281,271,
            265,264,267,265,267,260,
            231,249)
m1<-glm(crushes~log(year) + log(distance),family=poisson)
summary(m1)
anova(m1,test="Chisq")
1-pchisq(14.844,11)  #p-value>0.05 so we fail to reject the null hypothesis
                    #the model fits the data well

#confidence interval to see ,if 0 belongs in there
#the effect of that predictor might not be statistically significant.
confint(m1) 

# Calculate residuals
residuals_m1 <- residuals(m1, type = "pearson")

# Calculate fitted values
fitted_values <- fitted(m1)

# Plot residuals vs fitted values
plot(fitted_values, residuals_m1, xlab = "Fitted Values", ylab = "Pearson Residuals", main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")
#test GOF using residuals
#I do not see any pattern so that means that we do not reject Ho
#the model fits the data well