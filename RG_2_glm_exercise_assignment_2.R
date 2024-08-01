#exercise 2

#a
y = c(1,1,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,1,0,0,0,0,0)
temps = c(53,57,58,63,66,67,67,67,68,69,70,70,70,70,72,73,75,75,76,76,78,79,81)

m1=glm(y~temps,binomial)

summary(m1)

1-pchisq(20.315,21) #p value=0.50>0.05=a so i fail to reject the null,model good fit

anova(m1,test="Chisq")
#with the p value we Test 
#the significance of the linear effect 
#of temperature on the probability of failure

##we check the temps row,at the Pr(>|z|) column
#the p-value is 0.032 so the effect of temperature
#is statistically significant.

#we reject the null hypothesis
#(no relationship between temperature and the probability of O-ring failure)



#b
# predict the prob of failure at temps=31

predict(m1,newdata = list(temps = c(31)),type = "response")

#c
#chechking GOF Pearson residuals
res_m1=residuals(m1,type="pearson")
plot(temps,res_m1)

#there is clearly patterns on the plot,so m1 is not a good fit for the data
# residuals not informative
# ungrouped - ONLY HOSMER-LEMSHOW FOR GOF

#Hosmere Lemeshow test a better alternative in this case
HL_m1 = hoslem.test(m1$y,fitted(m1),g = 5)
HL_m1
#do not reject, model good fit for the data
#d
m2_temp <- factor(ifelse(temps>=70, "high", "low"))
m2=glm(y~m2_temp,binomial)
summary(m2)
1-pchisq(27.506,21)

#p value is over 0.05 so i fail to reject the null hypothesis ,
#so the new variable is statistically insignificant