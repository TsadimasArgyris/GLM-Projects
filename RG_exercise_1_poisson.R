#exercise 1
#a

new_aids_cases<-c(0,0,3,0,1,1,
                  1,2,2,4,2,8,
                  0,3,4,5,2,2,
                  2,5,4,3,15,12,
                  7,14,6,10,14,8,
                  19,10,7,20,10,19)
months<-seq(1,36,1)
m1<-glm(new_aids_cases~months,poisson)
summary(m1)  

1-pchisq(62.36,34)  # p-value is <0.05 so we reject the null hypothesis
                    #so the model doesn't fit the data well

exp(coef(m1))      #β1: as the month increases by one unit the
                  #new aids cases increase by 8,3%

anova(m1,test="Chisq") # months is a statistically significant variable to the model
#b

month4<-rep(rep(1:3,each=4),3)
fmonth4<-factor(month4)
m2<-glm(new_aids_cases~fmonth4,poisson)
summary(m2)
1-pchisq(170.03,33) ##I notice big scaled deviances
                    #so I see I have overdispersion
                    #the model does not fit the data well

m3<-glm(new_aids_cases~month4,poisson)
summary(m3)
1-pchisq(170.51,34)  ##I notice big scaled deviances
                    #so I see I have overdispersion
                    #the model does not fit the data well

AIC(m2,m3)   #m3 seems to be better fit for the data


#c
new_aids_cases_per_season<-c(3,5,16,12,11,34,37,51,56)
plot(month4,log(new_aids_cases + 0.5))


month_periods<-c(rep(rep(1:3,each=1),3))
m4<-glm(new_aids_cases_per_season~month_periods,poisson)

summary(m4)

1-pchisq(114.54,7) ##I notice big scaled deviances
                  #so I see I have overdispersion
                  #the model does not fit the data well
