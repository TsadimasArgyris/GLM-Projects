#exercise 3 

#a

Radiation_dose<-c(0,5,25,75,150,250)
leukemia<-c(13,5,5,3,4,18)
other_c<-c(378,200,151,47,31,33)

y<-cbind(leukemia,other_c)
m1=glm(y~Radiation_dose,binomial)

summary(m1) #radiation dose is statistically significant 
            #predictor of cancer mortality rates

#b
#testing GOF

options(scipen = 999)

1-pchisq(54.35089,5) ##There is low p value ,so we reject the null
                    #(the model containing only the intercept
                    #is not equal to the saturated/a good one)

pvalue=1-pchisq(deviance(m1),4)

pvalue ##good model



res.m1 = residuals(m1,type = "pearson") #calculate pearson residuals
plot(fitted(m1),res.m1)             #plot residuals against fitted values

#c

##Interpretation  of model parameters.

exp(coef(m1))

## b0 = 0.03 which means that for every 100 people who got the dose
#we expect 3 to die. Or the odds ratio if the dose is 0
#for someone to die from leukemia is 3%.

## b1 = 1.0116 which means that if we increase the dose by 1 unit
#then why expect to have an increase in the odds of deaths by 1.16%.

