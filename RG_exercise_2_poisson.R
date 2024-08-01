#exercise 2
#a

Deaths<-c(32,104,206,186,102,2,12,28,28,31)
Smoker<-factor(rep(0:1,each=5))
Age_group<-c(0,1,2,3,4)
Human_years<-c(52407,43248,28612,
               12663,5317,18790,
               10673,5710,2585,1462)
options(scipen = 999)
m1<-glm(Deaths~Smoker,poisson,
        offset =log(Human_years))
summary(m1)

anova(m1,test = "Chisq") ##Smoker coefficient is statistically significant
                        #since its p-value is <0.05

#because i have signs of overdispersion i will adjust my model

quasi.m1<-glm(Deaths~Smoker,quasipoisson,
              offset =log(Human_years))

summary(quasi.m1)

anova(quasi.m1,test = "Chisq") ##Smoker coefficient is statistically significant
#since its p-value is <0.05


#b
exp(coef(m1))
exp(coef(quasi.m1))#β1:the death rate is different with the expected death rate 
                #for non-smokers being 48% smaller than that of the smokers
                ### β0 = 0.004 means that the expected numbers of deaths 
                #per human years is 4 to 1000 people

##OVERDISPERSION: residual deviance>>>degrees of freedom


#c

fage<-factor(rep(Age_group,times=2))
print(fage)
m2<-glm(Deaths~fage,poisson,offset = log(Human_years))
summary(m2)
1-pchisq(23.99,5) #reject the null hypothesis
                  #the model doesn't fit the data well

anova(m2,test = "Chisq")

#age is statistically significant with p=value<0.05
#effect of age into the rate of death exists

#d
m3<-glm(Deaths~Smoker*fage,poisson,offset = log(Human_years))
summary(m3)  
#overfitting , more variables than observations
#useless model

#e

#we will fit a simpler model
m4<-glm(Deaths~Smoker+fage,poisson,offset=log(Human_years))
summary(m4)


1-pchisq(12.132,4) #we reject Ho because p-value is 0.016
                  #the model doesn't fit the data well

#chechking GOF Pearson residuals
res.m4=residuals(m4,type="pearson")
plot(fitted(m4),res.m4)
#we do not see patterns so we suppose it is good


#intepretation of the model parameters
anova(m4,test = "Chisq")    #smoking and age are statistically significant
                          #since p values are <0.05

#f
exp(coef(m4))

#β0:represents the baseline death rate for non-smokers 
#in the youngest age group per human year.
#β1: effect of being a smoker on the death rate, controlling for age,
#being
#β2,3,4,5: how many times the group death rate increases from the previous ,
#with smoking remaining the same
