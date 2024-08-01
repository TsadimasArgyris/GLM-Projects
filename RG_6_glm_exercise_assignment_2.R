#Exercise 6
#a
personality_type<-factor(c("A","A","B","B"))

cholesterol_level<-factor(c("Normal","High","Normal","High"))

normal_bp<-c(716,207,819,186)
high_bp<-c(79,25,67,22)

y<-cbind(high_bp,normal_bp)

m1<-glm(y~personality_type,binomial) 
#effect of personality type on the probability of high blood pressure

summary(m1)

1-pchisq(deviance(m1),2)  #m1 fits data well, p-value > 0.05
exp(coef(m1))  #the odds of high blood pressure probability
              #for a person with personality type B are 21% lower than A

#b

m2<-glm(y~cholesterol_level,binomial)
#effect of cholesterol on the probability of high blood pressure

summary(m2)

1-pchisq(deviance(m2),2)  #m2 fits data well, p-value > 0.05
exp(coef(m2))     #the odds of high blood pressure probability
                #for a person with normal level cholesterol
                #are 20% lower than a person with high level cholesterol


#c

m3<-glm(y~cholesterol_level+personality_type,binomial)
#effect of personality type and cholesterol with no interaction

summary(m3)

1-pchisq(deviance(m3),1) #m3 fits data well, p-value > 0.05
exp(coef(m3))


#d

m4<-glm(y~cholesterol_level*personality_type,binomial)

summary(m4)

#e

#Aic for model comparisons. (I know that the saturated model is the best one but I do the model comp. using AIC)

AIC(m1,m2,m3,m4) #AIC gives as the best model the m1.

exp(coef(m1))  

#exp(βo):the expexted odds of high blood pressure for those
#with personality type A are 88% less

#exp(β1):the odds of high blood pressure probability
#for a person with personality type B are 21% lower than A

