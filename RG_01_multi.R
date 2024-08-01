#exercise 1

#a
install.packages("VGAM")
library(VGAM)
#importing the data 
not_important<-c(26,9,5,40,17,8)
important<-c(12,21,14,17,15,15)
very_important<-c(7,15,41,8,12,18)
response<-cbind(not_important,important,very_important)

gender<-factor(rep(1:2,each=3))
age<-factor(rep(1:3,times=2))

rating=data.frame(gender,age,response)

rating



m1<-vglm(response~gender+age,multinomial(refLevel="very_important"))


#b
exp(coef(m1))

##Interpretations:

##exp(b001)=2.83. It means that the odds of No important rating
#(Over to very important rating) is 2.83 when Age =18-23 and Sex=Female.

##exp(b002)=1.57. It means that the odds of the second category of rating
#(Over to very important rating) is 1.57 when Age=18-23 and Sex=Female.

##exp(b101)=2.25. It means that the odds of No important rating for males 
#compared to females is 125% more for each level of age.

##exp(b102)=1.53. It means that the odds of important rating for males
#compared to females is 59% more for each level of age.

##exp(b021)=0.22. It means that the odds of No important rating 
#for ages 24-39 compared to  18-23 is 78% less  for each level of sex.

##exp(b022)=.070. It means that the odds of important rating for ages 
#24-39 compared to  18-23 is 30% less for each level of sex.

##exp(b013)=0.05. It means that the odds of No important rating 
#for ages 40+ compared to  18-23 is 95% less  for each level of sex.

##exp(b023)=0.26.  It means that the odds of important rating for ages 
#40+ compared to  18-23 is 74% less for each level of sex.


#c
anova(m1,type="III")## both age and gender are statistically significant

#d

m2<-vglm(response~gender,cumulative(parallel=T))
summary(m2)
1-pchisq(69.5213,9)

anova(m2,type="III")   #gender important

exp(coef(m2)) 

##exp(b01)=0.36. It means that the odd ratio of no Important rating is 0.36 when sex=female.

##exp(b02)=1.38. It means that the odd ratio of Important rating is 1.38 when sex=female.

##exp(b1)=2.11. Males have 112% more probability than females for each category. 


#e
m3<-vglm(response~gender+age,cumulative(parallel=T))
exp(coef(m3))

#the effect of gender is less in the second model

#exp(b01):
#exp(b02):
#exp(b1):the probability of males is 78% more than females in each category
#exp(b2): the probability of 24-39 age is 68% less than 18-23 age group in each category
#exp(b3): the probability of 40+ age is 90% less than 18-23 age group in each category

#f
m4<-vglm(response~gender+age,cumulative)
summary(m3)
summary(m4)
1-pchisq(deviance(m3)-deviance(m4),7-4)

## P-value>0.0.5. So we assume that b1=b11=b12 and b2=b21=b22 .
#It means that the proportional odds is the same for both models.