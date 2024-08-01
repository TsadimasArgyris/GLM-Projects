#exercise 2

#a
install.packages("VGAM")
response<-matrix(c(28,4,41,12,45,12,44,7,29,5,20,3,26,2,20,1),ncol=4)
response
gender<-factor(rep(1:2,times=2))
gender
treatment<-factor(rep(1:2,each=2))
treatment

library(VGAM)

m1<-vglm(response~treatment+gender,cumulative(parallel = T))
summary(m1)
exp(coef(m1))

##exp(b01)=0.27. The odds ratio for those who will have Progressive Treatment for Males and Sequential Treatment 0.27.

##exp(b02)=1.28. The odds ratio for those who will have No_Change Treatment for Males and Sequential Treatment 1.28.

##exp(b03)=3.67. The odds ratio for those who will have Partial_Remission Treatment for Males and Sequential Treatment is 3.67.

##exp(b1)=1.79.  The odds ratio for Alternating Treatment is 79% bigger than
#the odds for Sequential Treatment for both males and females for any category.

##exp(b2)=1.72.  The odds ratio for Females is 72% bigger than the odds ratio 
#for Males for both Sequential and Alternating method for any category.

#b
par(mfrow=c(1,3))
plot(fitted(m1)[,1], resid(m1)[,1])

plot(fitted(m1)[,2], resid(m1)[,2])

plot(fitted(m1)[,3], resid(m1)[,3])

dim(fitted(m1))
dim(resid(m1))
par(mfrow=c(1,1))
#no patterns emerge , good fit

#c

m2<-vglm(response~treatment*gender,cumulative(parallel = T))
summary(m2)
anova(m2)  #interaction not statistically significant ,so m1 better

#d
m1.npo <- vglm(response~treatment+gender,cumulative)
summary(m1)
summary(m1.npo)

#i compare the 2 models with parallel=T and F and check if 
#proportionality assumption is statistically significant
1 - pchisq(5.5677-2.378,7-3) # do not reject so proportionality holds
