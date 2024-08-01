#exercise 3
#a

pneumonia<-read.table(file=file.choose() ,header = T)
attach(pneumonia)

#because we have some zeros i add the 0.5
emperical_logit_1<-(c1+0.5)/(c2+c3+0.5)  
emperical_logit_2<-(c1+c2+0.5)/(c3+0.5) 
#plot emp logits
par(mfrow=c(1,2))
plot(period,log(emperical_logit_1))
plot(period,log(emperical_logit_2))

#b
#tranformation with log
par(mfrow=c(1,2))
plot(log(period),log(emperical_logit_1))
plot(log(period),log(emperical_logit_2))  #the plot is same as before so its ok 

#c
library(VGAM)

m1<-vglm(cbind(c1,c2,c3)~log(period),cumulative(parallel=T))
summary(m1)

anova(m1,type= "III") ## The log(period) time effect is statistically significant .

round(exp(coef(m1)),3)

#a1:intercept for severity 1
#a2:intercept for severity 2
#b:if we increase the log(period) by one unit the log odds
#of being in a higher category of pneumonia severity
#are 92.5% lower