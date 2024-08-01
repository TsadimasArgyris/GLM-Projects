#exercise 4
library(VGAM)
#a ordinal
Frequency_of_visits<-factor(c(2,10,20))
normal<-c(43,16,3)
no_normal<-c(6,11,10)
never<-c(9,18,16)
visits<-rbind(normal,no_normal,never)

m1<-vglm(visits~Frequency_of_visits,cumulative(parallel=T))
summary(m1)
1 - pchisq(0.3144,2) # do not reject
anova(m1,type="III")
# p-value<0.05 we reject the null so the frequency of visits
#is statistically significant
exp(coef(m1)) 

# Lets also check the if the proportionality assumption holds
m2<-vglm(visits~Frequency_of_visits,cumulative)
summary(m2) #saturated model ,proportionallity holds

#b multinomial
m3<-vglm(visits~Frequency_of_visits,multinomial)
summary(m3) #saturated model
anova(m3,type="III")
# p-value<0.05 we reject the null so the frequency of visits
#is statistically significant

#so proportional model better

