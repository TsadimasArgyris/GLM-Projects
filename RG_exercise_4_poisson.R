#exercise 4 
#a

flowering<-read.table(file.choose(),header=T)

#emperical logit
emplogit<-((flowering$flowered+0.5)/(flowering$number - flowering$flowered+0.5))
plot((emplogit),flowering$dose)

#creating model 1
variety<-factor(flowering$variety)
y<-cbind(flowering$flowered,flowering$number - flowering$flowered)
m1<-glm(y~flowering$dose+variety,binomial)
summary(m1)

anova(m1,test="Chisq") ##there is variety effect and dose effect
                      #both <0.05 ,statistically significant

1-pchisq(96.769,24) #we reject the null hypothesis 
                    #the model doesn't fit the data well
res.m1 = resid(m1,type = "pearson")
plot(fitted(m1),res.m1)  #there is a pattern so the model is not good

#b

m2<-glm(y~flowering$dose*variety,binomial)
summary(m2)

1-pchisq(51.083,20)  #GOF:we reject the null hypothesis 
                    #the model doesn't fit the data well

anova(m2,test="Chisq") ##there is variety effect and dose effect
                      #both <0.05 ,statistically significant
                      #so is the interaction effect

res.m2 = resid(m2,type = "pearson")
plot(fitted(m2),res.m2)  #better plot than before

sqrt(sum(res.m2^2)/20) # overdispersion

# interpretation of model parameters

exp(coef(m2))

#bo:the expected ratio for a tree to blossom when dose is zero
#at the variety a is 90% less
#b1:with all the other variables constant ,when dose is raised by a unit
#the probability of flowering is 51% more
#b2:with all the other constant going from variaty a to b the probability
#increases 2.037%
#b3:compared to a ,for c is 253% more probable
#b4: >>     >>   ,for d is 2293% more probable
#b5:compared to a ,for e is 51% less probable
#b6: the effect of dose on b is 29% less than the effect of dose on a
#b7: the effect of dose on c is 21% less than the effect of dose on a
#b8: the effect of dose on d is 26% less than the effect of dose on a
#b9: the effect of dose on e is almost equal to the effect of dose on a

#c
m3<-glm(y~flowering$dose*variety,quasibinomial)
summary(m3)
anova(m3,test = "F") #given the overdispersion,chisq is no longer valid

#variety seems to have no effect in this model

res.m3 = resid(m3,type = "pearson")
plot(fitted(m3),res.m3)

1-pchisq(51.083,20)  #GOF:we reject the null hypothesis 
                    #the model doesn't fit the data well
