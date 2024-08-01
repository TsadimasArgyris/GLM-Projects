#exercise 7

#a)


wind = c(5,6,3.4,2.7,10,9.7,9.55,3.05,8.15,6.20,
         2.90,6.35,4.60,5.80,7.40,3.60,7.85,8.80,
         7,5.45,9.10,10.20,4.10,3.95,2.45)

y = c(1.582,1.822,1.057,0.500,2.236,2.386,2.294,
      0.558,2.166,1.866,0.653,1.930,1.562,1.737,2.088,1.137,2.179,1.112,1.800,
      1.501,2.303,2.310,1.194,1.144,0.123)

hist(y)

qqnorm(y)
qqline(y)
shapiro.test(y)  #we can assume y to be normally distributed


m1<-glm(y~wind,gaussian)
summary(m1)
anova(m1,test ="F")
m2<-glm(y~wind,Gamma)
summary(m2)
anova(m2,test ="F")
AIC(m1,m2)
BIC(m1,m2)

#m1 better in both cases

#b)

m3<-glm(y~wind+I(wind^2),gaussian)
summary(m3)
anova(m3,test = "F")

m4<-glm(y~wind+I(wind^2),Gamma)

AIC(m1,m2,m3,m4)
BIC(m1,m2,m3,m4)
#m3 significantly better

anova(m1,m3,test = "F") #p value=0.0012<0.05=a so we reject the null

#simpler model not sufficient to explain the data

par(mfrow=c(1,2))
plot(fitted(m1),rstandard(m1))
plot(fitted(m3),rstandard(m3))
par(mfrow=c(1,1)) #both plots not good

#c)
m5<-glm(y~wind,Gamma(link = log))
m6<-glm((1/y)~wind,Gamma(link = log))
m7<-glm((1/y)~wind,gaussian)
m8<-glm((1/y)~wind+I(wind^2),gaussian)
m9<-glm(log(y+1)~wind+I(wind^2),gaussian)
m10<-glm(y~(1/wind)+I(wind^2),gaussian)
m11<-glm(y~log(wind)+I(wind^2),gaussian)

AIC(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11)
BIC(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11)
#m12 better
plot(fitted(m14),rstandard(m14))
#we stick with m3,m11

#d)
m12<-glm(y~wind+I(wind^2),gaussian(link = log))
AIC(m3,m12)
#not the same
