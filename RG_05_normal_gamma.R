#exercise 5
#a)
cholest<-read.table(file.choose(),header=T)
attach(cholest)

par(mfrow=c(1,2))
plot(chol,age)
plot(chol,bmi)
par(mfrow=c(1,1))

shapiro.test(chol) #reject normality

#b)
hist(chol)
m1<-glm(chol~age+bmi,gaussian)
summary(m1)

m2 = glm(chol~age+bmi,family=Gamma(link=log))

summary(m2)

m3 = glm(chol~age+bmi,Gamma)

plot(fitted(m3),rstandard(m3))
summary(m3)


BIC(m1,m2,m3)  #m2 better model
#c)
anova(m1,test = "F") #age,bmi stat significant , in all models
anova(m2,test="F")
anova(m3,test="F")

exp(coef(m2))
## b0 = 1.79 : does not provide interpretation
#(expected cholesterol when bmi,age are zero)

## b1 = 1.0066 which means that if we increase age by one unit we expect cholest to increased by 0.66%.

## b2= 1.037 which means that if we increase bmi by 1 unit we expect cholest to decrease by 3.7%

#d)
plot(fitted(m2),rstandard(m2))  #good plot


