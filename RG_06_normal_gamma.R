#exercise 6

#a)
airflow = c(80,80,75,62,62,62,62,62,58,58,58,58,58,58,
            50,50,50,50,50,56,70)

temp = c(27,27,25,24,22,23,24,24,23,18,18,17,
         18,19,18,18,19,19,20,20,20)

acid = c(89,88,90,87,87,87,93,93,87,80,
         89,88,82,93,89,86,72,79,80,82,91)

loss = c(42,37,37,28,18,18,19,20,15,14,14
         ,13,11,12,8,7,8,8,9,15,15)


shapiro.test(loss)
hist(loss)


m1<-glm(loss~airflow*temp*acid,Gamma)

summary(m1)
anova(m1,test = "F") #temp acid interaction least significant var,remove

m2<-glm(loss~airflow*(temp+acid)+airflow:temp:acid,Gamma)
summary(m2)
anova(m2,test = "F") #three way interaction term least significant,remove

m3<-glm(loss~airflow*(temp+acid),Gamma)
summary(m3)
anova(m3,test = "F") #acid variable least significant,remove

m4<-glm(loss~airflow*(temp+acid)-acid,Gamma)
summary(m4)
anova(m4,test = "F") # airflow acid interaction term is not statistically significant

m5<-glm(loss~airflow+temp+airflow:temp,Gamma)
summary(m5)
anova(m5,test = "F") #all variables statistically significant

m6<-glm(loss~airflow*temp,Gamma(link = log))
summary(m6)
anova(m6,test = "F") 


BIC(m1,m2,m3,m4,m5,m6) #m6 model is best

#b)
plot(fitted(m6),rstandard(m6))  #good plot

#c)

#the airflow and temperature and their interaction are enough to explain the data
#while log function seems to be better than the identity as a link