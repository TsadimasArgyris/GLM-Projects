#exercise 10

data<-read.table(file.choose(),header = T)
attach(data)
str(data)
hist(Volume)
shapiro.test(Volume) #reject normality

qqnorm(Volume)
qqline(Volume)
#so we will choose a gamma distribution

m1<-glm(Volume~Diam*Height,Gamma)
m2<-glm(Volume~Diam*Height,Gamma(link = log))
BIC(m1,m2)

summary(m2)
anova(m2,test = "F") #all terms statistically significant
plot(fitted(m2),rstandard(m2))

new_diam <- 15.5
new_heights <- c(60,70,75,80,85,90)
new<-data.frame(Diam=new_diam,Height=new_heights)
predict(m2,newdata = new,type = "response")

#given the values above, we get the predicted values for Volume
#for each one of them
#       1        2        3        4        5        6 
#33.23395 36.19378 37.77113 39.41722 41.13504 42.92773 