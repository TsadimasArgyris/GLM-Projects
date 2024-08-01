#exercise 8
nerve<-read.table(file.choose(),header=T)
attach(nerve)
hist(time)# gamma distribution
shapiro.test(time) #reject normality

model<-glm(time~1,Gamma)
model2<-glm(time~1,Gamma(link = log))
summary(model)
summary(model2)
BIC(model,model2) #equivelant models,lets go with the first

plot(fitted(model),rstandard(model))

coef(model) #0.091 seconds difference

#phi almost equal to 1, gamma distribution good fit for the data