#exercise 3

data<-read.table(file.choose(),header = T)

attach(data)
Tree<-factor(Tree)

with(data,plot(Load,Velocity))
hist(Load)
hist(Velocity)

m1<-glm(Velocity~Load*Tree,gaussian)
summary(m1)
anova(m1,test = "F") #tree not statistically significant
plot(fitted(m1),rstandard(m1))

m2<-glm(Velocity~Load,gaussian)
summary(m2)
anova(m2,test = "F") #load statistically significant
plot(fitted(m2),rstandard(m2))

m3<-glm(Velocity~Tree,gaussian)
summary(m3)
anova(m3,test = "F") #tree statistically significant
plot(fitted(m3),rstandard(m3))

m4<-glm(Velocity~Load+Tree,gaussian)
summary(m4)
anova(m4,test = "F") #tree not statistically significant
plot(fitted(m4),rstandard(m4))

AIC(m1,m2,m3,m4)  #m1 better model

m5<-glm(Velocity~Load*Tree,Gamma)
summary(m5)
anova(m5,test = "F") #tree not statistically significant
plot(fitted(m5),rstandard(m5))

m6<-glm(Velocity~Load,Gamma)
summary(m6)
anova(m6,test = "F") #load statistically significant
plot(fitted(m6),rstandard(m6))

m7<-glm(Velocity~Tree,Gamma)
summary(m7)
anova(m7,test = "F") #tree statistically significant
plot(fitted(m7),rstandard(m7))

m8<-glm(Velocity~Load+Tree,Gamma)
summary(m8)
anova(m8,test = "F") #tree not statistically significant
plot(fitted(m8),rstandard(m8))

AIC(m1,m2,m3,m4,m5,m6,m7,m8) #m5 model better, m1 slightly worse


coef(m5)
#  bo:expected velocity for a nut from tree 1 when load is zero
#  b1:falling from tree 2 increases velocity by 0.77 compared to falling from tree 1
#  b2:falling from tree 3 increases velocity by 0.61 compared to falling from tree 1
# b3:increasing load by 1 unit results in decrease of velocity by 1.9
# b4:increasing by 1 load unit while on tree 2, results in velocity reduction of 3.37 compared to tree 1
# b5:increasing by 1 load unit while on tree 3 results in velocity reduction of 2.4 compared to tree 1