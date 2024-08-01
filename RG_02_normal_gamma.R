#exercise 2
#a)
ball <- factor(rep(c(1,2),each=8))
temp <- factor(rep(c(1,2),each=4,times=2))
age <- factor(c(1,1,2,2,2,2,1,1,2,2,1,1,2,2,1,1))
distance <- c(540,567,553,465,637,562,613,685,467,412,497,
             525,647,619,719,673)
data = data.frame(ball,temp,age,distance)

shapiro.test(distance)

hist(distance, freq = FALSE, main = "Histogram of dist with Normal Density Line", 
     xlab = "dist", ylab = "Density", col = "blue")

mean_distance <- mean(distance)
sd_distance <- sd(distance)
curve(dnorm(x, mean = mean_distance, sd = sd_distance), 
      col = "red", lwd = 2, add = TRUE)


m1<-glm(distance~ball*temp*age,gaussian)
summary(m1)
anova(m1,test="F")  #interaction between temp and age the least significant variable,remove

m2<-glm(distance~ball*(temp+age)+ball:temp:age,gaussian)
summary(m2)
anova(m2,test="F") #three way interaction least significant variable,remove

m3<-glm(distance~ball*(temp+age),gaussian)
summary(m3)
anova(m3,test="F") #ball variable least important here,remove

m4<-glm(distance~temp*age+ball:temp,gaussian)
summary(m4)
anova(m4,test="F") #interaction between temp and age should be removed here


m5<-glm(distance~temp+age+ball:temp,gaussian)
summary(m5)
anova(m5,test="F") # the models looks ok 

m6<-glm(distance~temp+age,gaussian)
summary(m6)
anova(m6,test="F") #all variables left are statistically significant

AIC(m1,m2,m3,m4,m5,m6)  #m5 model looks better in AIC comparison

#temperature age and the interaction between ball and temp are important to explain the data

#Apply multiple comparisons for explaining the statistically significant interactions of the model.
library(emmeans)
emmeans(m3,pairwise~ball*temp,adjust="bonferroni",type="response")
#we take estimates of differences

emmeans(m3,pairwise~temp|ball,adjust="tukey",type="response") 

