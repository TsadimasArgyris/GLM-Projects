#exercise 5
#a
data<-read.table(file.choose(),header=T)

orobanche<-factor(data$Orobanche)
extract<-factor(data$extract)
y<-cbind(data$count,data$sample-data$count)
m1<-glm(y~orobanche*extract,binomial)
summary(m1)

1-pchisq(33.278,17)  #p-value <0.05 so we reject the null hypothesis
                    #that the interaction has no effect on the response variable
                    #the model fits the data well

#we can see patterns so the model does not fit the data well
#also we have overdispersion because (res.dev/residual df)>1.5
res.m1=residuals(m1,type="pearson")
plot(fitted(m1),res.m1)

anova(m1,test = 'Chisq')
#orobanche not statistically significant p-value >0.05
#extract is statistically significant p-value <0.05
#interaction term is statistically significant p-value <0.05

pearson_resid <- residuals(m1, type = "pearson")
phi_estimate <- sum(pearson_resid^2) / m1$df.residual

#b

m2<-glm(y~orobanche*extract,family=quasibinomial)

summary(m2)

anova(m2,test="F")

##I see in this case I do not have orobanche effect neither interaction effect.

m3<-glm(y~extract,quasibinomial)
summary(m3)

pearson_resid <- residuals(m3, type = "pearson")
phi_estimate <- sum(pearson_resid^2) / m3$df.residual


plot(fitted(m3),resid(m3,type="pearson")) #plot is good

exp(coef(m3))

## b0 = 0.60 which means that the expected odd of successfully
#planting beans is 0.60%

## b1= 2.88 which means that the expected odd ratio of successfully
#planting cucumber is 188% higher than planting beans 

