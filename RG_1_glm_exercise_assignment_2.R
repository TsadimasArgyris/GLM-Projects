#exercise 1 glm
#A. i create the matrix of the data

Toxic_concentrations <- c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.8610, 1.8839)
Number_of_bugs = c(59, 60, 62, 56, 63, 59, 62, 60) #yn
Number_of_deaths = c(6, 13, 18, 28, 52, 53, 61, 60) # y1

y<-cbind(Number_of_deaths,Number_of_bugs - Number_of_deaths)
# i create the matrix with y1 and yn-y1->#Failures 1-π
y

#calculate the death/no death proportions
#and plot them against the toxic concentration

Death_Proportions<-c(Number_of_deaths/Number_of_bugs)

plot(Toxic_concentrations,Death_Proportions,
     xlab="Dose",ylab = "Proportion Killed",col="black",pch = 16,
     main = "Death Proportions vs. Toxic Concentrations")

#i calculate the empirical logit for each dose


Death_Proportions<-c((Number_of_deaths+0.5)/(not_deaths+0.5)) 
#π/1-π adjusted for infinity correction with 0.5

Death_Proportions

emplogit = log(Death_Proportions) 
plot(Toxic_concentrations,emplogit) #and finally plotting 

#there is a positive linear relationship
#between toxic concentration and death rate
#in both cases but it becomes much stronger in the 2nd plot

#Conclusion: a logistic regression model might be appropriate
#for modeling the probability of bug death based
#on toxic concentrations. But there might be some non-linearity
#in the relationship. It could be worth exploring 
#more complex models, like polynomial  
#to capture potential nonlinear patterns in the data.

#B. we have to Propose a transformation 
#for better interpretation of the finally fitted model

conctransf = 100*Toxic_concentrations - 170 #i choose 100 to avoid decimals
plot(conctransf,emplogit)                   #and 170 to be close to zero
m1 = glm(y~conctransf,binomial)
summary(m1)

1 - pchisq(11.232,6)  #Ho: the model is a good fit, H1:not Ho
                      #take the chi squared test for residual deviance
                      #and its degrees of freedom
                      #GOF for m1, p-value testing significance level at a=0.05

#C

exp(coef(m1))       #to obtain the exponential of the coefficients
                    #which can be interpreted as odds ratios.

predict(m1)         # linear predictors / log odds of death
predict(m1,type = "response")  #probabilities

newscore = c(170-170,177.09-170,184.03-170,188.65-170)
newscore
new = data.frame(conctransf = newscore)
new
predict(m1,newdata = new,type = "response")

#D
resm1 = residuals(m1,type = "pearson") #calculate pearson residuals
plot(conctransf,resm1)                #plot residuals against toxic concentration

#E
#model 2

m2 = glm(y~conctransf+I(conctransf^2),binomial)
summary(m2)
1 - pchisq(3.1949,5)

#F
resm2 = residuals(m2,type = "response")
plot(conctransf,resm2)

anova(m1,m2,test = "Chisq")
1-pchisq(8.04,1)

#G

new
predict(m2,newdata = new,type = "response")
