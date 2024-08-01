#Exercise 5
#a


score<-c(0,2,4,5) #hours of snoring

snoring <- matrix(c(24,1355,35,603,21,192,30,224), ncol=2, byrow=TRUE)
#create a matrix with heart failure and no heart attck as columns

plot(score,log((snoring[,1]+0.5)/(snoring[,2]+0.5)))
#plot of empirical logits of the probability of heart failure
# against snoring hours 

#b

# Fit a logistic regression model considering the linear effect
# of average snoring hours.

m1<-glm(snoring~score,family=binomial)

summary(m1)
##parameter interpretations

exp(coef(m1))

##exp(b0) = 0.021 which means that the expected odd of snoring
#is 0.021. which actually means that for every 1000 people
#who do not snore I expect the 21 to have heart failure.

##exp(b1) = 1.49 which means that the expected odd ratio
#for those who snore to have heart failure is 49% bigger
#than those who dont snore. Also means that if we increase
#the snoring hours by 1 unit we expect to have increased odd by 49%

#c

1-pchisq(2.8089,2)  #checking the scaled deviance
                    #p-value > 0.05 so we do not reject the null hypothesis
                    #the model fits the data well


Pearson_residuals<-resid(m1,type="pearson")  ##Pearson's residuals

plot(fitted(m1),Pearson_residuals) #no clear pattern, good fit

#d

newscore<-c(1,2,3)
newscore
new=data.frame(score=newscore)
new
predict(m1,newdata = new,type = "response")
