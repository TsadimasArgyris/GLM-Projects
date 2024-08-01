#exercise 4

#a
wais<-read.table(file.choose(),header=T)

plot(wais$x,wais$s)  #we are just ploting the data,it is not required by the exercise

sorting_table<-table(wais$x,wais$s)
colnames(sorting_table) <- c("Successes","Failures")
print(sorting_table)


proportions <- sorting_table[, "Successes"] / rowSums(sorting_table)

plot(rownames(sorting_table),proportions)

#b
m1<-glm(s~x,binomial,data=wais)
summary(m1)

#c

1-pchisq(51.017,52) #we fail to reject the null, so the model is a good fit
                    #for the data //chi-squared test


sum(resid(m1,type = "pearson")^2) #This line calculates the sum of squared Pearson residuals, 
                                  #which is another way to assess the goodness-of-fit.
                                  #It should be approximately equal to the degrees of freedom 
                                  #for a well-fitting model //pearson residual test
1 - pchisq(51.6,52)

#d

install.packages("ResourceSelection")
library(ResourceSelection)

HL_wais_m1<-hoslem.test(wais$x,fitted(m1),g=5)
HL_wais_m1
m1$y                              #y0:failures, y1=successes
HL_wais_m1$observed               #Not a good GOF. Not a good model.
HL_wais_m1$expected               #low p-value
                            #the observed vs the expected counts are very different
#E

##Pearson's residuals : 

res1<-resid(m1,type="pearson")

plot(fitted(m1),res1)    #the plot is 2 alsmost parallel lines ,
                        #so there is a clear pattern,m1 doesnt fit the data


##F 
#for ROC curve
install.packages("ROCR")
library(ROCR)

probs <- predict(m1, type = "response")
pred.m1 <- prediction(probs, wais$s)
perf.m1 <- performance(pred.m1, "tpr", "fpr")  #true positive rate 
                                        #false positive rate
plot(perf.m1, colorize = TRUE)

perf.m1.auc <- performance(pred.m1, "auc")
str(perf.m1.auc)
auc <- perf.m1.auc@y.values[[1]]
print(paste("AUC:", auc))

#the model's predictive power is good since it is close to 0.8