#Assignment 4
#exercise 1
#a)



# interest lies into admission rates to psychiatric hospitals according to the
# phase of the moon (Before full moon, During, After) and the month.
# Interest is in the moon effect.
# y_i ~ Normal(\mu_i,\sigma^2), i = 1,...,36 assume independence
# M1: E(y_i|moon_i) = b_0 + b_1*z_i1 + b_2*z_i2
# z_i1 = 1 (before), z_i2 = 1 (during)
# M2: E(y_i|moon_i,month_i) = b_0 + b_1*z_i1 + b_2*z_i2 + c1*w_i1+...+c11*w_i11

data<-read.table(file.choose(),header=T)
attach(data)

Moon<-factor(Moon)
str(Moon)
Month<-factor(Month)
str(Month)


m1<-glm(Admissions~Moon,gaussian)
summary(m1)
#dispersion parameter phi=17.67
anova(m1,test="F") ##p-value=0.32>0.05=a , effect of the moon not statistically significant

#b)
m2<-glm(Admissions~Moon+Month,gaussian)
summary(m2)
#by adding the month effect we have reduced the deviance by much
anova(m2,test = "F")
#small p values for both Moon,Month,  both statistically significant

#c)
rss_m1 <- 
rss_m2 <- sum(resid(m2)^2)

var1<-(sum(resid(m1)^2))/33
var2<-(sum(resid(m2)^2))/22

var1  #[1] 17.67886
var2  #[1] 5.809975
#better the smaller variance of the m2 model
#also check them in summary-> dispersion parameter

#d)
plot(fitted(m2),rstandard(m2)) ## We have a good fit  ,i use standardized residuals

shapiro.test(rstandard(m2)) #p value=0.98, we can assume normality for the residuals
