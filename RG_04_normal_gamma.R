#exercise 4 

Mass<-c(77,85.5,63,80.5,79.5,94,66,69,65,58,69.5,73,74,68,80,66,54.5,64,84,73,89,94)

Fore<-c(28.5,29.5,25,28.5,28.5,30.5,26.5,27,26.5,26.5,28.5,27.5,29.5,25,29.5,26.5,24,25.5,30,28,29,31)

Bicep<-c(33.5,36.5,31,34,36.5,38,29,31,29,31,37,33,36,30,36,32.5,30,28.5,34.5,34.5,35.5,33.5)

Chest<-c(100,107,94,104,107,112,93,95,93,96,109.5,102,101,98.5,103,89,92.5,87.5,99,97,106,106)

Neck<-c(38.5,39,36.5,39,39,39,35,37,35,35,39,38.5,38.5,37,40,35,35.5,35,40.5,37,39,39)

Shoulder<-c(114,119,102,114,114,121,105,108,112,103,118,113,115.5,108,117,104.5,102,109,119,104,118,120)

Waist<-c(85,90.5,80.5,91.5,92,101,76,84,74,76,80,86,82,82,95.5,81,76,84,88,82,96,99.5)

Height<-c(178,187,175,183,174,180,177.5,182.5,178.5,168.5,170,180,186.5,188,173,171,169,181,188,173,179,184)

Calf<-c(37.5,40,33,38,40,39.5,38.5,36,34,35,38,36,38,37,37,38,32,35.5,39,38,39.5,42)

Thigh<-c(53,52,49,50,53,57.5,50,49,47,46,50,49,49,49.5,52.5,48,42,42,50.5,49,51,55)

Head<-c(58,59,57,60,59,59,58.5,60,55.5,58,58.5,59,60,57,58,56.5,57,58,56,58,58.5,57)

data<-data.frame(Mass,Fore,Bicep,Chest,Neck,Shoulder,Waist,Height,Calf,Thigh,Head)

round(cor(data$Mass,data[2:11]),3)




#a)
# Load the necessary library
if (!require("corrplot")) {
  install.packages("corrplot")
  library(corrplot)
}

# Assuming your data is in a data frame named 'data'
# Select only the columns you're interested in
selected_data <- data[, c("Mass", "Fore", "Bicep", "Chest", "Neck", "Shoulder", "Waist", "Height", "Calf", "Thigh", "Head")]

# Compute the correlation matrix
cor_matrix <- cor(selected_data, use = "complete.obs")  # 'use' argument handles missing values

# Plot the correlation matrix with numbers
corrplot(cor_matrix, method = "circle", addCoef.col = "black")


#c)
model.null<-glm(Mass~1,family=gaussian,data=data)

model.full<-glm(Mass~.,family=gaussian,data=data)

step(model.full,scope=list(model.null,model.full),direction="both",k=log(dim(data)[1]))

m1<-glm(Mass ~ Fore + Waist + Height + Thigh + Head,gaussian)
summary(m1)
anova(m1,test = "F")

m2<-glm(Mass ~ Fore + Waist + Height + Thigh,gaussian)
summary(m2)

BIC(m1,m2)  #m1 better model

#d)
plot(fitted(m1),rstandard(m1)) #good plot

#e)
## Final conclusion : Best model based on BIC criterion is m1 which is a good model provided by residual plots.

#Using ANOVA with F test we see that Head is not statistically significant in the model
#yet it gives the model extra predictive power


