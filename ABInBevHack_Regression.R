# This is used in conjuncture with arima and tableau to generate inference #### 


#Analysing Drivers####
source_data=ABHck
new_data <- source_data[ which(source_data$Week >156),]
source_data=new_data


ab_data=source_data[which(source_data$`Brewer Value`=='ABI'),]

attach(source_data)

#data Preparation is done in Excel (replacing NA values with mean 0f columns of macro econmic variables, 0 in promotional activity like display share, feauture count etc) )  )

#Performing Multiple  Regression 

model_2_linear_price=lm(ab_data$`Unit Sales`~.,data=ab_data)
summary(model_2_linear_price)
View(ab_data)
ab_data <- subset( ab_data, select = -c(1))
anova(model_1_linear_price)

# Removing colums not reqd

source_data <- subset( source_data, select = -c( 10,11,12,16,18,20,21,23,24,25,26,27,28,29,30,31,32,33,34,35,39,43,50,51) )
source_data <- subset( source_data, select = -c(1))
source_data <- subset( source_data, select = -c(5,8,15,16,22,25,27,29,30))
source_data <- subset( source_data, select = -c(5))
source_data <- subset( source_data, select = -c(12,13,15,16,17,18))
source_data <- subset( source_data, select = -c(12,13)   )   
source_data <- subset( source_data, select = -c(4))
source_data <- subset( source_data, select = -c(6))                       

View(source_data)

## anova(m1,m2) ## We tried out comparing models with interaction effects.


#performing General linear model 

model_2_glm=glm(model_mixed2_data$`Unit Sales`~., family = "gaussian", data=model_mixed2_data)
summary(model_2_glm)
# Analysis of deviance
anova(model_2_glm ,test="Chisq")

# McFadden R^2
library(pscl)
pR2(model_2_glm)



#### Forwward ######


fullmod<-glm(source_data$`Unit Sales` ~.,family=gaussian(link='identity'),data=source_data)
nothing<-glm(source_data$`Unit Sales` ~1,family=gaussian(link='identity'),data=source_data)

forwards = step(nothing, scope=list(lower=formula(nothing),upper=formula(fullmod)),nvmax=14, direction="forward")
summary(forwards)





##### Glmmulti 
library(leaps)





##### Combined Model Linear #######  Final 

data=ABHck
rm(new_data)
new_data <- data[ which(data$Week >156),]
data=new_data
data[is.na(data)]=0
View(data)
data <- subset( data, select = -c( 1,5,6,7,10,1,14,15,18,20,21,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,47,49,50,52,54,55))
model_mixed=lm(data$`Unit Sales`~.,data=data)
model_mixed2_data=subset( model_mixed2_data, select = -c(6))
rm(model_mixed2)
model_mixed2=lm(model_mixed2_data$`Unit Sales`~.,data=model_mixed2_data)
summary(model_mixed2)
View(model_mixed2_data)


