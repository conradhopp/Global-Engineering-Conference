rm(list = ls())
dataset <- read.csv("student-mat-sample.csv", header = T)

#Encoding Categorical Data
#------------------------------------------
#encode school codes
dataset$school = as.numeric(factor(dataset$school,
                        levels = c('GP','MS'),
                        labels = c(1,2)))
#encode sex codes
dataset$sex <- as.numeric(factor(dataset$sex,
                      levels = c('M','F'),
                      labels = c(1,2)))

#encode addresses
dataset$address <- as.numeric(factor(dataset$address,
                          levels = c('U','R'),
                          labels = c(1,2)))
#encode family sizes
dataset$famsize <- as.numeric(factor(dataset$famsize,
                          levels = c("LE3",'GT3'),
                          labels = c(1,2)))
#encode parental status
dataset$Pstatus <- as.numeric(factor(dataset$Pstatus,
                          levels = c('T','A'),
                          labels = c(1,2)))
#encode mothers job
dataset$Mjob <- as.numeric(factor(dataset$Mjob,
                       levels = c('at_home','health','services','teacher','other'),
                       labels = c(1,2,3,4,5)))

#encode dads job
dataset$Fjob <- as.numeric(factor(dataset$Fjob,
                       levels = c('at_home','health','services','teacher','other'),
                       labels = c(1,2,3,4,5)))
#encode reason?? what is reason
dataset$reason <- as.numeric(factor(dataset$reason,
                         levels = c('course','other','home','reputation'),
                         labels = c(1,2,3,4)))
#encode guardian
dataset$guardian <- as.numeric(factor(dataset$guardian,
                           levels = c('mother','father','other'),
                           labels = c(1,2,3)))
#encode school support
dataset$schoolsup <- as.numeric(factor(dataset$schoolsup,
                            levels = c('no','yes'),
                            labels = c(1,2)))
#encode parental support
dataset$famsup <- as.numeric(factor(dataset$famsup,
                         levels = c('no','yes'),
                         labels = c(1,2)))
#encode payment status
dataset$paid <- as.numeric(factor(dataset$paid,
                         levels = c('no','yes'),
                         labels = c(1,2)))

#encode payment status
dataset$activities <- as.numeric(factor(dataset$activities,
                       levels = c('no','yes'),
                       labels = c(1,2)))
#encode nursery status
dataset$nursery <- as.numeric(factor(dataset$nursery,
                             levels = c('no','yes'),
                             labels = c(1,2)))
#encode higher status ?? 
dataset$higher <- as.numeric(factor(dataset$higher,
                          levels = c('no','yes'),
                          labels = c(1,2)))
#encode internet status LoL
dataset$internet <- as.numeric(factor(dataset$internet,
                          levels = c('no','yes'),
                          labels = c(1,2)))
#encode romantic status
dataset$romantic <- as.numeric(factor(dataset$romantic,
                          levels = c('no','yes'),
                          labels = c(1,2)))
#--------------------------------------------------------

#Splitting data into training and test set
#--------------------------------------------------------
library(caTools)
set.seed(122)
split <- sample.split(dataset$G3,SplitRatio = 0.80)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)
#--------------------------------------------------------

#feature scaling
training_set <- data.frame(scale(training_set))
test_set <- data.frame(scale(test_set))

#Fitting Multiple linear regression
#--------------------------------------------------------
#building on training set
regressor <- lm(formula = G3 ~ .,
                data = training_set)

#predicting on test set
y_pred <- predict(regressor,
                  newdata = test_set)

#Backwards regression algorithm
#-------------------------------------------------------

regressor <- lm(formula = G3 ~.,
                data = dataset) #use dataset for complete analysis of variables

#remove variables... 
#LEFT OFF HERE#################################
regressor <- lm(formula = G3 ~ famsize +
                  Medu + failures + 
                  romantic + goout,
                data = dataset)
#STEP1 - remove internet (0.701)
#STEP2 - remove Fedu (0.99)
#STEP3 - remove nursery (0.76)
#STEP4 - remove higher (0.56)
#STEP5 - remove Dalc (0.53)
#STEP6 - remove Paid (0.51)
#STEP7 - remove Walc (0.35)
#STEP8 - remove Health (0.38)
#STEP9 - remove famrel (0.41)
#STEP10 - remove guardian (0.259)
#STEP11 - remove traveltime (0.22)
#STEP12 - remove school (0.20)
#STEP13 - remove pstatus (0.32)
#STEP14 - remove freetime (0.17)
#STEP15 - remove age
#STEP16 - remove reason (0.13)
#STEP17 - remove schoolsup (0.099)
#STEP18 - remove address (0.10)

#these variables were the only ones with statistical significance
#--------------------------------------------------------

#Making predictions on new data
#--------------------------------------------------------
#importing new data

newdataset <- read.csv('question_set.csv', header = T)
#Encoding Categorical Data
#--------------------------------------------------------
#encode school codes
newdataset$school = factor(newdataset$school,
                        levels = c('GP','MS'),
                        labels = c(1,2))
#encode sex codes
newdataset$sex <- factor(newdataset$sex,
                      levels = c('M','F'),
                      labels = c(1,2))

#encode addresses
newdataset$address <- factor(newdataset$address,
                          levels = c('U','R'),
                          labels = c(1,2))
#encode family sizes
newdataset$famsize <- as.numeric(factor(newdataset$famsize,
                          levels = c("LE3",'GT3'),
                          labels = c(1,2)))
#encode parental status
newdataset$Pstatus <- factor(newdataset$Pstatus,
                          levels = c('T','A'),
                          labels = c(1,2))
#encode mothers job
newdataset$Mjob <- factor(newdataset$Mjob,
                       levels = c('at_home','health','services','teacher','other'),
                       labels = c(1,2,3,4,5))

#encode dads job
newdataset$Fjob <- factor(newdataset$Fjob,
                       levels = c('at_home','health','services','teacher','other'),
                       labels = c(1,2,3,4,5))
#encode reason?? what is reason
newdataset$reason <- factor(newdataset$reason,
                         levels = c('course','other','home','reputation'),
                         labels = c(1,2,3,4))
#encode guardian
newdataset$guardian <- factor(newdataset$guardian,
                           levels = c('mother','father','other'),
                           labels = c(1,2,3))
#encode school support
newdataset$schoolsup <- factor(newdataset$schoolsup,
                            levels = c('no','yes'),
                            labels = c(1,2))
#encode parental support
newdataset$famsup <- factor(newdataset$famsup,
                         levels = c('no','yes'),
                         labels = c(1,2))
#encode payment status
newdataset$paid <- factor(newdataset$paid,
                       levels = c('no','yes'),
                       labels = c(1,2))

#encode payment status
newdataset$activities <- factor(newdataset$activities,
                             levels = c('no','yes'),
                             labels = c(1,2))
#encode nursery status
newdataset$nursery <- factor(newdataset$nursery,
                          levels = c('no','yes'),
                          labels = c(1,2))
#encode higher status ?? 
newdataset$higher <- factor(newdataset$higher,
                         levels = c('no','yes'),
                         labels = c(1,2))
#encode internet status LoL
newdataset$internet <- factor(newdataset$internet,
                           levels = c('no','yes'),
                           labels = c(1,2))
#encode romantic status
newdataset$romantic <- as.numeric(factor(newdataset$romantic,
                           levels = c('no','yes'),
                           labels = c(1,2)))
#---------------------------------------------------

g3_predict <- predict(regressor,
                      newdata = newdataset)

write.csv(g3_predict,'sample_solution.csv')


