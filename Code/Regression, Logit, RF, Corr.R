library(data.table) #faster way to read large dataset
library(tidyverse) #load dplyr, tidyr and ggplot
library(lubridate) #date manuplation
library(ggrepel) #better label
library(varhandle) #load the function unfactor
library(stringr)
library(dplyr)
library(readr)
library(magrittr)
library(tibble)
library(randomForest)
library(caret)
library(foreign)
library(nnet)
library(stats)
library(e1071)
library(MASS)
library(rpart)
library(pROC)
library(ISLR)
library(miscTools)
library(PerformanceAnalytics)
library(polycor)
library(hmisc)
library(ggcorrplot)
library(corrplot)         
library(standardize)
library(tseries)
library(spread)
library(slp)
library(car)
library(Hmisc)
library(olsrr)
library(GGally)
library(stargazer)

### Read the file
cleanData3 <- read_csv("Downloads/cleanData3.csv")
View(cleanData3)
### Save in native r file
save(anyobjectname_1,anyobjectname_2, file = "crime_la.Rdata")
crime_la <- cleanData3

####Splitting in Training and Testing sets
#select only 2011 to 2016 as training set  and 2017 as test set
train <- filter(crime_la, Date.Occurred >= as.Date("2011-01-01"), Date.Occurred <= as.Date("2016-12-31"))
test <- filter(crime_la, Date.Occurred >= as.Date("2017-01-01"), Date.Occurred <= as.Date("2017-12-31"))


###Obtaining Frequency table to get overall frequency of crime- the DV for Stepwise Regression Model
table <-table(crime_la$`Crime.Cateogry`, crime_la$`zipcode`, crime_la$year)
t1=as.data.frame(table)

##changing names of t2 into crime.category
names(t1)[names(t1)== "Var1"] <-"Crime.Cateogry"
names(t1)[names(t1)== "Var2"] <-"zipcode"
names(t1)[names(t1)== "Var3"] <-"year"
names(t1)[names(t1)== "Freq"] <-"crime"
##merging data frames left join
crime_ols <- merge(crime_la,t1,by=c("zipcode", "year","Crime.Cateogry"))
names(crime_ols)[names(crime_ols)== "Freq"] <-"Crime.Freq"
View(crime_ols)

##save frequency crime data as CSV
write.csv(crime_ols, file = "Crime.Frequency.May5.csv")

################CorrelatGRion Matrix
##Save as data frame
###Data for Correlation plot
corrdata <- crime_ols[,c(21,22,28,33,39,41,42,45)]


###GGPAIRS correlation plot
ggpairs(corrdata)

####Normalizing variables for the regression
crime_<-crime_ols[,c(8,19,21:26, 28,30,31:36,38,39,41:45)]

normalized<-function(y) {
  
  x<-y[!is.na(y)]
  
  x<-(x - min(x)) / (max(x) - min(x))
  
  y[!is.na(y)]<-x
  
  return(y)
}
crime_norm<- apply(crime_[],2,normalized)
crime_norm<-as.data.frame(crime_norm)


################################################## STEPWISE REGRESSION on normalized data
lm<-lm(crime~Victim.Age+ethnicafricanamerican+ethnicasian+ethnicwhite+ethnicPacific+
         ethnicnative+poverty+foreignBorn+medage+mapctbahi+migaborad+migdifcounty+migdifcounty+
         migdifstate+nativeborn+unemp+totpctbahi+medhoval+totmale+unemp, data=crime_norm)

###Lowest model
nullmodel <- lm(crime ~ 1, data=crime_ols)
#Define the largest model
fullmodel<-lm
#Running Stepwise Regression: backwards
stepmodel <- step(fullmodel, scope=list(lower=nullmodel, upper=fullmodel),
                  direction ="backward")
summary(stepmodel)

###AIC for full model and step model
AIC(stepmodel) ##-311889.2
AIC(fullmodel) ##-311887.5


####Estimating RMSE for regression
error1 <- reg$residuals
y.hat <- predict(reg, crime_ols)
error2 <- y.hat - crime_ols$crime

###RMSE
rmse<-function(error)
{
  sqrt(mean(error^2))
}
rmse(error1 
)
##0.1674

####Nice ouput using Stargazer package
reg<- lm(crime ~ ethnicafricanamerican + ethnicasian + ethnicwhite + ethnicPacific + 
           ethnicnative + poverty + foreignBorn + medage + mapctbahi + 
           migaborad + migdifcounty + migdifstate + unemp + totpctbahi + 
           medhoval + totmale, data=crime_norm)


######################################## MULTINOM LOGIT model
logit.crime <-multinom(Crime.Cateogry ~ Area.Name + Victim.Age + Weapon.Type + 
                         month + day + Victim.Ethnicity + poverty + medinc + medhoval + unemp + 
                         totpop +ethnicafricanamerican + engnotwell + foreignBorn + highschool + 
                         mapctbahi, data = train)

summary(logit.crime)

###Cross Validating MultiNom Logit
fitControl <- trainControl(method = "cv", #Cross Validation Methods
                           number = 10, #number of fold
                           repeats = 5, #How many times the process is repeated over the same fold
                           allowParallel = TRUE )

###Running cross validation
gbmFit1 <- train(Crime.Cateogry ~ Area.Name + Victim.Age + Weapon.Type + month + day + Victim.Ethnicity + poverty + medinc + medhoval + unemp + totpop +ethnicafricanamerican + engnotwell + foreignBorn + highschool + mapctbahi,  # Formula
                 data = train, # Dataset
                 method = "multinom",  #name of model techniques
                 trControl = fitControl
)



##Evaluate by using Confusion Matrix
#logit.class <- predict(logit.crime, newdata = train, "class")
logit.class.test <-predict(gbmFit1, newdata = test, "raw")

##training data
#confusionMatrix(logit.class,train$Crime.Cateogry)
##testing data on cross validated Logit
confusionMatrix(logit.class.test,test$Crime.Cateogry)

###Get VarImp
gbmImp <- varImp(gbmFit1, scale = FALSE)
plot(gbmImp, top = 20)

##ROC and AUC for Logit
roc_logit <- roc(as.numeric(test$Crime.Cateogry), as.numeric(logit.class.test))
auc(roc_logit)
##0.5496
plot.roc(roc_logit, col="yellow")




######################################Random Forest on Crime Category
RForest4 <- randomForest(Crime.Cateogry ~ Area.Name + Victim.Age + Weapon.Type + month + day + Victim.Ethnicity + poverty + medinc + medhoval + unemp + totpop +ethnicafricanamerican + engnotwell + foreignBorn + highschool + mapctbahi, data = train, importance = TRUE, ntree= 10)

###Cross Validating Random Forest
fitControl <- trainControl(method = "cv", #Cross Validation Methods
                           number = 5, #number of fold
                           repeats = 2, #How many times the process is repeated over the same fold
                           allowParallel = TRUE )

###Running cross validation
gbmFit <- train(Crime.Cateogry ~ Area.Name + Victim.Age + Weapon.Type + month + day + Victim.Ethnicity + poverty + medinc + medhoval + unemp + totpop +ethnicafricanamerican + engnotwell + foreignBorn + highschool + mapctbahi,  # Formula
                data = train, # Dataset
                method = "rf",  #name of model techniques
                trControl = fitControl
)

RF_pred4 <- predict(gbmFit,train)
RF_predtest <-predict(RForest4, test)
##Confusion matrix RF4
confusionMatrix(RF_pred4,train$Crime.Cateogry)
##accuracy 0.7843 
confusionMatrix(RF_predtest,test$Crime.Cateogry)
#accuracy 0.4615

##Var Imp Plot for RF
varImpPlot(RForest4)

# calculate ROC and AUC for RandomForest4
# roc_obj <- multiclass.roc(category, prediction) RandomForest3
roc_obj2 <- roc(train$Crime.Cateogry, as.numeric(RF_pred4))

auc(roc_obj2)
##0.6223
plot(roc_obj2, col = "purple")
































