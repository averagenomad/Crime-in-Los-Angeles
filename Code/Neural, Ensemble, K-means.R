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
library(caretEnsemble)
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
library(Hmisc)
library(corrplot)
library(ggmap)
library(purrr)


# set the "Path" variable to the path where the data set file is on your computer!!!!

Path <- 'C:/Users/mskim/OneDrive/Email attachments/Desktop/titanicsunk4.csv'
crime_la <- read_csv(Path)
as_tibble(crime_la)     # change into a tibble 

# remove spaces in feature names
names(crime_la) <-str_replace_all(names(crime_la), c(" " = "."))
glimpse(crime_la)

#the feature Weapon.Description has 140 levels. create bins to reduce levels 

Guns <- c('AIR PISTOL/REVOLVER/RIFLE/BB GUN','ASSAULT WEAPON/UZI/AK47/ETC',' HECKLER & KOCH 91 SEMIAUTOMATIC ASSAULT RIFLE','M1-1 SEMIAUTOMATIC ASSAULT RIFLE','MAC-11 SEMIAUTOMATIC ASSAULT WEAPON',
          'REVOLVER','SAWED OFF RIFLE/SHOTGUN','SEMI-AUTOMATIC PISTOL','SHOTGUN','STARTER PISTOL/REVOLVER','STUN GUN','UNK TYPE SEMIAUTOMATIC ASSAULT RIFLE','UZI SEMIAUTOMATIC ASSAULT RIFLE','ANTIQUE FIREARM',
          'AUTOMATIC WEAPON/SUB-MACHINE GUN','HAND GUN','HECKLER & KOCH 93 SEMIAUTOMATIC ASSAULT RIFLE', 'M-14 SEMIAUTOMATIC ASSAULT RIFLE','MAC-10 SEMIAUTOMATIC ASSAULT WEAPON','RIFLE','SEMI-AUTOMATIC RIFLE','SIMULATED GUN',
          'UNKNOWN FIREARM')
Sharp_Object <- c('AXE','BOWIE KNIFE','FOLDING KNIFE','ICE PICK','KNIFE WITH BLADE 6INCHES OR LESS','MACHETE','OTHER KNIFE','OTHER CUTTING INSTRUMENT','RAZOR BLADE','SCISSORS','STRAIGHT RAZOR','SWORD','BOW AND ARROW',
                  'CLEAVER','DIRK/DAGGER','GLASS','KITCHEN KNIFE','KNIFE WITH BLADE OVER 6 INCHES IN LENGTH','RAZOR','SCREWDRIVER','SWITCH BLADE','SYRINGE')
Blunt_Object <- c('BOARD','BOTTLE','CLUB/BAT','HAMMER','PIPE/METAL PIPE','ROCK/THROWN OBJECT','TIRE IRON','BLUNT INSTRUMENT','BRASS KNUCKLES','CONCRETE BLOCK/BRICK','FIXED OBJECT')
Strong_Arm <- c("STRONG-ARM (HANDS, FIST, FEET OR BODILY FORCE)")
Intimidation <- c("VERBAL THREAT",'BOMB THREAT', 'PHYSICAL PRESENCE')

# create new feature, called Weapon.Type to store new factor 
crime_la$Weapon.Type = ''



# use indexing to assign weapons.type
crime_la$Weapon.Type[which(crime_la$Weapon.Description %in% Guns)] <- "Gun"
crime_la$Weapon.Type[which(crime_la$Weapon.Description %in% Sharp_Object)] <- 'Sharp Object'
crime_la$Weapon.Type[which(crime_la$Weapon.Description %in% Blunt_Object)] <- 'Blunt Object'
crime_la$Weapon.Type[which(crime_la$Weapon.Description %in% Strong_Arm)]  <- 'Strong-Arm'
crime_la$Weapon.Type[which(crime_la$Weapon.Description %in% Intimidation)] <- 'Intimidation'
crime_la$Weapon.Type[which(crime_la$Weapon.Type == '')] <- "Other" 



Against_nature <- c('CRUELTY TO ANIMALS')
Against_public_order <- c('ABORTION/ILLEGAL', 'BIKE - ATTEMPTED STOLEN', 'BIKE - STOLEN', 'BOMB SCARE', 'CONTEMPT OF COURT', 'CONTRIBUTING', 'DISRUPT SCHOOL', 'DISTURBING THE PEACE', 'DRUGS, TO A MINOR', 'DRUNK ROLL', 'FAILURE TO DISPERSE', 'FAILURE TO YIELD', 'FALSE IMPRISONMENT', 'FALSE POLICE REPORT', 'INCITING A RIOT', 'INDECENT EXPOSURE', 'LETTERS, LEWD  -  TELEPHONE CALLS, LEWD', 'LEWD CONDUCT', 'LEWD/LASCIVIOUS ACTS WITH CHILD', 'PIMPING', 'PROWLER', 'RECKLESS DRIVING', 'RESISTING ARREST', 'TELEPHONE PROPERTY - DAMAGE', 'THEFT-GRAND ($950.01 & OVER)EXCPT,GUNS,FOWL,LIVESTK,PROD', 'TRAIN WRECKING', 'TRESPASSING', 'VANDALISM - FELONY ($400 & OVER, ALL CHURCH VANDALISMS)', 'VANDALISM - MISDEAMEANOR ($399 OR UNDER)', 'VIOLATION OF COURT ORDER', 'VIOLATION OF RESTRAINING ORDER', 'VIOLATION OF TEMPORARY RESTRAINING ORDER', 'WEAPONS POSSESSION/BOMBING')
Causing_death <- c('ASSAULT WITH DEADLY WEAPON ON POLICE OFFICER', 'ASSAULT WITH DEADLY WEAPON, AGGRAVATED ASSAULT', 'CHILD ABUSE (PHYSICAL) - AGGRAVATED ASSAULT', 'CHILD STEALING', 'CRIMINAL HOMICIDE', 'CRIMINAL THREATS - NO WEAPON DISPLAYED', 'CRM AGNST CHLD (13 OR UNDER) (14-15 & SUSP 10 YRS OLDER)', 'HUMAN TRAFFICKING - INVOLUNTARY SERVITUDE', 'ILLEGAL DUMPING', 'MANSLAUGHTER, NEGLIGENT', 'SHOTS FIRED AT INHABITED DWELLING', 'SHOTS FIRED AT MOVING VEHICLE, TRAIN OR AIRCRAFT', 'THROWING OBJECT AT MOVING VEHICLE') 
Causing_bodily_harm <- c('BATTERY - SIMPLE ASSAULT', 'BATTERY ON A FIREFIGHTER', 'BATTERY POLICE (SIMPLE)', 'BRANDISH WEAPON', 'CHILD ABUSE (PHYSICAL) - SIMPLE ASSAULT', 'CHILD ANNOYING (17YRS & UNDER)', 'CHILD NEGLECT (SEE 300 W.I.C.)', 'DISCHARGE FIREARMS/SHOTS FIRED', 'KIDNAPPING', 'KIDNAPPING - GRAND ATTEMPT', 'LYNCHING', 'LYNCHING - ATTEMPTED', 'PANDERING', 'STALKING', 'THREATENING PHONE CALLS/LETTERS')
Fraud_deception_corruption <- c('BRIBERY', 'BUNCO, ATTEMPT', 'BUNCO, GRAND THEFT', 'BUNCO, PETTY THEFT', 'CONSPIRACY', 'COUNTERFEIT', 'CREDIT CARDS, FRAUD USE ($950.01 & OVER)', 'DEFRAUDING INNKEEPER/THEFT OF SERVICES, $400 & UNDER', 'DEFRAUDING INNKEEPER/THEFT OF SERVICES, OVER $400', 'DOCUMENT FORGERY / STOLEN FELONY', 'DOCUMENT WORTHLESS ($200 & UNDER)', 'DOCUMENT WORTHLESS ($200.01 & OVER)', 'REPLICA FIREARMS(SALE,DISPLAY,MANUFACTURE OR DISTRIBUTE)', 'THEFT OF IDENTITY', 'TILL TAP - PETTY ($950 & UNDER)', 'UNAUTHORIZED COMPUTER ACCESS')
Involving_property_and_human <- c('DRIVING WITHOUT OWNER CONSENT (DWOC)', 'EXTORTION', 'GRAND THEFT / INSURANCE FRAUD', 'THEFT FROM PERSON - ATTEMPT', 'THEFT, PERSON')
Involving_property_only <- c('ARSON', 'ATTEMPTED ROBBERY', 'BURGLARY', 'BURGLARY FROM VEHICLE', 'BURGLARY FROM VEHICLE, ATTEMPTED', 'BURGLARY, ATTEMPTED', 'DISHONEST EMPLOYEE - GRAND THEFT', 'DISHONEST EMPLOYEE - PETTY THEFT', 'EMBEZZLEMENT, GRAND THEFT ($950.01 & OVER)', 'EMBEZZLEMENT, PETTY THEFT ($950 & UNDER)', 'PETTY THEFT - AUTO REPAIR', 'PICKPOCKET', 'PICKPOCKET, ATTEMPT', 'PURSE SNATCHING', 'PURSE SNATCHING - ATTEMPT', 'ROBBERY', 'SHOPLIFTING - ATTEMPT', 'SHOPLIFTING - PETTY THEFT ($950 & UNDER)', 'SHOPLIFTING-GRAND THEFT ($950.01 & OVER)', 'THEFT FROM MOTOR VEHICLE - ATTEMPT', 'THEFT FROM MOTOR VEHICLE - GRAND ($400 AND OVER)', 'THEFT FROM MOTOR VEHICLE - PETTY ($950 & UNDER)', 'THEFT PLAIN - ATTEMPT', 'THEFT PLAIN - PETTY ($950 & UNDER)', 'THEFT, COIN MACHINE - ATTEMPT', 'THEFT, COIN MACHINE - GRAND ($950.01 & OVER)', 'THEFT, COIN MACHINE - PETTY ($950 & UNDER)', 'VEHICLE - ATTEMPT STOLEN', 'VEHICLE - STOLEN')
Other <- c('CHILD ABANDONMENT', 'OTHER ASSAULT', 'OTHER MISCELLANEOUS CRIME')
Sex_Crimes <- c('BATTERY WITH SEXUAL CONTACT', 'BEASTIALITY, CRIME AGAINST NATURE SEXUAL ASSLT WITH ANIM', 'CHILD PORNOGRAPHY', 'HUMAN TRAFFICKING - COMMERCIAL SEX ACTS', 'INCEST (SEXUAL ACTS BETWEEN BLOOD RELATIVES)', 'INTIMATE PARTNER - AGGRAVATED ASSAULT', 'INTIMATE PARTNER - SIMPLE ASSAULT', 'ORAL COPULATION', 'PEEPING TOM', 'RAPE, ATTEMPTED', 'RAPE, FORCIBLE', 'SEX,UNLAWFUL(INC MUTUAL CONSENT, PENETRATION W/ FRGN OBJ', 'SEXUAL PENETRATION W/FOREIGN OBJECT', 'SODOMY/SEXUAL CONTACT B/W PENIS OF ONE PERS TO ANUS OTH')


crime_la$Crime.Category <- ''
crime_la$Crime.Cateogry[which(crime_la$Crime.Code.Description %in%  Against_nature  )] <- 'Against Nature'  
crime_la$Crime.Cateogry[which(crime_la$Crime.Code.Description %in%  Against_public_order  )] <- 'Against public order' 
crime_la$Crime.Cateogry[which(crime_la$Crime.Code.Description %in%  Causing_death  )] <- 'Causing Death' 
crime_la$Crime.Cateogry[which(crime_la$Crime.Code.Description %in%  Causing_bodily_harm  )] <- 'Causing bodily harm' 
crime_la$Crime.Cateogry[which(crime_la$Crime.Code.Description %in%  Fraud_deception_corruption  )] <- 'Fraud, Deception & Corruption' 
crime_la$Crime.Cateogry[which(crime_la$Crime.Code.Description %in%  Involving_property_and_human  )] <- 'Involving property and human' 
crime_la$Crime.Cateogry[which(crime_la$Crime.Code.Description %in%  Involving_property_only  )] <- 'Involving propertly only ' 
crime_la$Crime.Cateogry[which(crime_la$Crime.Code.Description %in%  Other  )] <- 'Other ' 
crime_la$Crime.Cateogry[which(crime_la$Crime.Code.Description %in%  Sex_Crimes  )] <- 'Sex Crimes ' 


# change Time to factor with 24 time periods/categories     
# 'hour' change to char type,  substr on '03' first 2 chars.  transform 1130 => 1100  -> 
crime_la$Hour <- as.character(crime_la$Time.Occurred)     
#substr(x, start, stop)
crime_la$Hour <-  paste( substr(crime_la$Hour, 1, 2),"00", sep='' )



#convert the date into date type
crime_la_selected$Date.Occurred <- mdy(crime_la_selected$Date.Occurred) 

location <- crime_la_selected$Location %>% # take coord as string
  str_replace_all("[()]", "") %>% # replace parantheses
  str_split_fixed(", ", n=2) %>% # split up based on comma and space after
  as_tibble %>% # turn this to a data frame
  transmute(lat=V1, long=V2) # rename the variables 

location$lat <- as.numeric(location$lat)
location$long <- as.numeric(location$long)

#combine the lat and long then remove the location
crime_la_selected <- cbind(crime_la_selected, location)
crime_la_selected <- select(crime_la_selected, -Location)

# assign crime location to a zip code
crime_la$ZipCode <- ''

#separate date into year, month and day.
crime_la_selected$year <- as.factor((year(crime_la_selected$Date.Occurred)))
crime_la_selected$month <- as.factor(month(crime_la_selected$Date.Occurred))
crime_la_selected$day <- as.factor(day(crime_la_selected$Date.Occurred))
crime_la_selected$Time.Occurred <- as.numeric(crime_la_selected$Time.Occurred)


#Recode the variable into readable format
crime_la_selected$Victim.Sex <- recode(crime_la_selected$Victim.Sex, 'F' = 'Female', 'M' = 'Male', 'X' = 'Unknown')

crime_la_selected$Victim.Ethnicity <- recode(crime_la_selected$Victim.Descent, "A" = "Other Asian", "B" = "Black", "C" = "Chinese", "D" = "Cambodian", "F" = "Filipino", "G" = "Guamanian", "H" = "Hispanci/Latin/Mexican", 'I' = "American Indian/Alaskan Native", "J" = "Japanese", "K" = "Korean", "L" = "Laotian", "O" = "Other", "P" = "Pacific Islander", "S" = "Somoan", "U" = "Hawaiian", "V" = "Vietnamese", "W" = "White", "X" = "Unknown", "Z" = "Asian Indian")

#convert the character into factor
character_vars <- lapply(crime_la_selected, class) == "character"
crime_la_selected[, character_vars] <- lapply(crime_la_selected[, character_vars], as.factor)

crime_la$weapon.Type <- NULL

#remove NAs 
titanic <- crime_la_selected[complete.cases(crime_la_selected),]   #remove ALL rows with any NA in 1 category


#write back to cSV file
write_csv(crime_la,"C:/Users/mskim/OneDrive/Email attachments/Desktop/titanic2.csv")
# clean data row by row


#select only 2010 to 2017 as training set  and 2018 as test set
crime_2010to2017 <- filter(crime_la_selected, Date.Occurred >= as.Date("2010-01-01"), Date.Occurred <= as.Date("2017-12-30"))
crime_2018 <- filter(crime_la_selected, Date.Occurred >= as.Date("2018-01-01"), Date.Occurred <= as.Date("2018-12-30"))


# set the "Path" variable to the path where the data set file is on your computer!!!!

#Path <- 'C:/Users/mskim/OneDrive/Email attachments/Desktop/cleanData3.csv'
crime_la <- cleanData3
#crime_la$Date.Occurred <- as.Date(crime_la$Date.Occurred, format = "%m/%d/%Y")


# R reads int factor data as Char data automatically even if you save factors to .csv
#so we must use as.factor() after we read in the .csv file each time
crime_la$Area.Name <- as.factor(crime_la$Area.Name)
crime_la$Victim.Sex <- as.factor(crime_la$Victim.Sex)
crime_la$Weapon.Type <- as.factor(crime_la$Weapon.Type)
crime_la$Victim.Ethnicity <- as.factor(crime_la$Victim.Ethnicity)
crime_la$Crime.Cateogry <- as.factor(crime_la$Crime.Cateogry)
as_tibble(crime_la)     # change into a tibble 


#EDA Exploratory Data Analysis
type <- crime_la %>%
  filter(year == "2018")

# Chart of Top 10 crimes commited 
group <- crime_la %>%
  group_by(Crime.Code.Description) %>%
  summarise(total = n()) %>%
  distinct() %>%
  top_n(10)

group %>%
  ggplot(aes(reorder(Crime.Code.Description, total), y = total)) +
  geom_col(fill = "red") +
  geom_label_repel(aes(label = total), size = 2.5) +
  coord_flip() +
  labs(title = "Top 10 Crimes Commited from 2010-2017", 
       x = "Crime Description", 
       y = "Total")


#Exploratory Data Analysis
year_2018 <- crime_2018 %>%
  filter(year == "2018")

# Chart of Top 10 crimes commited 
group <- year_2018 %>%
  group_by(Crime.Code.Description) %>%
  summarise(total = n()) %>%
  distinct() %>%
  top_n(10)

group %>%
  ggplot(aes(reorder(Crime.Code.Description, total), y = total)) +
  geom_col(fill = "red") +
  geom_label_repel(aes(label = total), size = 2.5) +
  coord_flip() +
  labs(title = "Top 10 Crimes Commited in 2018", 
       x = "Crime Description", 
       y = "Total")

# AIzaSyDk529u2Sv1AUQpudmpHsq09PIMJ7TjNR4
register_google(key = X, write = TRUE)


#select relevant variables
crime.location <- year_2018 %>%
  select(Crime.Code.Description, long, lat) %>%
  filter(Crime.Code.Description == 'BATTERY - SIMPLE ASSAULT') %>%
  na.omit()

#get the map of LA using Google maps


p <- ggmap(get_googlemap(center = "Los Angeles",
                         zoom = 12, scale = 2,
                         maptype ='terrain',
                         color = 'color'))

p + geom_point(data = crime.location, aes( x = long, y = lat,  size = 0.01, alpha = .5 )) 


#mapping

qmplot(long, lat, data = crime.location, maptype = "toner-lite", color = I("red"))

qmplot(long, lat, data = crime.location, maptype = "toner-lite", geom = "density2d", color = I("red"))

qmplot(long, lat, data = crime.location, geom = "blank", 
       zoom = 11, maptype = "toner-background", darken = .7, legend = "topleft" ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Battery\nPropensity", low = "white", mid = "yellow", high = "red" )



# Cluster Analysis K-means
# analyze crimes involving guns
Guns <- select(filter(crime_la, Weapon.Type == 'Gun'), lat, long, Weapon.Type)
clusters <- kmeans(Guns[,1:2], 10)
Guns$Area <- as.factor(clusters$cluster)

LAMap <- get_map("Los Angeles", zoom = 10)
ggmap(LAMap) + geom_point(aes(x=long[], y = lat[], colour = as.factor(Area)), data = Guns) +
  ggtitle("Los Angeles Gun Crimes")


# cluster Analysis K-means
# analyze type of crime committed
Deaths <- select(filter(crime_la, Crime.Cateogry == 'Causing Death'), lat, long, Crime.Cateogry)
clusters2 <- kmeans(Deaths[,1:2],10)
Deaths$Crime <- as.factor(clusters2$cluster)

LAMap2 <- get_map("Los Angeles", zoom = 10)
ggmap(LAMap2) + geom_point(aes(x=long[], y = lat[], colour = as.factor(Crime)), data = Deaths) +
  ggtitle("Los Angeles Crimes causing Death")


###################neural network to predict Crime.Type
require('neuralnet')

trainN <- dplyr::select(train, Victim.Sex,Victim.Age,poverty,foreignBorn,medage,mapctbahi,migaborad,migdifcounty,migdifcounty,migdifstate,nativeborn,unemp,totpctbahi,medhoval,medinc,totmale,unemp)

index <- sample(1:nrow(trainN), 10000)
trainNN <- train[index,]
testNN <- test[1:1000,]
# fit neural network



nn = neuralnet(Victim.Sex ~ Victim.Age+poverty+foreignBorn+medage+mapctbahi+migaborad+migdifcounty+migdifcounty+migdifstate+nativeborn+unemp+totpctbahi+medhoval+medinc+totmale+
  unemp, data = trainNN, hidden= 3, act.fct = "logistic", linear.output = FALSE)

plot(nn)



Predict=compute(nn,testNN)
Predict$net.result

prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)
pred

#confusion matrix
confusionMatrix(pred,testNN$Victim.Sex)

# calculate ROC and AUC
# roc_obj <- roc(category, prediction)
roc_obj_Test <- roc(as,numeric(testNN$Victim.Sex), pred)

auc(roc_obj_Test)



## neural net using nnet

library(nnet)
gender.nn <- nnet(Victim.Sex ~ Victim.Age+poverty+foreignBorn+medage+mapctbahi+migaborad+migdifcounty+migdifcounty+migdifstate+nativeborn+unemp+totpctbahi+medhoval+medinc+totmale+
                    unemp, data = trainNN, size=10, linout=F, maxit=1000, Hess=T)

gender.nn2 <- nnet(Victim.Sex ~ Victim.Age+poverty+foreignBorn+medage+unemp, data = trainNN, size=10, linout=F, maxit=1000, Hess=T)
plot.nnet(gender.nn)


table(predict(gender.nn, trainNN, type="class"), trainNN$Victim.Sex)



causingDeathSex <-  select(causingDeathCategory, Victim.Sex,Victim.Age,poverty,foreignBorn,medage,mapctbahi,migaborad,migdifcounty,migdifcounty,migdifstate,nativeborn,unemp,totpctbahi,medhoval,medinc,totmale,unemp)

#write back to cSV file:  NOTE factor types will change back to Char types after the write operation
#write_csv(crime_la,"C:/Users/mskim/OneDrive/Email attachments/Desktop/cleanData3.csv")

## alternate method to save/preserve a DF's structure is to save the file as an .R file instead of .csv


causingDeathSex <- select(causingDeathCategory, Victim.Sex,Victim.Age,poverty,foreignBorn,medage,mapctbahi,migaborad,migdifcounty,migdifcounty,migdifstate,nativeborn,unemp,totpctbahi,medhoval,medinc,totmale,unemp)



#################### ensemble model

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, savePredictions = TRUE,
                        summaryFunction = twoClassSummary, classProbs = TRUE, verboseIter = TRUE)


# List of algorithms to use in ensemble
algorithmList <- c('rpart','glm','knn','svmRadial')

models <- caretList(Victim.Sex ~ . - Victim.Sex, data = causingDeathCategory5k, trControl = control, methodList = algorithmList, metric = "ROC")

# Results
res <- resamples(models)
summary(res)
dotplot(res)

modelCor(res)
splom(res)

greedy_ensemble <- caretEnsemble(models, 
  metric="ROC",
  trControl=trainControl(
    number= 10,
    summaryFunction=twoClassSummary,
   classProbs=TRUE
  ))
summary(greedy_ensemble)

sink("ensembleModel")
summary(res)
dotplot(res)
modelCor(res)
summary(greedy_ensemble)
sink()
