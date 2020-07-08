#Libraries to use
library(readr)
library(lubridate)
library(tidyverse)
library(randomForest)
library(stringr)
library(gtools)
library(dplyr)
library(tree)
library(rpart)
library(rattle)
library(rpart.plot)
library(caret)

#Data Set 
allservicecalls <- read.csv(".../allservicecalls.csv")

#Copy of Data Set
servicecalls <- allservicecalls

#Changing variables in Data Set
servicecalls$OPENEDDATETIME <- as.Date(servicecalls$OPENEDDATETIME, format = "%m/%d/%Y")
servicecalls$SLA_Date <- as.Date(servicecalls$SLA_Date, format = "%m/%d/%Y")
servicecalls$CLOSEDDATETIME <- as.Date(servicecalls$CLOSEDDATETIME,format ="%m/%d/%Y")
servicecalls$OBJECTDESC <- as.character(servicecalls$OBJECTDESC)
servicecalls$Council.District <- as.factor(servicecalls$Council.District)
servicecalls$Report.Starting.Date <- as.Date(servicecalls$Report.Starting.Date, format = "%m/%d/%Y")
servicecalls$Report.Ending.Date <- as.Date(servicecalls$Report.Ending.Date, format = "%m/%d/%Y")

#Only use these variables
servicecalls <- servicecalls[,1:15]

#Find where 1/1/2018 is in the data set
# which(servicecalls$OPENEDDATETIME=="2018-01-01")

#Subset the data set 
servicecalls <- subset(servicecalls, OPENEDDATETIME >= "2018-01-01")

#Creation of the Variable Open Month
servicecalls$OpenMonth <- month(servicecalls$OPENEDDATETIME)
servicecalls$OpenMonth <- as.factor(servicecalls$OpenMonth)
levels(servicecalls$OpenMonth) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov"
                                    , "Dec")
colnames(servicecalls)[6] = "Late"


#Find the top 10 addresses for the data set 
head(sort(table(servicecalls$OBJECTDESC), decreasing = T), n = 10)

#Put theses addresses in a temporary data set 
tempdata <- subset(servicecalls, servicecalls$OBJECTDESC==c("10133  FIGARO CANYON, San Antonio, 78251", 
                                                            "834  BARREL POINT, San Antonio, 78251",
                                                            "838  BARREL POINT, San Antonio, 78251",
                                                            "837  BARREL POINT, San Antonio, 78251",
                                                            "861  BARREL POINT, San Antonio, 78251",
                                                            "939  MARBLE POINT, San Antonio, 78251",
                                                            "928  MARBLE POINT, San Antonio, 78251",
                                                            "814  BARREL POINT, San Antonio, 78251",
                                                            "10129  BOXING PASS, San Antonio, 78251"))


#Subset the data into new app and old app
oldapp <- subset(servicecalls, servicecalls$OPENEDDATETIME<"2018-08-15")
newapp <- subset(servicecalls, servicecalls$OPENEDDATETIME>="2018-08-15")

oldapp <- subset(oldapp, oldapp$SourceID =="311 Mobile App")
newapp <- subset(newapp, newapp$SourceID =="311 Mobile App")

#How many Late/ Not of each in old app and new app
table(oldapp$Late)
table(newapp$Late)

levels(oldapp$Dept)
plot(oldapp$Late.)

#Graphs for old app and new app
ggplot(data = oldapp, aes(x= oldapp$Late)) +
  geom_bar(aes(fill = Dept), position = "dodge")
ggplot(data = newapp, aes(x= newapp$Late)) +
  geom_bar(aes(fill = Dept), position = "dodge")

#New temp data to get when the last time the new app was used, only want certain variables to show
tempdata <- tail(newapp)
tempdata <- tempdata[,-1]
tempdata <- tempdata[,-3]
tempdata <- tempdata[,-3]
tempdata <- tempdata[,-4]
tempdata <- tempdata[,-4]
tempdata <- tempdata[,-4]
tempdata <- tempdata[,1:5]

#Testing data set to make Reason Name and Type names into integers
testing <- servicecalls
#The reason is because Reason Name has 39 unique levels and Type names has 326 Unique levels
unique(testing$REASONNAME)#39
unique(testing$TYPENAME)#326

#gsub function to see how to change name to a number
testing$REASONNAME <- gsub("Field Operations",1, servicecalls$REASONNAME)

#needed to change the variable to a factor
testing$REASONNAME <- as.character.factor(testing$REASONNAME)


#Now real testing to completely change  the names for whole data set
testing2 <- testing

#gives a list of the names of the different unique factors 
list <- as.character(unique(testing2$REASONNAME))
#Need same number of unique factors 
numbers <- 1:39
#Bind them into a data frame for easier list
frame <- cbind.data.frame(numbers, list)
frame$list <- as.character.factor(frame$list)
for (i in numbers) {
  x <- frame[i,2]
  testing2$REASONNAME <- gsub(x,i, testing2$REASONNAME, fixed = T)
}
#unique(testing2$REASONNAME) ; #test to see unique names

#Run again, not an accidental repeat, this helps catch any things like Storm Water and Storm Water Engineering
list <- as.character(unique(testing2$REASONNAME))
numbers <- 1:39

frame <- cbind.data.frame(numbers, list)
frame$list <- as.character.factor(frame$list)
for (i in numbers) {
  x <- frame[i,2]
  testing2$REASONNAME <- gsub(x,i, testing2$REASONNAME, fixed = T)
}
#Final test to see if they are all numbers
unique(testing2$REASONNAME)




#Change TYPENAME to integers
list2 <- as.character(unique(testing2$TYPENAME))
numbers2 <- 1:326
frame2 <- cbind.data.frame(numbers2, list2)
frame2$list2 <- as.character.factor(frame2$list2)

for (i in numbers2) {
  x <- frame2[i,2]
  testing2$TYPENAME <- gsub(x,i, testing2$TYPENAME, fixed = T)
}
unique(testing2$TYPENAME)
list2 <- as.character(unique(testing$TYPENAME))
numbers2 <- 1:326
frame2 <- cbind.data.frame(numbers2, list2)
frame2$list2 <- as.character.factor(frame2$list2)

for (i in numbers2) {
  x <- frame2[i,2]
  testing2$TYPENAME <- gsub(x,i, testing2$TYPENAME, fixed = T)
}
#Final test to see if they are all numbers, may need to run again
unique(testing2$TYPENAME)

#There was only one variable that had not been changed, manually changed to number
testing2$TYPENAME <- gsub("323 - High Priority",325, testing2$TYPENAME)

#Final Final Test 
unique(testing2$TYPENAME)


#If these variables are all numbers then change to intergers 
testing2$REASONNAME <- as.integer(testing2$REASONNAME)
testing2$TYPENAME <- as.integer(testing2$TYPENAME)


#Modeling Data 
modeldata <- testing2

modeldata$REASONNAME <- as.integer(modeldata$REASONNAME)
modeldata$TYPENAME <- as.integer(modeldata$TYPENAME)


#Checking to see if these next two variables NA values were significant 
modeldata$dummyclosed <- ifelse(is.na(servicecalls$CLOSEDDATETIME),1,0)
head(modeldata$dummyclosed)
                      
lmtestclosed <- lm(as.numeric(Late)~dummyclosed, data = modeldata) # significiant
summary(lmtestclosed)

modeldata$dummysla <- ifelse(is.na(modeldata$SLA_Date),1,0)
head(modeldata$dummysla)

lmtestsla <- lm(as.numeric(Late)~dummysla, data = modeldata) #kind of significant
summary(lmtestsla)


#They were significant but decided to remove because they were both Dates and imputation would have skewed data

#Use Model Data to make data to run through models
data <- modeldata[,-2]#remove caseID
data <- data[,-3]#remove sla date
data <- data[,-3]#remove closed date
data <- data[,-9]#remove object description


#Make Train and Test Set Data 
idx.train.calls <- sample(1:nrow(data), size = 0.7 * nrow(data))
train.calls <- data[idx.train.calls,]
test.calls <- data[-idx.train.calls,]

#Linear model
linmodel <- glm(formula = as.numeric(Late)~.-CaseStatus, data = train.calls)

#Summary
summary(linmodel)
#Which coefficients are significant for the linear model
as.matrix(which((summary(linmodel)$coefficients[,4])<=0.05))[,1]
#Which coefficients had the largest estimates in decreasing order
sort((summary(linmodel)$coefficients[,1]),decreasing =T)

#Predict using the linear model
linpredict <- predict(linmodel, newdata = test.calls, type = "response")

#Make the predictions binary 
linpredict <- ifelse(linpredict>=mean(linpredict), "YES", "NO")
#Confusion Matrix for the linear model 
confusionMatrix(as.factor(linpredict), as.factor(test.calls$Late))
#Accuracy : 0.6027

#Set AIC over the Linear model 
stepAIC(linmodel, direction = "both", trace = F)
#Best model based on Step for the linear model
steplinmodel <- glm(formula = as.numeric(Late) ~ (Category + OPENEDDATETIME + 
                                                    Dept + REASONNAME + TYPENAME + CaseStatus + SourceID + Council.District + 
                                                    XCOORD + YCOORD + OpenMonth) - CaseStatus, data = train.calls)
#Summary
summary(steplinmodel)
#Which coefficients are significant for the step linear model
as.matrix(which((summary(steplinmodel)$coefficients[,4])<=0.05))[,1]
#Which coefficients had the largest estimates in decreasing order
sort((summary(steplinmodel)$coefficients[,1]),decreasing =T)
#Predictions for step linear
steplinpredict <- predict(steplinmodel, newdata = test.calls, type = "response")
#Make predictions binary
steplinpredict <- ifelse(steplinpredict>=mean(steplinpredict), "YES", "NO")
#Confusion Matrix for step linear model
confusionMatrix(as.factor(steplinpredict), as.factor(test.calls$Late))
#Accuracy : 0.6027

#Log model
logmodel <- glm(formula = Late~.- CaseStatus, data = train.calls, family = "binomial")
#Summary
summary(logmodel)
#Which coefficients are significant for the log model
as.matrix(which((summary(logmodel)$coefficients[,4])<=0.05))[,1]
#Which coefficients had the largest estimates in decreasing order
sort((summary(logmodel)$coefficients[,1]),decreasing =T)
#Predict using Log model
logpredict <- predict(logmodel, newdata = test.calls, type = "response")
#Make the prediction Binary
logpredict <- ifelse(logpredict>=0.5, "YES", "NO")
#Confusion Matrix for log model
confusionMatrix(as.factor(logpredict), as.factor(test.calls$Late))
#Accuracy : 0.636

#Step AIC model for log model
stepAIC(logmodel, direction = "both", trace = F)
#Best model based off step
steplogmodel <- glm(formula = Late ~ (Category + OPENEDDATETIME + Dept + REASONNAME + 
                                        TYPENAME + CaseStatus + SourceID + Council.District + XCOORD + 
                                        YCOORD + OpenMonth) - CaseStatus, family = "binomial", data = train.calls)

#Which coefficients are significant for the step log model
as.matrix(which((summary(steplogmodel)$coefficients[,4])<=0.05))[,1]
#Which coefficients had the largest estimates in decreasing order
sort((summary(steplogmodel)$coefficients[,1]),decreasing =T)

#Predict using step log model
steplogpredict <- predict(steplogmodel, newdata = test.calls, type = "response")
#Make predictions binary
steplogpredict <- ifelse(steplogpredict>=mean(steplogpredict), "YES", "NO")
#Make confusion matrix over the step log model
confusionMatrix(as.factor(steplogpredict), as.factor(test.calls$Late))
#Accuracy : 0.636

#Take out Casestatus variable
train.calls2 <- train.calls[,-7]
#Make a decision Tree Model
treemodel <- rpart(formula = Late~., data = train.calls2)
#Summary of tree model
summary(treemodel)
#Fancy plot for tree
fancyRpartPlot(treemodel, sub = "", cex=0.5)
#Variable importance plot 
data.frame(importance = treemodel$variable.importance) %>%
  tibble::rownames_to_column(var = "variable") %>%
  ggplot(aes(x = reorder(variable,importance), y = importance)) +
  geom_bar(stat = "identity", fill = "orange", color = "black")+
  coord_flip() +
  labs(x = "Variables", y = "Variable importance")+
  theme_bw()
#Tree model predictions
treepredict <- predict(treemodel, newdata = test.calls, type = "class")
#Confusion Matrix for tree model 
confusionMatrix(as.factor(treepredict), as.factor(test.calls$Late))

