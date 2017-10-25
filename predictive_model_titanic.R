#loading datasets
test <-read.csv("test.csv")
train <- read.csv("train.csv")

#adding a required column
test.survived <- data.frame(Survived=rep("None", nrow(test)),test[,])
#merging data frames
combined.data <- rbind(train,test.survived)
#type conversions
combined.data$Pclass <-as.factor(combined.data$Pclass)
combined.data$Survived <- as.factor(combined.data$Survived)

#have a look to the survived dataa column
table(combined.data$Survived)

train$Pclass <- as.factor(train$Pclass)

#to check how many unique names are there in the given data frame
length(unique(as.character(combined.data$Name)))

#check if any duplicate names are there
dub.names <-as.character(combined.data[which(duplicated(as.character(combined.data))),"Name"])
dub.names

combined.data[which(combined.data$Name %in% dub.names),]


library("stringr")

#create a utility function to extract titles from name field

extractTitle <- function(Name){
  Name <-as.character(Name)
  if(length(grep("Miss.",Name)) > 0){
    return("Miss.")
  }
  else if(length(grep("Mrs.",Name)) > 0){
    return("Mrs.")
  }
  else if(length(grep("Mr.",Name)) > 0){
    return("Mr.")
  }
  else if(length(grep("Master.",Name)) > 0){
    return("Master.")
  }
  else{
    return("Others")
  }
}


titles <- NULL
 for(i in 1:nrow(combined.data)){
   titles <- c(titles,extractTitle(combined.data[i,"Name"]))
 }

combined.data$title <- as.factor(titles)




table(combined.data$Sex)
table(combined.data$Survived)

summary(combined.data$Age)
 boy <-combined.data[which(combined.data$title == "Master."),]
 summary(boy$Age)
 miss <-combined.data[which(combined.data$title == "Miss."),]
 Mr <-combined.data[which(combined.data$title == "Mr."),]
 summary(Mr$Age)
 Mrs <-combined.data[which(combined.data$title =="Mrs."),]
 Mrs
 miss.alone <-miss[which(miss$SibSp == 0 & miss$Parch ==0),]
 summary(miss.alone$Age)
 mr.alone <- Mr[which(Mr$SibSp ==0 & Mr$Parch==0),]
 summary(mr.alone$Age)
 
 temp.sibsp <- c(test$SibSp,train$SibSp)
 temp.parch <- c(test$Parch,train$Parch)
 
 combined.data$family.size <- as.factor(temp.parch + temp.sibsp +1)
median(Mr$Age, na.rm = TRUE)

# to check how many null values are there in perticular column
length(Mr$Age[is.na(Mr$Age)])

#to find the median of a set of number by removing NAs values
median(miss$Age,na.rm = TRUE)

#assigning the median value to all the NA values
miss$Age[is.na(miss$Age)] <- c(22)

Mrs$Age[is.na(Mrs$Age)]
length(Mrs$Age)

new.test.set <-rbind(new.test.set,Mrs)

#filtering data by Name titles
Man <- combined.data[which(combined.data$Sex == "male" & combined.data$title =="Mr."),]
boy <- combined.data[which(combined.data$Sex == "male" & combined.data$title== "Master."),]
miss <- combined.data[which(combined.data$Sex == "female" & combined.data$title == "Miss."),]
Mrs <- combined.data[which(combined.data$Sex == "female" & combined.data$title== "Mrs."),]
others <- combined.data[which(combined.data$title == "Others"),]



# boy missing values fill
boy$Age[is.na(boy$Age)] <- 4

#man missing values fill
Man$Age[is.na(Man$Age)] <-29

#women missing values fill
Mrs$Age[is.na(Mrs$Age)] <- 36

#miss missing values fill 
miss$Age[is.na(miss$Age)] <- median(miss$Age,na.rm = TRUE)

#others missing values fill
others$Age[is.na(others$Age)] <- 44

new.set.full <-rbind(boy,Man)
new.set.full <-rbind(new.set.full,miss)
new.set.full <-rbind(new.set.full,Mrs)
new.set.full <- rbind(new.set.full,others)

new.test <-new.set.full[which(new.set.full$Survived == "None"),]
new.train <- new.set.full[which(!new.set.full$Survived == "None"),]

new.test$Fare[is.na(new.test$Fare)] <- 14.4542

new.test$Survived[which(new.test$Survived == "None")] <- NA
#now building the actual predictive model


#loading the randomForest package
new.train$Survived <- droplevels(new.train$Survived)
library("randomForest")
survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)

 titanic.model<- randomForest(formula = survived.formula, data = new.train, ntree= 500, mtry =3 , nodesize = 0.01*nrow(new.test))
features.equation <- " Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived <- predict(titanic.model,newdata = new.test)
passengerid <-new.test$PassengerId
survived
output.df <- as.data.frame(passengerid)
output.df$survived <- survived
