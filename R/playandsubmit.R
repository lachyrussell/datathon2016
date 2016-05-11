#clear all object from memory
rm(list=ls())

#location of data
setwd("C:/Users/lachyrussell/Desktop/Final/")
set.seed(100)

library(data.table)
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(caTools)
library(ggplot2)
library(tm)
library(SnowballC) # for stemming 
library(magrittr)
library(Matrix)
library(RODBC)
library(irlba)
library(plyr)

load(file="readytogo.RData")


xgb <- xgboost(data = combined,
                  label = Y,
                  nround=2000,
                  nthread = 8,
                  gamma= 0,
                  eval_metric = "auc",
                  max.depth = 6,
                  eta =.3,
                  early.round.stop = 3,
                  subsample = 0.8,
                  colsample_bytree =0.8,
                  objective = "binary:logistic")

pred <- predict(xgb, X_test)
print(length(pred))
scoredata$prediction <- pred

#calculate the gini on train rows (these are the ones we know the answers for)
predtrain <- predict(xgb, combined)
print(length(predtrain))
trainingdata$prediction <- predtrain

jobsnew <- rbind(trainingdata,scoredata)
jobsnew$prediction[jobsnew$prediction<=0]<-0
jobsnew$prediction[jobsnew$prediction>=1]<-1


#flag the rows with the words in we want

#myRows1 <- grep(pattern="aborig",x=jobsnew$title,ignore.case = TRUE)
#myRows2 <- grep(pattern="accounts",x=jobsnew$title,ignore.case = TRUE)
#myRows3 <- grep(pattern="architect",x=jobsnew$title,ignore.case = TRUE)
#myRows4 <- grep(pattern="boilermaker",x=jobsnew$title,ignore.case = TRUE)
#myRows5 <- grep(pattern="bookkeeper",x=jobsnew$title,ignore.case = TRUE)
#myRows6 <- grep(pattern="bricklayer",x=jobsnew$title,ignore.case = TRUE)
#myRows7 <- grep(pattern="HR",x=jobsnew$title,ignore.case = FALSE)

#title0 <- read.delim('C:/Users/lachyrussell/Desktop/Final/Variables/title0.csv',header = FALSE,na.strings = "",stringsAsFactors=FALSE)
#title0 <- c(title0$V1)
#myRows8 <- grep(paste(title0,collapse="|"),x=jobsnew$title,ignore.case = TRUE)

#myRows <- Reduce(union,list(myRows1,myRows2,myRows3,myRows4,myRows5,myRows6,myRows7,myRows8))

#changerows1 <- which(jobsnew$prediction <=0.8)
#changerows2 <- which(jobsnew$prediction >=0.1)
#changerows <- intersect(changerows1,changerows2)

#newvalues <- intersect(myRows8,changerows)

#jobsnew[newvalues,'prediction'] <- 1

#myRows9 <- grep(pattern="barista",x=jobsnew$title,ignore.case = TRUE)
#myRows10 <- grep(pattern="bartender",x=jobsnew$title,ignore.case = TRUE)
#myRows11 <- grep(pattern="chef",x=jobsnew$title,ignore.case = TRUE)
#myRows12 <- grep(pattern="cook",x=jobsnew$title,ignore.case = TRUE)
#myRows13 <- grep(pattern="dishwasher",x=jobsnew$title,ignore.case = TRUE)
#myRows14 <- grep(pattern="kitchenhand",x=jobsnew$title,ignore.case = TRUE)
#myRows15 <- grep(pattern="waiter",x=jobsnew$title,ignore.case = TRUE)
#myRows16 <- grep(pattern="waitress",x=jobsnew$title,ignore.case = TRUE)
#myRows17 <- grep(pattern="waitstaff",x=jobsnew$title,ignore.case = TRUE)

#title1 <- read.delim('C:/Users/lachyrussell/Desktop/Final/Variables/title1.csv',header = FALSE,na.strings = "",stringsAsFactors=FALSE)
#title1 <- c(title1$V1)
#myRows18 <- grep(paste(title1,collapse="|"),x=jobsnew$title,ignore.case = TRUE)

#myRows <- Reduce(union,list(myRows9,myRows10,myRows11,myRows12,myRows13,myRows14,myRows15,myRows16,myRows17,myRows18))

#changerows1 <- which(jobsnew$prediction <=0.1)
#changerows2 <- which(jobsnew$prediction >=0.0125)
#changerows <- intersect(changerows1,changerows2)

#newvalues <- intersect(myRows18,changerows)

#jobsnew[newvalues,'prediction'] <- 0

abs0 <- read.delim('C:/Users/lachyrussell/Desktop/Final/Variables/abs0.csv',header = FALSE,na.strings = "",stringsAsFactors=FALSE)
abs0 <- c(abs0$V1)
myRows19 <- grep(paste(abs0,collapse="|"),x=jobsnew$title,ignore.case = FALSE)

changerows1 <- which(jobsnew$prediction <=0.1)
changerows2 <- which(jobsnew$prediction >=0.0125)
changerows <- intersect(changerows1,changerows2)
newvalues <- intersect(myRows19,changerows)
jobsnew[newvalues,'prediction'] <- 0

abs1 <- read.delim('C:/Users/lachyrussell/Desktop/Final/Variables/abs1.csv',header = FALSE,na.strings = "",stringsAsFactors=FALSE)
abs1 <- c(abs1$V1)
myRows20 <- grep(paste(abs1,collapse="|"),x=jobsnew$title,ignore.case = FALSE)

changerows1 <- which(jobsnew$prediction <=0.8)
changerows2 <- which(jobsnew$prediction >=0.1)
changerows <- intersect(changerows1,changerows2)
newvalues <- intersect(myRows20,changerows)
jobsnew[newvalues,'prediction'] <- 1




trainRowsnew <- which(jobsnew$hat != -1)

act <- jobsnew[trainRowsnew,'hat']
pred <- jobsnew[trainRowsnew,'prediction']

AUC <- colAUC(pred,act,plotROC=TRUE)
GINI <- 2 * (AUC - 0.5)
GINI
#expected gini
#0.996

#submit to Kaggle
scoreRowsnew <- which(jobsnew$hat == -1)
submission <- jobsnew[scoreRowsnew,c('job_id','prediction')]
colnames(submission) <- c('job_id','HAT')
myOutputFile <- paste("C:/Users/lachyrussell/Desktop/Datathon/","R_prediction.csv",sep="")
write.csv(submission,myOutputFile,row.names = FALSE,quote=FALSE)

