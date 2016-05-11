
###########################################
# datathon 2016
# Kaggle Comp
###########################################

#clear all object from memory
rm(list=ls())

#location of data
setwd("C:/Users/lachyrussell/Desktop/Final/")
set.seed(100)

#Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
#install.packages(Needed, dependencies=TRUE)   
#install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")  
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
##SQL Server Linked see documentation.
#https://andersspur.wordpress.com/2013/11/26/connect-r-to-sql-server-2012-and-14/

#read in the data from SQL database


odbcChannel <- odbcConnect("datathon2016")
jobs <- sqlFetch(odbcChannel, "joinfeatures")
#jobs <- sqlFetch(odbcChannel, "joinfeaturessmall")
#can also remotely write queries: sqlQuery(odbcChannel, "SELECT.......... QUERY")
#odbcClose(odbcChannel)

#-----------------------------
#Kaggle submission
#-----------------------------

#initialise prediction
jobs$prediction <- 0
jobsprecleaning <- jobs
jobs <- jobsprecleaning
#Cleaning
#----------
#Clean bad salary data
jobs$salary_min <- ifelse(is.na(jobs$salary_min), mean(jobs$salary_min,na.rm = TRUE), jobs$salary_min)
jobs$salary_max <- ifelse(is.na(jobs$salary_max), mean(jobs$salary_max,na.rm = TRUE), jobs$salary_max)
jobs$salary_min <- ifelse(jobs$salary_min <= 1/10*jobs$salary_max, -1, jobs$salary_min)
jobs$salary_min <- ifelse(jobs$salary_min >= 10*jobs$salary_max, -1, jobs$salary_min)
jobs$salary_min <- ifelse(jobs$salary_min >= jobs$salary_max, -1, jobs$salary_min)
cleanedRows <- which(jobs$salary_min != -1 | jobs$hat == -1)

jobs=jobs[cleanedRows,]
jobs$salary_min <- ifelse(jobs$salary_min >= jobs$salary_max, -1, jobs$salary_min)
jobs$salary_max = jobs$salary_max/max(jobs$salary_max, na.rm = TRUE)
jobs$salary_max = round(jobs$salary_max, digits = 2)
jobs$salary_min = jobs$salary_min/max(jobs$salary_min, na.rm = TRUE)
jobs$salary_min = round(jobs$salary_min, digits = 2)

#Clean mobile data
jobs$avg_mobile_user <- ifelse(is.na(jobs$avg_mobile_user), mean(jobs$avg_mobile_user[jobs$avg_mobile_user >= 0],na.rm=TRUE), jobs$avg_mobile_user)
jobs$avg_mobile_user = round(jobs$avg_mobile_user, digits = 2)
#train and score rows
trainRows <- which(jobs$hat != -1)
scoreRows <- which(jobs$hat == -1)
trainingdata=jobs[trainRows,]
scoredata=jobs[scoreRows,]


#Some Feature Creation
#-----------------------------------------
#create a feature of the salary type
st <- data.frame(table(jobs$salary_type))
st <-st[order(st$Freq,decreasing=TRUE),]
myRows89 <- grep(pattern="y",x=jobs$salary_type,ignore.case = TRUE)
myRows88 <- grep(pattern="h",x=jobs$salary_type,ignore.case = TRUE)
jobs$y <- 0
jobs$h <- 0
jobs[myRows89,'y'] <- 1
jobs[myRows88,'h'] <- 1


rm(odbcChannel,myRows88,myRows89,cleanedRows,jobsprecleaning,st)
save.image(file = "jobs.RData")
