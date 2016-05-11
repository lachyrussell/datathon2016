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

load(file="jobs.RData")
load(file="bigmatrix.RData")

# one-hot-encoding categorical features
#---------------------------------------
cleanjobs = c(colnames(jobs[1]),colnames(jobs[5]),colnames(jobs[6]),colnames(jobs[9]),colnames(jobs[11]),colnames(jobs[12]))
jobs <- jobs[,cleanjobs]
dim(jobs)
dim(combined)
X_testjobs = jobs[jobs$job_id %in% scoredata$job_id,]
combinedjobs = jobs[jobs$job_id %in% trainingdata$job_id,]
colnames(X_testjobs) <- 1:length(combinedjobs)
colnames(combinedjobs) <- 1:length(combinedjobs)
Y <- as.numeric(combinedjobs$"5")
trainprediction<- as.numeric(combinedjobs$"6")
verifyhat <-as.numeric(X_testjobs$"5")
verifyprediction <- as.numeric(X_testjobs$"6")
ohe_jobfeats = c(colnames(combinedjobs[2]),colnames(combinedjobs[3]),colnames(combinedjobs[4]))

combinedjobs <- combinedjobs[,ohe_jobfeats]
combinedjobs <- as.matrix(combinedjobs)
combinedjobs<-Matrix(combinedjobs)
combinedjobs <- as(combinedjobs, "dgCMatrix")

X_testjobs <- X_testjobs[,ohe_jobfeats]
X_testjobs<- as.matrix(X_testjobs)
X_testjobs <- Matrix(X_testjobs)
X_testjobs <- as(X_testjobs, "dgCMatrix")

combined <- cbind(combinedjobs,combined)
X_test <- cbind(X_testjobs,X_test)
rm(jobs,ohe_jobfeats,X_testjobs,combinedjobs,cleanjobs)
save.image("readytogo.RData")

