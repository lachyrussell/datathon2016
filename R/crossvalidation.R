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

load(file="readytogo.RData")


history <- xgb.cv(data = combined,
                  label = Y,
                  nround=150,
                  nthread = 4,
                  gamma= 0,
                  nfold = 2,
                  metrics = "auc",
                  max.depth = 3,
                  eta =.2,
                  early.round.stop = 3,
                  objective = "binary:logistic")
print(history)

               