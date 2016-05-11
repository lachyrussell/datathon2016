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

load(file="jobs.RData")
load(file = "preliminarytextmining.RData")
##################################################################################################
#Text Mining 
##################################################################################################
###########################
#Create Keyword Matrices 
###########################
#Title
##############
# convert the text data into a corpus of documents
docstitle<-Corpus(DataframeSource(data.frame(jobs[,"title"])))

show(docstitle) # confirm we've got the right number of documents

writeLines(as.character(docstitle[[1]])) # look at specific documents

# do some pre-processing

docstitle<-tm_map(docstitle, removePunctuation)
docstitle<-tm_map(docstitle, removeNumbers)
docstitle<-tm_map(docstitle,removeWords,stopwords("english"))
docstitle<-tm_map(docstitle,stemDocument)
docstitle<-tm_map(docstitle,tolower) # convert all to lower case
docstitle<-tm_map(docstitle,stripWhitespace)
docstitle <- tm_map(docstitle, PlainTextDocument) # this is needed for the next step: converting the data to a document-term matrix

writeLines(as.character(docstitle[[1]])) # look at what the documents look like now after all the pre-processing

# turn the data into a structured dataset
a0a<-DocumentTermMatrix(docstitle, control = list(dictionary = titlewordsa, global = c("50","Inf")))
a0b<-DocumentTermMatrix(docstitle, control = list(dictionary = titlewordsb, global = c("50","Inf")))
a0c<-DocumentTermMatrix(docstitle, control = list(dictionary = titlewordsc, global = c("50","Inf")))
a0d<-DocumentTermMatrix(docstitle, control = list(dictionary = titlewordsd, global = c("50","Inf")))

# turn the data into a structured dataset
a00a<-DocumentTermMatrix(docstitle, control = list(dictionary = titlewords2a, global = c("50","Inf")))
a00b<-DocumentTermMatrix(docstitle, control = list(dictionary = titlewords2b, global = c("50","Inf")))
a00c<-DocumentTermMatrix(docstitle, control = list(dictionary = titlewords2c, global = c("50","Inf")))


# see how many docs and how many terms in the matrix

#Create Sparse Title Matrices to feed xgboost
##########
load(file="jobs.RData")
a01a = as.matrix(a0a)
cleanjobs = c(colnames(jobs[1]))
jobs <- jobs[,cleanjobs]
a01a <- cbind(jobs,a01a)
dim(a01a)
load(file="jobs.RData")
X_testa01a = a01a[a01a[,1] %in% scoredata$job_id,]
a01a = a01a[a01a[,1] %in% trainingdata$job_id,]
rm(jobs)
colnames(X_testa01a) <- 1:ncol(a01a)
colnames(a01a) <- 1:ncol(a01a)

ohe_feats = colnames(a01a)[2:ncol(a01a)]
X_testa01a<-X_testa01a[,ohe_feats]
a01a<-a01a[,ohe_feats]

X_testa01a <- as.matrix(X_testa01a)
X_testa01a <- Matrix(X_testa01a)
X_testa01a <- as(X_testa01a, "dgCMatrix")

a01a<-as.matrix(a01a)
a01a<-Matrix(a01a)
a01a <- as(a01a, "dgCMatrix")
#----------------
load(file="jobs.RData")
a01b = as.matrix(a0b)
cleanjobs = c(colnames(jobs[1]))
jobs <- jobs[,cleanjobs]
a01b <- cbind(jobs,a01b)
dim(a01b)
load(file="jobs.RData")
X_testa01b = a01b[a01b[,1] %in% scoredata$job_id,]
a01b = a01b[a01b[,1] %in% trainingdata$job_id,]
rm(jobs)
colnames(X_testa01b) <- 1:ncol(a01b)
colnames(a01b) <- 1:ncol(a01b)

ohe_feats = colnames(a01b)[2:ncol(a01b)]
X_testa01b<-X_testa01b[,ohe_feats]
a01b<-a01b[,ohe_feats]

X_testa01b <- as.matrix(X_testa01b)
X_testa01b <- Matrix(X_testa01b)
X_testa01b <- as(X_testa01b, "dgCMatrix")

a01b<-as.matrix(a01b)
a01b<-Matrix(a01b)
a01b <- as(a01b, "dgCMatrix")
#------------------
load(file="jobs.RData")
a01c = as.matrix(a0c)
cleanjobs = c(colnames(jobs[1]))
jobs <- jobs[,cleanjobs]
a01c <- cbind(jobs,a01c)
dim(a01c)
load(file="jobs.RData")
X_testa01c = a01c[a01c[,1] %in% scoredata$job_id,]
a01c = a01c[a01c[,1] %in% trainingdata$job_id,]
rm(jobs)
colnames(X_testa01c) <- 1:ncol(a01c)
colnames(a01c) <- 1:ncol(a01c)

ohe_feats = colnames(a01c)[2:ncol(a01c)]
X_testa01c<-X_testa01c[,ohe_feats]
a01c<-a01c[,ohe_feats]

X_testa01c <- as.matrix(X_testa01c)
X_testa01c <- Matrix(X_testa01c)
X_testa01c <- as(X_testa01c, "dgCMatrix")

a01c<-as.matrix(a01c)
a01c<-Matrix(a01c)
a01c <- as(a01c, "dgCMatrix")
#-----------------------
load(file="jobs.RData")
a01d = as.matrix(a0d)
cleanjobs = c(colnames(jobs[1]))
jobs <- jobs[,cleanjobs]
a01d <- cbind(jobs,a01d)
dim(a01d)
load(file="jobs.RData")
X_testa01d = a01d[a01d[,1] %in% scoredata$job_id,]
a01d = a01d[a01d[,1] %in% trainingdata$job_id,]
rm(jobs)
colnames(X_testa01d) <- 1:ncol(a01d)
colnames(a01d) <- 1:ncol(a01d)

ohe_feats = colnames(a01d)[2:ncol(a01d)]
X_testa01d<-X_testa01d[,ohe_feats]
a01d<-a01d[,ohe_feats]

X_testa01d <- as.matrix(X_testa01d)
X_testa01d <- Matrix(X_testa01d)
X_testa01d <- as(X_testa01d, "dgCMatrix")

a01d<-as.matrix(a01d)
a01d<-Matrix(a01d)
a01d <- as(a01d, "dgCMatrix")
#----------------
load(file="jobs.RData")
a001a = as.matrix(a00a)
cleanjobs = c(colnames(jobs[1]))
jobs <- jobs[,cleanjobs]
a001a <- cbind(jobs,a001a)
dim(a001a)
load(file="jobs.RData")
X_testa001a = a001a[a001a[,1] %in% scoredata$job_id,]
a001a = a001a[a001a[,1] %in% trainingdata$job_id,]
rm(jobs)
colnames(X_testa001a) <- 1:ncol(a001a)
colnames(a001a) <- 1:ncol(a001a)

ohe_feats = colnames(a001a)[2:ncol(a001a)]
X_testa001a<-X_testa001a[,ohe_feats]
a001a<-a001a[,ohe_feats]

X_testa001a <- as.matrix(X_testa001a)
X_testa001a <- Matrix(X_testa001a)
X_testa001a <- as(X_testa001a, "dgCMatrix")

a001a<-as.matrix(a001a)
a001a<-Matrix(a001a)
a001a <- as(a001a, "dgCMatrix")
#----------------
load(file="jobs.RData")
a001b = as.matrix(a00b)
cleanjobs = c(colnames(jobs[1]))
jobs <- jobs[,cleanjobs]
a001b <- cbind(jobs,a001b)
dim(a001b)
load(file="jobs.RData")
X_testa001b = a001b[a001b[,1] %in% scoredata$job_id,]
a001b = a001b[a001b[,1] %in% trainingdata$job_id,]
rm(jobs)
colnames(X_testa001b) <- 1:ncol(a001b)
colnames(a001b) <- 1:ncol(a001b)

ohe_feats = colnames(a001b)[2:ncol(a001b)]
X_testa001b<-X_testa001b[,ohe_feats]
a001b<-a001b[,ohe_feats]

X_testa001b <- as.matrix(X_testa001b)
X_testa001b <- Matrix(X_testa001b)
X_testa001b <- as(X_testa001b, "dgCMatrix")

a001b<-as.matrix(a001b)
a001b<-Matrix(a001b)
a001b <- as(a001b, "dgCMatrix")
#----------------
load(file="jobs.RData")
a001c = as.matrix(a00c)
cleanjobs = c(colnames(jobs[1]))
jobs <- jobs[,cleanjobs]
a001c <- cbind(jobs,a001c)
dim(a001c)
load(file="jobs.RData")
X_testa001c = a001c[a001c[,1] %in% scoredata$job_id,]
a001c = a001c[a001c[,1] %in% trainingdata$job_id,]
rm(jobs)
colnames(X_testa001c) <- 1:ncol(a001c)
colnames(a001c) <- 1:ncol(a001c)

ohe_feats = colnames(a001c)[2:ncol(a001c)]
X_testa001c<-X_testa001c[,ohe_feats]
a001c<-a001c[,ohe_feats]

X_testa001c <- as.matrix(X_testa001c)
X_testa001c <- Matrix(X_testa001c)
X_testa001c <- as(X_testa001c, "dgCMatrix")

a001c<-as.matrix(a001c)
a001c<-Matrix(a001c)
a001c <- as(a001c, "dgCMatrix")
rm(docstitle)

#Queries
##############

# convert the text data into a corpus of documents
docsqueries<-Corpus(DataframeSource(data.frame(jobs[,"queries"])))

show(docsqueries) # confirm we've got the right number of documents

writeLines(as.character(docsqueries[[1]])) # look at specific documents

# do some pre-processing

docsqueries<-tm_map(docsqueries, removePunctuation)
docsqueries<-tm_map(docsqueries, removeNumbers)
docsqueries<-tm_map(docsqueries,removeWords,stopwords("english"))
docsqueries<-tm_map(docsqueries,stemDocument)
docsqueries<-tm_map(docsqueries,tolower) # convert all to lower case
docsqueries<-tm_map(docsqueries,stripWhitespace)
docsqueries <- tm_map(docsqueries, PlainTextDocument) # this is needed for the next step: converting the data to a document-term matrix

writeLines(as.character(docsqueries[[22]])) # look at what the documents look like now after all the pre-processing

# turn the data into a structured dataset
b0<-DocumentTermMatrix(docsqueries, control = list(dictionary = querywords, global = c("50","Inf")))
b0 # see how many docs and how many terms in the matrix
# turn the data into a structured dataset

b00a<-DocumentTermMatrix(docsqueries, control = list(dictionary = querywords2a, global = c("50","Inf")))
b00b<-DocumentTermMatrix(docsqueries, control = list(dictionary = querywords2b, global = c("50","Inf")))


# see how many docs and how many terms in the matrix

#Create Sparse Title Matrices to feed xgboost
##########
load(file="jobs.RData")
b01 = as.matrix(b0)
cleanjobs = c(colnames(jobs[1]))
jobs <- jobs[,cleanjobs]
b01 <- cbind(jobs,b01)
dim(b01)
load(file="jobs.RData")
X_testb01 = b01[b01[,1] %in% scoredata$job_id,]
b01 = b01[b01[,1] %in% trainingdata$job_id,]
rm(jobs)
colnames(X_testb01) <- 1:ncol(b01)
colnames(b01) <- 1:ncol(b01)

ohe_feats = colnames(b01)[2:ncol(b01)]
X_testb01<-X_testb01[,ohe_feats]
b01<-b01[,ohe_feats]

X_testb01 <- as.matrix(X_testb01)
X_testb01 <- Matrix(X_testb01)
X_testb01 <- as(X_testb01, "dgCMatrix")

b01<-as.matrix(b01)
b01<-Matrix(b01)
b01 <- as(b01, "dgCMatrix")
#------------------------------------
load(file="jobs.RData")
b001a<-as.matrix(b00a)
cleanjobs = c(colnames(jobs[1]))
jobs <- jobs[,cleanjobs]
b001a <- cbind(jobs,b001a)
dim(b001a)
load(file="jobs.RData")
X_testb001a = b001a[b001a[,1] %in% scoredata$job_id,]
b001a = b001a[b001a[,1] %in% trainingdata$job_id,]
rm(jobs)
colnames(X_testb001a) <- 1:ncol(b001a)
colnames(b001a) <- 1:ncol(b001a)

ohe_feats = colnames(b001a)[2:ncol(b001a)]
X_testb001a<-X_testb001a[,ohe_feats]
b001a<-b001a[,ohe_feats]

X_testb001a <- as.matrix(X_testb001a)
X_testb001a <- Matrix(X_testb001a)
X_testb001a <- as(X_testb001a, "dgCMatrix")

b001a<-as.matrix(b001a)
b001a<-Matrix(b001a)
b001a <- as(b001a, "dgCMatrix")
#----------------------------------
load(file="jobs.RData")
b001b<-as.matrix(b00b)
cleanjobs = c(colnames(jobs[1]))
jobs <- jobs[,cleanjobs]
b001b <- cbind(jobs,b001b)
dim(b001b)
load(file="jobs.RData")
X_testb001b = b001b[b001b[,1] %in% scoredata$job_id,]
b001b = b001b[b001b[,1] %in% trainingdata$job_id,]
rm(jobs)
colnames(X_testb001b) <- 1:ncol(b001b)
colnames(b001b) <- 1:ncol(b001b)

ohe_feats = colnames(b001b)[2:ncol(b001b)]
X_testb001b<-X_testb001b[,ohe_feats]
b001b<-b001b[,ohe_feats]

X_testb001b <- as.matrix(X_testb001b)
X_testb001b <- Matrix(X_testb001b)
X_testb001b <- as(X_testb001b, "dgCMatrix")

b001b<-as.matrix(b001b)
b001b<-Matrix(b001b)
b001b <- as(b001b, "dgCMatrix")
rm(docsqueries)


#Abstract
##############

# convert the text data into a corpus of documents
docsabstract<-Corpus(DataframeSource(data.frame(jobs[,"abstract"])))

show(docsabstract) # confirm we've got the right number of documents

writeLines(as.character(docsabstract[[1]])) # look at specific documents

# do some pre-processing

docsabstract<-tm_map(docsabstract, removePunctuation)
docsabstract<-tm_map(docsabstract, removeNumbers)
docsabstract<-tm_map(docsabstract,removeWords,stopwords("english"))
docsabstract<-tm_map(docsabstract,stemDocument)
docsabstract<-tm_map(docsabstract,tolower) # convert all to lower case
docsabstract<-tm_map(docsabstract,stripWhitespace)
docsabstract <- tm_map(docsabstract, PlainTextDocument) # this is needed for the next step: converting the data to a document-term matrix

writeLines(as.character(docsabstract[[1]])) # look at what the documents look like now after all the pre-processing
save.image("C:/Users/lachyrussell/Desktop/QuickModel/toolong12.RData")
# turn the data into a structured dataset
c0a<-DocumentTermMatrix(docsabstract, control = list(dictionary = abstractwordsa, global = c("50","Inf")))
c0a # see how many docs and how many terms in the matrix
c0b<-DocumentTermMatrix(docsabstract, control = list(dictionary = abstractwordsb, global = c("50","Inf")))
c0b # see how many docs and how many terms in the matrix
c0c<-DocumentTermMatrix(docsabstract, control = list(dictionary = abstractwordsc, global = c("50","Inf")))
c0c # see how many docs and how many terms in the matrix
c0d<-DocumentTermMatrix(docsabstract, control = list(dictionary = abstractwordsd, global = c("50","Inf")))
c0d # see how many docs and how many terms in the matrix

c00a<-DocumentTermMatrix(docsabstract, control = list(dictionary = abstractwords2a, global = c("50","Inf"), weighting = weightTfIdf)) #tfidf coding
c00a # see how many docs and how many terms in the matrix
c00b<-DocumentTermMatrix(docsabstract, control = list(dictionary = abstractwords2b, global = c("50","Inf"), weighting = weightTfIdf)) #tfidf coding
c00b # see how many docs and how many terms in the matrix

#Create Sparse Title Matrices to feed xgboost
##########
load(file="jobs.RData")
c01a = as.matrix(c0a)
cleanjobs = c(colnames(jobs[1]))
jobs <- jobs[,cleanjobs]
c01a <- cbind(jobs,c01a)
dim(c01a)
load(file="jobs.RData")
X_testc01a = c01a[c01a[,1] %in% scoredata$job_id,]
c01a = c01a[c01a[,1] %in% trainingdata$job_id,]
rm(jobs)
colnames(X_testc01a) <- 1:ncol(c01a)
colnames(c01a) <- 1:ncol(c01a)

ohe_feats = colnames(c01a)[2:ncol(c01a)]
X_testc01a<-X_testc01a[,ohe_feats]
c01a<-c01a[,ohe_feats]

X_testc01a <- as.matrix(X_testc01a)
X_testc01a <- Matrix(X_testc01a)
X_testc01a <- as(X_testc01a, "dgCMatrix")

c01a<-as.matrix(c01a)
c01a<-Matrix(c01a)
c01a <- as(c01a, "dgCMatrix")
#----------------
load(file="jobs.RData")
c01b = as.matrix(c0b)
cleanjobs = c(colnames(jobs[1]))
jobs <- jobs[,cleanjobs]
c01b <- cbind(jobs,c01b)
dim(c01b)
load(file="jobs.RData")
X_testc01b = c01b[c01b[,1] %in% scoredata$job_id,]
c01b = c01b[c01b[,1] %in% trainingdata$job_id,]
rm(jobs)
colnames(X_testc01b) <- 1:ncol(c01b)
colnames(c01b) <- 1:ncol(c01b)

ohe_feats = colnames(c01b)[2:ncol(c01b)]
X_testc01b<-X_testc01b[,ohe_feats]
c01b<-c01b[,ohe_feats]

X_testc01b <- as.matrix(X_testc01b)
X_testc01b <- Matrix(X_testc01b)
X_testc01b <- as(X_testc01b, "dgCMatrix")

c01b<-as.matrix(c01b)
c01b<-Matrix(c01b)
c01b <- as(c01b, "dgCMatrix")
#----------------
load(file="jobs.RData")
c01c = as.matrix(c0c)
cleanjobs = c(colnames(jobs[1]))
jobs <- jobs[,cleanjobs]
c01c <- cbind(jobs,c01c)
dim(c01c)
load(file="jobs.RData")
X_testc01c = c01c[c01c[,1] %in% scoredata$job_id,]
c01c = c01c[c01c[,1] %in% trainingdata$job_id,]
rm(jobs)
colnames(X_testc01c) <- 1:ncol(c01c)
colnames(c01c) <- 1:ncol(c01c)

ohe_feats = colnames(c01c)[2:ncol(c01c)]
X_testc01c<-X_testc01c[,ohe_feats]
c01c<-c01c[,ohe_feats]

X_testc01c <- as.matrix(X_testc01c)
X_testc01c <- Matrix(X_testc01c)
X_testc01c <- as(X_testc01c, "dgCMatrix")

c01c<-as.matrix(c01c)
c01c<-Matrix(c01c)
c01c <- as(c01c, "dgCMatrix")
#----------------
load(file="jobs.RData")
c01d = as.matrix(c0d)
cleanjobs = c(colnames(jobs[1]))
jobs <- jobs[,cleanjobs]
c01d <- cbind(jobs,c01d)
dim(c01d)
load(file="jobs.RData")
X_testc01d = c01d[c01d[,1] %in% scoredata$job_id,]
c01d = c01d[c01d[,1] %in% trainingdata$job_id,]
rm(jobs)
colnames(X_testc01d) <- 1:ncol(c01d)
colnames(c01d) <- 1:ncol(c01d)

ohe_feats = colnames(c01d)[2:ncol(c01d)]
X_testc01d<-X_testc01d[,ohe_feats]
c01d<-c01d[,ohe_feats]

X_testc01d <- as.matrix(X_testc01d)
X_testc01d <- Matrix(X_testc01d)
X_testc01d <- as(X_testc01d, "dgCMatrix")

c01d<-as.matrix(c01d)
c01d<-Matrix(c01d)
c01d <- as(c01d, "dgCMatrix")
#----------------
load(file="jobs.RData")
c001a = as.matrix(c00a)
cleanjobs = c(colnames(jobs[1]))
jobs <- jobs[,cleanjobs]
c001a <- cbind(jobs,c001a)
dim(c001a)
load(file="jobs.RData")
X_testc001a = c001a[c001a[,1] %in% scoredata$job_id,]
c001a = c001a[c001a[,1] %in% trainingdata$job_id,]
rm(jobs)
colnames(X_testc001a) <- 1:ncol(c001a)
colnames(c001a) <- 1:ncol(c001a)

ohe_feats = colnames(c001a)[2:ncol(c001a)]
X_testc001a<-X_testc001a[,ohe_feats]
c001a<-c001a[,ohe_feats]

X_testc001a <- as.matrix(X_testc001a)
X_testc001a <- Matrix(X_testc001a)
X_testc001a <- as(X_testc001a, "dgCMatrix")

c001a<-as.matrix(c001a)
c001a<-Matrix(c001a)
c001a <- as(c001a, "dgCMatrix")
#----------------
load(file="jobs.RData")
c001b = as.matrix(c00b)
cleanjobs = c(colnames(jobs[1]))
jobs <- jobs[,cleanjobs]
c001b <- cbind(jobs,c001b)
dim(c001b)
load(file="jobs.RData")
X_testc001b = c001b[c001b[,1] %in% scoredata$job_id,]
c001b = c001b[c001b[,1] %in% trainingdata$job_id,]
rm(jobs)
colnames(X_testc001b) <- 1:ncol(c001b)
colnames(c001b) <- 1:ncol(c001b)

ohe_feats = colnames(c001b)[2:ncol(c001b)]
X_testc001b<-X_testc001b[,ohe_feats]
c001b<-c001b[,ohe_feats]

X_testc001b <- as.matrix(X_testc001b)
X_testc001b <- Matrix(X_testc001b)
X_testc001b <- as(X_testc001b, "dgCMatrix")

c001b<-as.matrix(c001b)
c001b<-Matrix(c001b)
c001b <- as(c001b, "dgCMatrix")

rm(docsabstract)

#Location
##############

# convert the text data into a corpus of documents
docslocation<-Corpus(DataframeSource(data.frame(jobs[,"location_id"])))

show(docslocation) # confirm we've got the right number of documents

writeLines(as.character(docslocation[[1]])) # look at specific documents

# do some pre-processing

docslocation <- tm_map(docslocation, PlainTextDocument) # this is needed for the next step: converting the data to a document-term matrix

writeLines(as.character(docslocation[[1]])) # look at what the documents look like now after all the pre-processing

# turn the data into a structured dataset
d0a<-DocumentTermMatrix(docslocation, control = list(dictionary = locationwordsa, global = c("50","Inf")))
d0a # see how many docs and how many terms in the matrix
d0b<-DocumentTermMatrix(docslocation, control = list(dictionary = locationwordsb, global = c("50","Inf")))
d0b # see how many docs and how many terms in the matrix
d0c<-DocumentTermMatrix(docslocation, control = list(dictionary = locationwordsc, global = c("50","Inf")))
d0c # see how many docs and how many terms in the matrix

rm(docslocation)

# turn the data into a structured dataset
d00<-DocumentTermMatrix(docslocation, control = list(dictionary = locationwords2, global = c("100","Inf")))
d00 # see how many docs and how many terms in the matrix

#Create Sparse Title Matrices to feed xgboost
##########
load(file="jobs.RData")
d01a = as.matrix(d0a)
cleanjobs = c(colnames(jobs[1]))
jobs <- jobs[,cleanjobs]
d01a <- cbind(jobs,d01a)
dim(d01a)
load(file="jobs.RData")
X_testd01a = d01a[d01a[,1] %in% scoredata$job_id,]
d01a = d01a[d01a[,1] %in% trainingdata$job_id,]
rm(jobs)
colnames(X_testd01a) <- 1:ncol(d01a)
colnames(d01a) <- 1:ncol(d01a)

ohe_feats = colnames(d01a)[2:ncol(d01a)]
X_testd01a<-X_testd01a[,ohe_feats]
d01a<-d01a[,ohe_feats]

X_testd01a <- as.matrix(X_testd01a)
X_testd01a <- Matrix(X_testd01a)
X_testd01a <- as(X_testd01a, "dgCMatrix")

d01a<-as.matrix(d01a)
d01a<-Matrix(d01a)
d01a <- as(d01a, "dgCMatrix")
#-------------------------------
load(file="jobs.RData")
d01b = as.matrix(d0b)
cleanjobs = c(colnames(jobs[1]))
jobs <- jobs[,cleanjobs]
d01b <- cbind(jobs,d01b)
dim(d01b)
load(file="jobs.RData")
X_testd01b = d01b[d01b[,1] %in% scoredata$job_id,]
d01b = d01b[d01b[,1] %in% trainingdata$job_id,]
rm(jobs)
colnames(X_testd01b) <- 1:ncol(d01b)
colnames(d01b) <- 1:ncol(d01b)

ohe_feats = colnames(d01b)[2:ncol(d01b)]
X_testd01b<-X_testd01b[,ohe_feats]
d01b<-d01b[,ohe_feats]

X_testd01b <- as.matrix(X_testd01b)
X_testd01b <- Matrix(X_testd01b)
X_testd01b <- as(X_testd01b, "dgCMatrix")

d01b<-as.matrix(d01b)
d01b<-Matrix(d01b)
d01b <- as(d01b, "dgCMatrix")
#-------------------------------
load(file="jobs.RData")
d01c = as.matrix(d0c)
cleanjobs = c(colnames(jobs[1]))
jobs <- jobs[,cleanjobs]
d01c <- cbind(jobs,d01c)
dim(d01c)
load(file="jobs.RData")
X_testd01c = d01c[d01c[,1] %in% scoredata$job_id,]
d01c = d01c[d01c[,1] %in% trainingdata$job_id,]
rm(jobs)
colnames(X_testd01c) <- 1:ncol(d01c)
colnames(d01c) <- 1:ncol(d01c)

ohe_feats = colnames(d01c)[2:ncol(d01c)]
X_testd01c<-X_testd01c[,ohe_feats]
d01c<-d01c[,ohe_feats]

X_testd01c <- as.matrix(X_testd01c)
X_testd01c <- Matrix(X_testd01c)
X_testd01c <- as(X_testd01c, "dgCMatrix")

d01c<-as.matrix(d01c)
d01c<-Matrix(d01c)
d01c <- as(d01c, "dgCMatrix")
#-------------------------------
load(file="jobs.RData")
d001 = as.matrix(d00)
cleanjobs = c(colnames(jobs[1]))
jobs <- jobs[,cleanjobs]
d001 <- cbind(jobs,d001)
dim(d001)
load(file="jobs.RData")
X_testd001 = d001[d001[,1] %in% scoredata$job_id,]
d001 = d001[d001[,1] %in% trainingdata$job_id,]
rm(jobs)
colnames(X_testd001) <- 1:ncol(d001)
colnames(d001) <- 1:ncol(d001)

ohe_feats = colnames(d001)[2:ncol(d001)]
X_testd001<-X_testd001[,ohe_feats]
d001<-d001[,ohe_feats]

X_testd001 <- as.matrix(X_testd001)
X_testd001 <- Matrix(X_testd001)
X_testd001 <- as(X_testd001, "dgCMatrix")

d001<-as.matrix(d001)
d001<-Matrix(d001)
d001 <- as(d001, "dgCMatrix")

rm(docslocation)

#raw_job_type
##############
# convert the text data into a corpus of documents
docsraw_job_type<-Corpus(DataframeSource(data.frame(jobs[,"raw_job_type"])))

show(docsraw_job_type) # confirm we've got the right number of documents

writeLines(as.character(docsraw_job_type[[1]])) # look at specific documents

# do some pre-processing

docsraw_job_type<-tm_map(docsraw_job_type, removePunctuation)
docsraw_job_type<-tm_map(docsraw_job_type, removeNumbers)
docsraw_job_type<-tm_map(docsraw_job_type,removeWords,stopwords("english"))
docsraw_job_type<-tm_map(docsraw_job_type,stemDocument)
docsraw_job_type<-tm_map(docsraw_job_type,tolower) # convert all to lower case
docsraw_job_type<-tm_map(docsraw_job_type,stripWhitespace)
docsraw_job_type <- tm_map(docsraw_job_type, PlainTextDocument) # this is needed for the next step: converting the data to a document-term matrix

writeLines(as.character(docsraw_job_type[[1]])) # look at what the documents look like now after all the pre-processing

# turn the data into a structured dataset
r0<-DocumentTermMatrix(docsraw_job_type)
r0 # see how many docs and how many terms in the matrix

#Create Sparse Title Matrices to feed xgboost
##########
load(file="jobs.RData")
r01<-removeSparseTerms(r0,0.99)
r01<-as.matrix(r01)
r01<-subset(r01, select = c("casual","casualvac","contracttemp","full","fulltim","part","parttim","perman","temporari"))
cleanjobs = c(colnames(jobs[1]))
jobs <- jobs[,cleanjobs]
r01 <- cbind(jobs,r01)
dim(r01)
load(file="jobs.RData")
X_testr01 = r01[r01[,1] %in% scoredata$job_id,]
r01 = r01[r01[,1] %in% trainingdata$job_id,]
rm(jobs)
colnames(X_testr01) <- 1:ncol(r01)
colnames(r01) <- 1:ncol(r01)

ohe_feats = colnames(r01)[2:ncol(r01)]
X_testr01<-X_testr01[,ohe_feats]
r01<-r01[,ohe_feats]

X_testr01 <- as.matrix(X_testr01)
X_testr01 <- Matrix(X_testr01)
X_testr01 <- as(X_testr01, "dgCMatrix")

r01<-as.matrix(r01)
r01<-Matrix(r01)
r01 <- as(r01, "dgCMatrix")

rm(docsraw_job_type)

#remove all the collected stuff except keywords e.g. abstractwords2a)
rm(list=ls())
load(file = "c.RData")
load(file = "c0.RData")
load(file = "trouble.RData")
load(file = "trouble2.RData")
load(file = "trouble3.RData")
load(file = "intermed6.RData")
combined <- cbind(a01a,a01b,
                  a01c,a01d,a001a,a001b,
                  a001c,b001a,b001b,b01,
                  c01,c01a,c01b,c01c,
                  c01d,c001,c001a,c001b,
                  d001,d01a,d01b,d01c,r01)
#combined <- cbind(combined,trouble0001a)
#,trouble0001b,
combined <- cbind(combined,trouble01a,trouble01b,trouble001a,trouble001b)
X_test <- cbind(X_testa01a,X_testa01b,
                X_testa01c,X_testa01d,X_testa001a,X_testa001b,
                X_testa001c,X_testb001a,X_testb001b,X_testb01,
                X_testc01,X_testc01a,X_testc01b,X_testc01c,
                X_testc01d,X_testc001,X_testc001a,X_testc001b,
                X_testd001,X_testd01a,X_testd01b,X_testd01c,X_testr01)
#X_test <- cbind(X_test,X_testtrouble0001a,X_testtrouble0001b,
              X_test <- cbind(X_test,X_testtrouble01a,X_testtrouble01b,X_testtrouble001a,X_testtrouble001b) 
rm(X_testtrouble0001a,X_testtrouble0001b,X_testtrouble001a,X_testtrouble001b,X_testtrouble01a,X_testtrouble01b,trouble0001a,trouble0001b,trouble001a,trouble001b,trouble01a,trouble01b,a01a,a01b,a01c,a01d,a001a,a001b,a001c,b001a,b001b,b01,c01,c01a,c01b,c01c,c01d,c001,c001a,c001b,d001,d01a,d01b,d01c,r01,X_testa01a,X_testa01b,X_testa01c,X_testa01d,X_testa001a,X_testa001b,X_testa001c,X_testb001a,X_testb001b,X_testb01,X_testc01,X_testc001,X_testc01a,X_testc01b,X_testc01c,X_testc01d,X_testc001a,X_testc001b,X_testd001,X_testd01a,X_testd01b,X_testd01c,X_testr01)
save.image("bigmatrix.RData")
