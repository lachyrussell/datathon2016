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
odbcChannel <- odbcConnect("datathon2016")
##################################################################################################
#Preliminary Text Mining 
##################################################################################################
#TRUE POSITIVES Title, Queries, Abstract and Location
#Look for features with strong positive correlation with hat variable
#Use my joinperfect database to create bags of key words
##################################################################################################
positiveRows <- which(jobs$hat == 1)
positivedata=jobs[positiveRows,]

joinperfect <- sqlFetch(odbcChannel, "joinperfect")

# convert the text data into a corpus of documents

joinperfecttitle<-Corpus(DataframeSource(data.frame(joinperfect[,"title"])))
joinperfectqueries<-Corpus(DataframeSource(data.frame(joinperfect[,"queries"])))
joinperfectabstract<-Corpus(DataframeSource(data.frame(joinperfect[,"abstract"])))
joinperfectlocation<-Corpus(DataframeSource(data.frame(joinperfect[,"location_id"])))


writeLines(as.character(joinperfecttitle[[1]])) # look at specific documents
writeLines(as.character(joinperfectqueries[[1]])) # look at specific documents
writeLines(as.character(joinperfectabstract[[1]])) # look at specific documents
writeLines(as.character(joinperfectlocation[[1]])) # look at specific documents

# do some pre-processing

joinperfecttitle<-tm_map(joinperfecttitle, removePunctuation)
joinperfecttitle<-tm_map(joinperfecttitle, removeNumbers)
joinperfecttitle<-tm_map(joinperfecttitle,removeWords,stopwords("english"))
joinperfecttitle<-tm_map(joinperfecttitle,stemDocument)
joinperfecttitle<-tm_map(joinperfecttitle,tolower) # convert all to lower case
joinperfecttitle<-tm_map(joinperfecttitle,stripWhitespace)
joinperfecttitle <- tm_map(joinperfecttitle, PlainTextDocument) # this is needed for the next step: converting the data to a document-term matrix

joinperfectqueries<-tm_map(joinperfectqueries, removePunctuation)
joinperfectqueries<-tm_map(joinperfectqueries, removeNumbers)
joinperfectqueries<-tm_map(joinperfectqueries,removeWords,stopwords("english"))
joinperfectqueries<-tm_map(joinperfectqueries,stemDocument)
joinperfectqueries<-tm_map(joinperfectqueries,tolower) # convert all to lower case
joinperfectqueries<-tm_map(joinperfectqueries,stripWhitespace)
joinperfectqueries <- tm_map(joinperfectqueries, PlainTextDocument) # this is needed for the next step: converting the data to a document-term matrix

joinperfectabstract<-tm_map(joinperfectabstract, removePunctuation)
joinperfectabstract<-tm_map(joinperfectabstract, removeNumbers)
joinperfectabstract<-tm_map(joinperfectabstract,removeWords,stopwords("english"))
joinperfectabstract<-tm_map(joinperfectabstract,stemDocument)
joinperfectabstract<-tm_map(joinperfectabstract,tolower) # convert all to lower case
joinperfectabstract<-tm_map(joinperfectabstract,stripWhitespace)
joinperfectabstract <- tm_map(joinperfectabstract, PlainTextDocument) # this is needed for the next step: converting the data to a document-term matrix

joinperfectlocation <- tm_map(joinperfectlocation, PlainTextDocument) # this is needed for the next step: converting the data to a document-term matrix

writeLines(as.character(joinperfecttitle[[1]])) # look at what the documents look like now after all the pre-processing
writeLines(as.character(joinperfectqueries[[1]])) # look at what the documents look like now after all the pre-processing
writeLines(as.character(joinperfectabstract[[1]])) # look at what the documents look like now after all the pre-processing
writeLines(as.character(joinperfectlocation[[1]])) # look at what the documents look like now after all the pre-processing

# turn the data into structured datasets
x0<-DocumentTermMatrix(joinperfecttitle) #binary coding
x0 # see how many docs and how many terms in the matrix
inspect(x0[1:10,100:110])


y0<-DocumentTermMatrix(joinperfectqueries) #binary coding
y0 # see how many docs and how many terms in the matrix
inspect(y0[1:10,100:110])


z0<-DocumentTermMatrix(joinperfectabstract) #binary coding
z0 # see how many docs and how many terms in the matrix
inspect(z0[1:10,100:110])

w0<-DocumentTermMatrix(joinperfectlocation) #binary coding
w0 # see how many docs and how many terms in the matrix
inspect(w0[1:10,100:110])

# remove sparse terms reduce number of columns note that the 2nd parameter is arbitrary
x1<-removeSparseTerms(x0,0.9999)
x1
y1<-removeSparseTerms(y0,0.9999)
y1
z1<-removeSparseTerms(z0,0.999)
z1
w1<-removeSparseTerms(w0,0.9999)
w1


# convert the data into a matrix object - some of the below operations require this
x<-as.matrix(x1)
x[1:10]
y<-as.matrix(y1)
y[1:10]
z<-as.matrix(z1)
z[1:10]
w<-as.matrix(w1)
w[1:10]


xcols <- colnames(x)
ycols <- colnames(y)
zcols <- colnames(z)
wcols <- colnames(w)

#Identify the key words

#Title
####

trainrowspredictorx <- vector(mode="integer", nrow(data.frame(xcols)))
c <- 1
for(i in xcols)
{
  trainrowspredictorx[c] <- nrow(data.frame(grep(pattern=i,x=trainingdata$title,ignore.case = TRUE,value=FALSE)))
  c <- c+1
}

positiverowspredictorx <- vector(mode="integer", nrow(data.frame(xcols)))
c <- 1
for(i in xcols)
{
  positiverowspredictorx[c] <- nrow(data.frame(grep(pattern=i,x=positivedata$title,ignore.case = TRUE,value=FALSE)))
  c <- c+1
}

goodwordspredictorx <- vector(mode="numeric", nrow(data.frame(xcols)))
for(i in 1:nrow(data.frame(xcols)))
{goodwordspredictorx[i] <-  positiverowspredictorx[i]/(trainrowspredictorx[i])
}

a1 <- data.frame(goodwordspredictorx, row.names = xcols)
b1 <- subset(a1, goodwordspredictorx>=0.20) #this can be changed 
b1 <- t(b1)
titlewordsa<- colnames(b1)[1:250]
titlewordsb<- colnames(b1)[251:500]
titlewordsc<- colnames(b1)[501:750]
titlewordsd<- colnames(b1)[751:length(b1)]
#Queries
####

trainrowspredictory <- vector(mode="integer", nrow(data.frame(xcols)))
c <- 1
for(i in ycols)
{
  trainrowspredictory[c] <- nrow(data.frame(grep(pattern=i,x=trainingdata$queries,ignore.case = TRUE,value=FALSE)))
  c <- c+1
}

positiverowspredictory <- vector(mode="integer", nrow(data.frame(ycols)))
c <- 1
for(i in ycols)
{
  positiverowspredictory[c] <- nrow(data.frame(grep(pattern=i,x=positivedata$queries,ignore.case = TRUE,value=FALSE)))
  c <- c+1
}

goodwordspredictory <- vector(mode="numeric", nrow(data.frame(ycols)))
for(i in 1:nrow(data.frame(ycols)))
{goodwordspredictory[i] <-  positiverowspredictory[i]/(trainrowspredictory[i])
}

a2 <- data.frame(goodwordspredictory, row.names = ycols)
b2 <- subset(a2, goodwordspredictory>=0.20)#this can be changed 
b2 <- t(b2)
querywords <- colnames(b2)

#Abstract
####

trainrowspredictorz <- vector(mode="integer", nrow(data.frame(zcols)))
c <- 1
for(i in zcols)
{
  trainrowspredictorz[c] <- nrow(data.frame(grep(pattern=i,x=trainingdata$abstract,ignore.case = TRUE,value=FALSE)))
  c <- c+1
}

positiverowspredictorz <- vector(mode="integer", nrow(data.frame(zcols)))
c <- 1
for(i in zcols)
{
  positiverowspredictorz[c] <- nrow(data.frame(grep(pattern=i,x=positivedata$abstract,ignore.case = TRUE,value=FALSE)))
  c <- c+1
}

goodwordspredictorz <- vector(mode="numeric", nrow(data.frame(zcols)))
for(i in 1:nrow(data.frame(zcols)))
{goodwordspredictorz[i] <-  positiverowspredictorz[i]/(trainrowspredictorz[i])
}

a3 <- data.frame(goodwordspredictorz, row.names = zcols)
b3 <- subset(a3, goodwordspredictorz>=0.20) #this can be changed
b3 <- t(b3)
abstractwordsa<- colnames(b3)[1:150]
abstractwordsb<- colnames(b3)[151:300]
abstractwordsc<- colnames(b3)[301:450]
abstractwordsd<- colnames(b3)[451:length(b3)]

#Location
####

trainrowspredictorw <- vector(mode="integer", nrow(data.frame(wcols)))
c <- 1
for(i in wcols)
{
  trainrowspredictorw[c] <- nrow(data.frame(grep(pattern=i,x=trainingdata$location_id,ignore.case = TRUE,value=FALSE)))
  c <- c+1
}

positiverowspredictorw <- vector(mode="integer", nrow(data.frame(wcols)))
c <- 1
for(i in wcols)
{
  positiverowspredictorw[c] <- nrow(data.frame(grep(pattern=i,x=positivedata$location_id,ignore.case = TRUE,value=FALSE)))
  c <- c+1
}

goodwordspredictorw <- vector(mode="numeric", nrow(data.frame(wcols)))
for(i in 1:nrow(data.frame(wcols)))
{goodwordspredictorw[i] <-  positiverowspredictorw[i]/(trainrowspredictorw[i])
}

a4 <- data.frame(goodwordspredictorw, row.names = wcols)
b4 <- subset(a4, goodwordspredictorw>=0.20) #this can be changed
b4 <- t(b4)
locationwordsa<- colnames(b4)[1:250]
locationwordsb<- colnames(b4)[251:500]
locationwordsc<- colnames(b4)[501:length(b4)]

##################################################################################################
#FURTHER FEATURE CREATION
##################################################################################################

#TRUE NEGATIVES Title, Queries, Abstract and Location
#Look for features with strong negative correlation with hat variable
#Use my joindisperfect database to create bags of key words
##################################################################################################
zeroRows <- which(jobs$hat== 0)
zerodata=jobs[zeroRows,]
joindisperfect <- sqlFetch(odbcChannel, "joindisperfect")

# convert the text data into a corpus of documents

joindisperfecttitle<-Corpus(DataframeSource(data.frame(joindisperfect[,"title"])))
joindisperfectqueries<-Corpus(DataframeSource(data.frame(joindisperfect[,"queries"])))
joindisperfectabstract<-Corpus(DataframeSource(data.frame(joindisperfect[,"abstract"])))
joindisperfectlocation<-Corpus(DataframeSource(data.frame(joindisperfect[,"location_id"])))

writeLines(as.character(joindisperfecttitle[[1]])) # look at specific documents
writeLines(as.character(joindisperfectqueries[[1]])) # look at specific documents
writeLines(as.character(joindisperfectabstract[[1]])) # look at specific documents
writeLines(as.character(joindisperfectlocation[[1]])) # look at specific documents

# do some pre-processing

joindisperfecttitle<-tm_map(joindisperfecttitle, removePunctuation)
joindisperfecttitle<-tm_map(joindisperfecttitle, removeNumbers)
joindisperfecttitle<-tm_map(joindisperfecttitle,removeWords,stopwords("english"))
joindisperfecttitle<-tm_map(joindisperfecttitle,stemDocument)
joindisperfecttitle<-tm_map(joindisperfecttitle,tolower) # convert all to lower case
joindisperfecttitle<-tm_map(joindisperfecttitle,stripWhitespace)
joindisperfecttitle <- tm_map(joindisperfecttitle, PlainTextDocument) # this is needed for the next step: converting the data to a document-term matrix

joindisperfectqueries<-tm_map(joindisperfectqueries, removePunctuation)
joindisperfectqueries<-tm_map(joindisperfectqueries, removeNumbers)
joindisperfectqueries<-tm_map(joindisperfectqueries,removeWords,stopwords("english"))
joindisperfectqueries<-tm_map(joindisperfectqueries,stemDocument)
joindisperfectqueries<-tm_map(joindisperfectqueries,tolower) # convert all to lower case
joindisperfectqueries<-tm_map(joindisperfectqueries,stripWhitespace)
joindisperfectqueries <- tm_map(joindisperfectqueries, PlainTextDocument) # this is needed for the next step: converting the data to a document-term matrix

joindisperfectabstract<-tm_map(joindisperfectabstract, removePunctuation)
joindisperfectabstract<-tm_map(joindisperfectabstract, removeNumbers)
joindisperfectabstract<-tm_map(joindisperfectabstract,removeWords,stopwords("english"))
joindisperfectabstract<-tm_map(joindisperfectabstract,stemDocument)
joindisperfectabstract<-tm_map(joindisperfectabstract,tolower) # convert all to lower case
joindisperfectabstract<-tm_map(joindisperfectabstract,stripWhitespace)
joindisperfectabstract <- tm_map(joindisperfectabstract, PlainTextDocument) # this is needed for the next step: converting the data to a document-term matrix

joindisperfectlocation <- tm_map(joindisperfectlocation, PlainTextDocument) # this is needed for the next step: converting the data to a document-term matrix


writeLines(as.character(joindisperfecttitle[[1]])) # look at what the documents look like now after all the pre-processing
writeLines(as.character(joindisperfectqueries[[1]])) # look at what the documents look like now after all the pre-processing
writeLines(as.character(joindisperfectabstract[[1]])) # look at what the documents look like now after all the pre-processing
writeLines(as.character(joindisperfectlocation[[1]])) # look at what the documents look like now after all the pre-processing


# turn the data into structured datasets
x0<-DocumentTermMatrix(joindisperfecttitle) #binary coding
x0 # see how many docs and how many terms in the matrix
inspect(x0[1:10,100:110])


y0<-DocumentTermMatrix(joindisperfectqueries) #binary coding
y0 # see how many docs and how many terms in the matrix
inspect(y0[1:10,100:110])


z0<-DocumentTermMatrix(joindisperfectabstract) #binary coding
z0 # see how many docs and how many terms in the matrix
inspect(z0[1:10,100:110])

w0<-DocumentTermMatrix(joindisperfectlocation) #binary coding
w0 # see how many docs and how many terms in the matrix
inspect(w0[1:10,100:110])

# remove sparse terms reduce number of columns note that the 2nd parameter is arbitrary
x1<-removeSparseTerms(x0,0.9995)
x1
y1<-removeSparseTerms(y0,0.9995)
y1
z1<-removeSparseTerms(z0,0.995)
z1
w1<-removeSparseTerms(w0,0.9999)
w1

# convert the data into a matrix object - some of the below operations require this
x<-as.matrix(x1)
x[1:10]
y<-as.matrix(y1)
y[1:10]
z<-as.matrix(z1)
z[1:10]
w<-as.matrix(w1)
w[1:10]

xcols <- colnames(x)
ycols <- colnames(y)
zcols <- colnames(z)
wcols <- colnames(w)


#Identify the key words

#Title
####

trainrowspredictorx <- vector(mode="integer", nrow(data.frame(xcols)))
c <- 1
for(i in xcols)
{
  trainrowspredictorx[c] <- nrow(data.frame(grep(pattern=i,x=trainingdata$title,ignore.case = TRUE,value=FALSE)))
  c <- c+1
}

zerorowspredictorx <- vector(mode="integer", nrow(data.frame(xcols)))
c <- 1
for(i in xcols)
{
  zerorowspredictorx[c] <- nrow(data.frame(grep(pattern=i,x=zerodata$title,ignore.case = TRUE,value=FALSE)))
  c <- c+1
}

goodwordspredictorx <- vector(mode="numeric", nrow(data.frame(xcols)))
for(i in 1:nrow(data.frame(xcols)))
{goodwordspredictorx[i] <-  zerorowspredictorx[i]/(trainrowspredictorx[i])
}

a1 <- data.frame(goodwordspredictorx, row.names = xcols)
b1 <- subset(a1, goodwordspredictorx>=0.95) #this can be changed
b1 <- t(b1)
titlewords2a<- colnames(b1)[1:200]
titlewords2b<- colnames(b1)[201:400]
titlewords2c<- colnames(b1)[401:length(b1)]

#Queries
####

trainrowspredictory <- vector(mode="integer", nrow(data.frame(xcols)))
c <- 1
for(i in ycols)
{
  trainrowspredictory[c] <- nrow(data.frame(grep(pattern=i,x=trainingdata$queries,ignore.case = TRUE,value=FALSE)))
  c <- c+1
}

zerorowspredictory <- vector(mode="integer", nrow(data.frame(ycols)))
c <- 1
for(i in ycols)
{
  zerorowspredictory[c] <- nrow(data.frame(grep(pattern=i,x=zerodata$queries,ignore.case = TRUE,value=FALSE)))
  c <- c+1
}

goodwordspredictory <- vector(mode="numeric", nrow(data.frame(ycols)))
for(i in 1:nrow(data.frame(ycols)))
{goodwordspredictory[i] <-  zerorowspredictory[i]/(trainrowspredictory[i])
}

a2 <- data.frame(goodwordspredictory, row.names = ycols)
b2 <- subset(a2, goodwordspredictory>=0.95)#this can be changed
b2 <- t(b2)
querywords2a<- colnames(b2)[1:250]
querywords2b<- colnames(b2)[251:length(b2)]

#Abstract
####

trainrowspredictorz <- vector(mode="integer", nrow(data.frame(zcols)))
c <- 1
for(i in zcols)
{
  trainrowspredictorz[c] <- nrow(data.frame(grep(pattern=i,x=trainingdata$abstract,ignore.case = TRUE,value=FALSE)))
  c <- c+1
}

zerorowspredictorz <- vector(mode="integer", nrow(data.frame(zcols)))
c <- 1
for(i in zcols)
{
  zerorowspredictorz[c] <- nrow(data.frame(grep(pattern=i,x=zerodata$abstract,ignore.case = TRUE,value=FALSE)))
  c <- c+1
}

goodwordspredictorz <- vector(mode="numeric", nrow(data.frame(zcols)))
for(i in 1:nrow(data.frame(zcols)))
{goodwordspredictorz[i] <-  zerorowspredictorz[i]/(trainrowspredictorz[i])
}

a3 <- data.frame(goodwordspredictorz, row.names = zcols)
b3 <- subset(a3, goodwordspredictorz>=0.95) #this can be changed
b3 <- t(b3)
abstractwords2a<- colnames(b3)[1:150]
abstractwords2b<- colnames(b3)[151:length(b3)]
#Location
####

trainrowspredictorw <- vector(mode="integer", nrow(data.frame(wcols)))
c <- 1
for(i in wcols)
{
  trainrowspredictorw[c] <- nrow(data.frame(grep(pattern=i,x=trainingdata$location_id,ignore.case = TRUE,value=FALSE)))
  c <- c+1
}

zerorowspredictorw <- vector(mode="integer", nrow(data.frame(wcols)))
c <- 1
for(i in wcols)
{
  zerorowspredictorw[c] <- nrow(data.frame(grep(pattern=i,x=zerodata$location_id,ignore.case = TRUE,value=FALSE)))
  c <- c+1
}

goodwordspredictorw <- vector(mode="numeric", nrow(data.frame(wcols)))
for(i in 1:nrow(data.frame(wcols)))
{goodwordspredictorw[i] <-  zerorowspredictorw[i]/(trainrowspredictorw[i])
}

a4 <- data.frame(goodwordspredictorw, row.names = wcols)
b4 <- subset(a4, goodwordspredictorw>=0.95) #this can be changed
b4 <- t(b4)
locationwords2 <- colnames(b4)

#rm(.............) #remove all the useless intermediate stuff except keyword vectors e.g. abstractwords2a

save.image(file = "preliminarytextmining.RData")
