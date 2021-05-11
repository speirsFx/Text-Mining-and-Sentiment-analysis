#Importing reviews dataset "McDonalds-Yelp-Sentiment-DFE"
txt <- read.csv(file.choose(), header= TRUE)


str(txt)
length(txt)


x <- as.character(txt$review)
length(x)

#Corpus
library(tm)

x <- Corpus(VectorSource(x))

inspect(x[1])
inspect(x[length(x)])

#Data Cleansing

x1 <- tm_map(x , tolower)
inspect(x1[1])

x1 <- tm_map(x1 , removePunctuation)

inspect(x1[1])
inspect(x1[length(x)])
 
x1 <-tm_map(x1 , removeNumbers)
inspect(x1[1])
inspect(x1[length(x)])

x1 <- tm_map(x1 , removeWords ,stopwords('english'))
inspect(x1[1])

#stripping whitespaces

x1 <- tm_map(x1 , stripWhitespace)
inspect(x1[1])

#Term Document Matrix(tdm)
#Converting unstructured data into structured format using this type

tdm <- TermDocumentMatrix(x1)
tdm
dtm <- t(tdm)


tdm <- as.matrix(tdm)

tdm[100:119 , 1:20]

#Read the 3rd review
inspect(x[3])

#Bar Plot

w <- rowSums(tdm)
w

w_sub <- subset(w , w >=40)
w_sub

barplot(w_sub ,las = 2, col = rainbow(30))

#term floor repeats in all documents

#x1 <- tm_map(x1, removeWords , "floor")
x1 <- tm_map(x1, removeWords , "ive")
x1 <- tm_map(x1,  stripWhitespace)

tdm <- TermDocumentMatrix(x1)
tdm

tdm <- as.matrix(tdm)
tdm
tdm[1:10,1:10]

#WordCloud

install.packages("wordcloud")
library(wordcloud)

w_sub1 <- sort(rowSums(tdm) , decreasing = TRUE)
w_sub1[w_sub1 > 80]

#wordcloud with only subset of words
#I did subset as my pc took very much time to execute it & it also requires 
#RAM >= 18GB  for executing it
#wordcloud(words = names(w_sub[w_sub > 80]), freq = w_sub[w_sub > 80]) 

#all words are considered
wordcloud(words = names(w_sub1), freq = w_sub1)

wordcloud(words = names(w_sub) , freq = w_sub, random_order = F, colors = rainbow(30)
  , scale = c(5, 1), rot.per = 0.3)



### ........ ###

#Emotion mining

install.packages("syuzhet")
install.packages("ggplot")


library("syuzhet")
library(lubridate)
library(dbplyr)
library(scales)
library(ggplot2)
library(reshape2)

txt <- readLines(file.choose()) #upload reviews of nokia lumia
txt <- iconv(txt, "UTF-8")

s <- get_nrc_sentiment(txt)
head(s)
tail(s)

txt[4]
get_nrc_sentiment('excellent')

#Bar plot for emotion mining
barplot(colSums(s) , las = 2, col = rainbow(10), ylab = 'Count',
        main = 'Emotion Scores')

