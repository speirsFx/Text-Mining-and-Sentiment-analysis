install.packages("tm")
library(tm)

install.packages("slam")
library(slam)

install.packages("topicmodels")
library(topicmodels)

x <-  readLines(file.choose())
x
length(x)

mydata.corpus <- Corpus(VectorSource(x))

mydata.corpus <- tm_map(mydata.corpus, removePunctuation)

my_stopwords <- c(stopwords('English'), "the", "due", "are", "not", "for", 
                  "this", "and")

mydata.corpus <- tm_map(mydata.corpus , removeWords, my_stopwords)

mydata.corpus <- tm_map(mydata.corpus, removeNumbers)

mydata.corpus <- tm_map(mydata.corpus , stripWhitespace)

### Buid a termdocument Matrix
mydata.tdm3 <- TermDocumentMatrix(mydata.corpus)
mydata.tdm3

#document term matrix
dtm <- t(mydata.tdm3)

rowTotals <- apply(dtm , 1, sum)
?apply

dtm.new <- dtm[rowTotals > 0, ]

lda <-LDA(dtm.new, 10)#upto 10 topics
?LDA
lda
term <- terms(lda, 10)
term

tops <-terms(lda)
tb <- table(names(tops), unlist(tops))

tb <- as.data.frame.matrix(tb)
?unlist

#ward is absolute distance
cls <- hclust(dist(tb), method ='ward.D2')
par(family = 'HiraKaKuPron-w3')
plot(cls)


######### #EMOTION MINING #############
install.packages("syuzhet")
library(syuzhet)

my_example_text <- readLines(file.choose())#upload lotr script

s_v <- get_sentences(my_example_text)
class(s_v)
str(s_v)
head(s_v)

sentiment_vector <- get_sentiment(s_v, method = 'bing')
head(sentiment_vector)

affin_s_v <- get_sentiment(s_v, method = 'afinn')
head(affin_s_v)


nrc_vector <- get_sentiment(s_v, method = 'nrc')
head(nrc_vector)


sum(sentiment_vector)
mean(sentiment_vector)
summary(sentiment_vector)

# plot
plot(sentiment_vector, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red")

#To extract the sentence with the most negative emotional valence
negative <- s_v[which.min(sentiment_vector)]
negative

#and to extract with the most positive emotional valence
positive <- s_v[which.max(sentiment_vector)]
positive

#more depth
poa_v <- my_example_text
poa_sent <- get_sentiment(poa_v, method = "bing")
plot(
  poa_sent,
  type = "h",
  main = "Example Plot Trajectory",
  xlab = "Narrative Time",
  ylab = "Emotional valence",
)
?plot
#percentages based figures
percent_vals <- get_percentage_values(poa_sent)

plot(
  percent_vals,
  type = 'l',
  main = "Throw the ring in the valcono using Percentage-Based Means",
  xlab = "Narrative Time",
  ylab = "Emotional Valence",
  color = 'red'
)

ft_values <- get_transformed_values(
  poa_sent,
  low_pass_size = 3,
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)

plot(
  ft_values,
  type = 'h',
  main = 'LOTR USING TRANSFORMED VALUES',
  xlab = "Narrative Time",
  ylab = "Emotional Valence",
  col = "red"
)

#categorize each sentence by eight emotions
library(dplyr)
nrc_data <- get_nrc_sentiment(head(s_v,100))
nrc_score_sent <- get_nrc_sentiment(negative)
nrc_score_word <- get_nrc_sentiment("grim")

#subset
sad_items <- which(nrc_data$sadness > 0)
head(s_v[sad_items])

#View emotions as a barplot
barplot(sort(colSums(prop.table(nrc_data[, 1:8]))), horiz = 'T', cex.names = 0.7,
        last = 1,main = "Emotions", xlab = "Percentage", 
        col = 1:8)

##########
#NAMED ENTITY RECOGNISATION
########### Structured Data Extrcation(NER) ########
install.packages("rJava")
install.packages("NLP")
install.packages("openNLP")
install.packages("RWeka")
install.packages("magrittr")

library(rJava)
library(NLP)
library(openNLP)
library(RWeka)
library(magrittr)

install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/" , 
                 type = "source")
library(openNLPmodels.en)

bio <- readLines(file.choose()) #Use Modi Speech Text
bio <- paste(bio ,collapse =  " ")
bio <-  as.String(bio)

word_ann <- Maxent_Word_Token_Annotator()
sent_Ann <- Maxent_Sent_Token_Annotator()
bio_annotations <- annotate(bio, list(sent_Ann, word_ann))

class(bio_annotations)
head(bio_annotations)
bio_doc <-  AnnotatedPlainTextDocument(bio, bio_annotations)
sents(bio_doc) %>% head(2)
words(bio_doc) %>% head(10)

person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organistaion_ann <- Maxent_Entity_Annotator(kind = "organization")

pipeline <- list(sent_Ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organistaion_ann)

bio_annotations <- annotate(bio, pipeline)
bio_doc <- AnnotatedPlainTextDocument(bio, bio_annotations)

entities <- function(doc, kind){
  s <- doc$content
  a <- annotation(doc)
  if(hasArg(kind)){
    k <- sapply(a$features, '[[', "kind")
    s[a[k == kind]]
  }
  else{
    s[a[a$type == "entity"]]
  }
}

entities(bio_doc, kind = "person")

entities(bio_doc, kind = "location")

entities(bio_doc, kind = "organization")
