library(DT)
library(wordcloud2)
library(shinydashboard)
library(shiny)
library(twitteR)
library(plotly)
library(wesanderson)
library(plyr)
library(memoise)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(httpuv)
library(RCurl)
library(tm)
library(stringr)
library(SnowballC)
library(ROAuth)
library(httr)
library(ggthemes)
library(ggplot2)
library(NLP)
library(wordcloud)
library(shinythemes)
#se connecter
consumer_key='ZedHC89vNJWhekl33mF6OIIoJ'
consumer_secret='zkNh64D9GJ3zP4dpMOzVCqp6VmsJVSWIMe5H5N2Ee1NNQUsYNO'
access_token='873076680729829376-FmGHhrjIxYXZTCjDGr1bomPHElWVIYy'
access_secret='esh2i6zMngE3bIJ4k7gu3gDVUg3YnrllxQ1BGTnMd6ULJ'
setup_twitter_oauth(consumer_key, consumer_secret, access_token,access_secret)







#TWeetmining

tweetmine<-function(myword,mycount){
  
  # harvest some tweets
  some_tweets <- searchTwitter(myword, n=mycount, lang="en")
  # get the text
  some_txt <- sapply(some_tweets, function(x) x$getText())
  # remove retweet entities
  some_txt <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  # remove at people
  some_txt <- gsub("@\\w+", "", some_txt)
  # remove punctuation
  some_txt <- gsub("[[:punct:]]", "", some_txt)
  # remove numbers
  some_txt <- gsub("[[:digit:]]", "", some_txt)
  # remove html links
  some_txt <- gsub("http\\w+", "", some_txt)
  # remove unnecessary spaces
  some_txt <- gsub("[ \t]{2,}", "", some_txt)
  some_txt <- gsub("^\\s+|\\s+$", "", some_txt)
  #Removing Line breaks
  some_txt <- gsub("[\r\n]", "", some_txt)
  #Removing non graphical characters so that data can be processed by tm package
  some_txt <- str_replace_all(some_txt,"[^[:graph:]]", " ") 
  return(some_txt)
} ;

tweetclean <- function(x){
  myCorpus <- Corpus(VectorSource(x))
  myCorpus <- tm_map(myCorpus, tolower)
  myCorpus <- tm_map(myCorpus, PlainTextDocument)
  myCorpus <- tm_map(myCorpus, removePunctuation)
  myCorpus <- tm_map(myCorpus, removeNumbers)
  myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))
  myCorpus <- tm_map(myCorpus, stemDocument)
  return(myCorpus)
}



  subject<-c("qatar","brexit")
  
  
  
  




