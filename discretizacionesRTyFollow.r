library(mongolite)
library(ggplot2)
library(tidyr)
library(dplyr)
library(NLP)
library(tm)
library(stringr)
library(stringi)
library(Matrix)
library(arules)
library(tokenizers)


tweets = mongo(collection = "tweets_mongo_covid19", db = "DMUBA" )

cant_tweets <- tweets$find(query='{}', fields='{"retweet_count": "TRUE", "favorite_count": "TRUE", "followers_count":"TRUE","friends_count":"TRUE"}')

cant_tweets$cant_RT<-discretize(cant_tweets$retweet_count, method = "fixed", breaks=c(-Inf, 1, 2, 3, 4, +Inf), labels=c("Ninguno", "1 RT","2 RT","3 RT","4 RT"))
cant_tweets$cant_followers<-discretize(log10(cant_tweets$followers_count), method = "fixed", breaks=c(-Inf, 2, 4,+Inf),labels=c("Pocos","Nivel medio","Muchos") )

