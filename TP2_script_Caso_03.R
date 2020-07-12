library(mongolite)
library(ggplot2)
library(tidyr)
if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
  detach(package:dplyr, unload=TRUE)
}
library(plyr)
library(dplyr)
library(NLP)
library(tm)
library(stringr)
library(stringi)
library(Matrix)
library(arules)
library(tokenizers)

# Estableciendo conexion con Mongo DB - Covid Collection
tweets <- mongo(url = "mongodb://gabriel:1234@192.168.10.10:27017/admin", db = "DMUBA", collection = "tweets_mongo_covid19")
#tweets <- mongo_tweets$find(limit = 1000 ) # para development se trae una muestra mas pequeña


# ANALISIS 3: 
# -----------
# TID (Carrito): cada uno de los tweets, sin agrupar
# ItemSet (items) : text, el mensaje del tweet

tweets_df <- tweets$find(
  query='{}', 
  fields='{"text":1}', 
  limit = 1000  # sacar el limite 
)

#View(head(tweets_df, n=100))

# Limpieza de símbolos no alfanuméricos
tweets_df$text_clean <- tweets_df$text
tweets_df$text_clean <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", tweets_df$text_clean)
tweets_df$text_clean <- gsub("U00..", "", tweets_df$text_clean)

# Limpieza de tildes
tweets_df$text_clean <- stri_trans_general(tweets_df$text_clean,"Latin-ASCII")

# Limpieza de mayúsculas, números, palabras, espacios en blanco
tweets_df$text_clean <- tolower(tweets_df$text_clean)
tweets_df$text_clean <- removeNumbers(tweets_df$text_clean)
tweets_df$text_clean <- removeWords(tweets_df$text_clean,stopwords("spanish"))
tweets_df$text_clean <- removePunctuation(tweets_df$text_clean)
tweets_df$text_clean <- stripWhitespace(tweets_df$text_clean)

# Se remueven palabras claves sin valor relacionada con los criterios de coleccion de los tweets
tweets_df$text_clean <- lapply(tweets_df$text_clean, 
                      function(x) removeWords(x,c("coronavirus","covid","covid19","covid_19","covid-19","covid 19","cuarentena")))

# Se hace split del string y se remueven los strings vacios
tweets_df$text_clean <- lapply(tweets_df$text_clean, function(x) stri_remove_empty_na((strsplit(x, " "))[[1]]))

# Se converte el DF a Transactions, con las columnas correspondientes
tweets_transaction <- as(tweets_df[,"text_clean"], "transactions")
inspect(head(tweets_transaction, 100))
summary(tweets_transaction)


# Busqueda de reglas de asociación con APRIORI
rules <- apriori(tweets_transaction, parameter =list(target="rules", support=0.001, confidence=0.5, maxlen=10))
rules_subset <- subset(sort(rules, by="lift", decreasing = TRUE), subset = lift > 100 & count > 50)
inspect(rules_subset)
inspect(rules[1:20])

# convert rules to a dataframe and then use View()
rules_df <- as(rules,"data.frame")
View(head(rules_df,50))


library(vctrs)
library(arulesViz)
library(RColorBrewer)
# Frecuencia relativa de items mas frecuentes
itemFrequencyPlot(tweets_transaction, topN = 20,type="relative",col=brewer.pal(8,'Pastel2'), main="Frecuencia relativa por item (retweet_screen_name)")

# Analisis de correlacion
plot(rules_df[c("support","confidence","lift","count")])

# Plot rules by support and confidence
plot(rules, method="two-key plot",  jitter = 0)


# Grafo interactivo de rules associations
subRulesTop <- head(rules_subset, n = 40, by = "confidence")
plot(subRulesTop, method = "graph",  engine = "htmlwidget")

subRulesTop <- head(rules_subset, n=40, by="lift")
plot(subRulesTop, method="paracoord")