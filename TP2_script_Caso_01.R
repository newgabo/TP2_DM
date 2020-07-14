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


# ANALISIS 1: 
# -----------
# TID (Carrito): screen_name (user id del tweet)
# ItemSet (items) : retweet_screen_name (del usuario retweeted)

## Se buscan los campos: user_id, screen_name, retweet_user_id, retweet_screen_name
## Se filtran solo los tweets que son retweets
tweets_df <- tweets$find(
  query='{"retweet_user_id": {"$exists": true, "$ne": null}}', 
  fields='{"user_id":1, "screen_name":1, "retweet_user_id":1, "retweet_screen_name":1}', 
  #limit = 10000  # sacar el limite 
)

#View(head(tweets_df, n=100))

# Se converte el DF a Transactions, con las columnas correspondientes
tweets_transaction <- as(split(tweets_df[,"retweet_screen_name"], tweets_df[,"screen_name"]), "transactions")
arules::inspect(head(tweets_transaction, 100))
summary(tweets_transaction)


# Busqueda de reglas de asociación con APRIORI
rules <- apriori(tweets_transaction, parameter =list(target="rules", support=0.0001, confidence=0.5, maxlen=5))
rules_subset <- subset(sort(rules, by="lift", decreasing = TRUE), subset = lift > 100 & count > 50)
arules::inspect(rules_subset)
arules::inspect(rules[1:10])

# convert rules to a dataframe and then use View()
rules_df <- as(rules,"data.frame")
View(head(rules_df))


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
subRulesTop <- head(rules, n = 40, by = "confidence")
plot(subRulesTop, method = "graph",  engine = "htmlwidget")


subRulesTop <- head(rules, n=40, by="lift")
plot(subRulesTop, method="paracoord")