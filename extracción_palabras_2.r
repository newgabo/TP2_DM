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

tweets_corpus_df <- tweets$find(query='{}', fields='{"retweet_count": "TRUE","followers_count":"TRUE","text": "TRUE"}')
tweets_corpus_df$text_ajustado<-tweets_corpus_df$text


#Discretización de RT_count y followers_count

tweets_corpus_df$retweet_count<-discretize(tweets_corpus_df$retweet_count, method = "fixed", breaks=c(-Inf, 1, 2, 3, 4, +Inf), labels=c("Ninguno", "1 RT","2 RT","3 RT","4 RT"))
colnames(tweets_corpus_df)[3]<-"RT: "
  
tweets_corpus_df$followers_count<-discretize(log(df_tweets_edg$followers_count), breaks = 3, labels=c("pocos", "medio", "muchos"))
colnames(tweets_corpus_df)[4]<-"followers: "

#-----Limpieza de texto

# Limpieza de s?mbolos no alfanum?ricos
tweets_corpus_df$text_ajustado<-gsub("[^[:alnum:][:blank:]?&/\\-]","",tweets_corpus_df$text_ajustado)
tweets_corpus_df$text_ajustado<-gsub("U00..","",tweets_corpus_df$text_ajustado)

# Limpieza de tildes
tweets_corpus_df$text_ajustado<-stri_trans_general(tweets_corpus_df$text_ajustado,"Latin-ASCII")

# Limpieza de may?sculas, n?meros, palabras, espacios en blanco
tweets_corpus_df$text_ajustado<-tolower(tweets_corpus_df$text_ajustado)
tweets_corpus_df$text_ajustado<-removeNumbers(tweets_corpus_df$text_ajustado)
tweets_corpus_df$text_ajustado<-removeWords(tweets_corpus_df$text_ajustado,stopwords("spanish"))
tweets_corpus_df$text_ajustado<-removePunctuation(tweets_corpus_df$text_ajustado)
tweets_corpus_df$text_ajustado<-stripWhitespace(tweets_corpus_df$text_ajustado)


#### PALABRAS POR COLUMNAS

# Calculo las palabras m?s frecuentes
nro_palabras<-30

# data frame de las palabras m?s utilizadas
freq_palabras<-termFreq(tweets_corpus_df$text_ajustado)
tabla_palabras<-data.frame("Palabras"=names(freq_palabras),"Frecuencia"=freq_palabras)
tabla_palabras<-tabla_palabras[order(-tabla_palabras$Frecuencia),]

# selecci?n de palabras m?s utilizadas y su frecuencia
tabla_palabras_reducida<-tabla_palabras[1:nro_palabras,1:2]


# se agrega tabla de verdad por cada palabra seleccioanda al data frame original
for (i in 1:nro_palabras) {
  palabra<-as.character(tabla_palabras_reducida$Palabras[i])
  tweets_corpus_df<-cbind(tweets_corpus_df,str_detect(tweets_corpus_df$text_ajustado,palabra))
  colnames(tweets_corpus_df)[5+i]<-paste0("termino: ",palabra)
}

# Solo me qued? con el id del tweet, los RT, los followers y todas las palabras (exclu?das "cuarentena", "covid" y "coronavirus")
df_asoc<-tweets_corpus_df[,-c(2,5:8)]


#calculo las reglas de asociaci?n
rules_c<-apriori(df_asoc,parameter =list(target="rule", support=0.001, confidence=0.2,minlen=1,maxlen=6))

View(arules::inspect(sort(rules_c, by="lift", decreasing = TRUE)))

rules_c_filter<-arules::subset(rules_c,subset=((support > 0.01) &(rhs %in% "termino: casos")))

rules_c_filter<-arules::subset(rules_c,subset=((support > 0.01) & (rhs %in% "termino: virus")))

rules_c_filter<-arules::subset(rules_c,subset=((lift>5) & (confidence > 0.9) & (support > 0.003) & (rhs %in% "termino: contagios")))

rules_c_filter<-arules::subset(rules_c,subset=((rhs %in% "termino: presidente")))

rules_c_filter<-arules::subset(rules_c,subset=((rhs %in% "termino: pandemia")))

View(arules::inspect(head(rules_c_filter, 20)))


item_freq<-apriori(df_asoc, parameter = list(target="frequent itemsets", support = 0.01))

##  PALABRAS POR FILAS

# Se eliminan las palabras "coronavirus","covid","cuarentena"
tweets_corpus_df$text_ajustado<-removeWords(tweets_corpus_df$text_ajustado,c("coronavirus","covid","cuarentena"))


## Tokenizo cada texto de tweet para anlizar las palabras por fila
token_palabras<-data.frame("ID"=tweets_corpus_df$`_id`)
token_palabras$palabras=tokenize_words(tweets_corpus_df$text_ajustado, simplify = T)
token_palabras = token_palabras %>% select("ID", "palabras")  %>% unnest(palabras) %>%  distinct()
token_palabras$item<-paste0("termino: ",token_palabras$palabras)
transacciones<-as(split(token_palabras$item, token_palabras$ID),"transactions")


## PAra evaluar items frecuentes
##items<-apriori(transacciones,parameter = list(target="frequent itemset",support=0.01))

rules_f<-apriori(transacciones,parameter = list(target="rule", support=0.007, confidence=0.2,minlen=7,maxlen=10))

View(arules::inspect(sort(rules_f, by=c("lift","confidence"), decreasing = TRUE)))

rules_f_filter<-arules::subset(rules_f, subset=(lift>94))

rules_f_filter<-arules::subset(rules_f, subset=( (coverage>0.0075) & (confidence>0.99) & (rhs %in% "termino: dificil")))

View(arules::inspect(head(rules_f_filter, 40)))
