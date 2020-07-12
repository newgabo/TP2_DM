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

tweets_corpus_df <- tweets$find(query='{}', fields='{"screen_name": "TRUE", "text": "TRUE"}')

tweets_corpus_df$text_ajustado<-tweets_corpus_df$text


#-----Limpieza de texto

# Limpieza de símbolos no alfanuméricos
tweets_corpus_df$text_ajustado<-gsub("[^[:alnum:][:blank:]?&/\\-]","",tweets_corpus_df$text_ajustado)
tweets_corpus_df$text_ajustado<-gsub("U00..","",tweets_corpus_df$text_ajustado)

# Limpieza de tildes
tweets_corpus_df$text_ajustado<-stri_trans_general(tweets_corpus_df$text_ajustado,"Latin-ASCII")

# Limpieza de mayúsculas, números, palabras, espacios en blanco
tweets_corpus_df$text_ajustado<-tolower(tweets_corpus_df$text_ajustado)
tweets_corpus_df$text_ajustado<-removeNumbers(tweets_corpus_df$text_ajustado)
tweets_corpus_df$text_ajustado<-removeWords(tweets_corpus_df$text_ajustado,stopwords("spanish"))
tweets_corpus_df$text_ajustado<-removePunctuation(tweets_corpus_df$text_ajustado)
tweets_corpus_df$text_ajustado<-stripWhitespace(tweets_corpus_df$text_ajustado)


#### PALABRAS POR COLUMNAS

# Calculo las palabras más frecuentes
nro_palabras<-20

# data frame de las palabras más utilizadas
freq_palabras<-termFreq(tweets_corpus_df$text_ajustado)
tabla_palabras<-data.frame("Palabras"=names(freq_palabras),"Frecuencia"=freq_palabras)
tabla_palabras<-tabla_palabras[order(-tabla_palabras$Frecuencia),]

# selección de palabras más utilizadas y su frecuencia
tabla_palabras_reducida<-tabla_palabras[1:nro_palabras,1:2]


# se agrega tabla de verdad por cada palabra seleccioanda al data frame original
for (i in 1:nro_palabras) {
  palabra<-as.character(tabla_palabras_reducida$Palabras[i])
  tweets_corpus_df<-cbind(tweets_corpus_df,str_detect(tweets_corpus_df$text_ajustado,palabra))
  colnames(tweets_corpus_df)[4+i]<-paste0("termino: ",palabra)
}

# Solo me quedó con el id del tweet y todas las palabras (excluídas "cuarentena", "covid" y "coronavirus")
df_asoc<-tweets_corpus_df[,-c(2:7)]


#calculo las reglas de asociación
rules_c<-apriori(df_asoc,parameter =list(target="rule", support=0.005, confidence=0.2,minlen=1,maxlen=6))

orden_rules_c<-head(arules::inspect(sort(rules_c, by="lift", decreasing = TRUE)),20)

View(orden_rules_c)


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

rules_f<-apriori(transacciones,parameter = list(target="rule", support=0.01, confidence=0.2,minlen=2,maxlen=9))

View(arules::inspect(sort(rules_f, by="lift", decreasing = TRUE)))

View(orden_rules_f)


### falta ordenar por lift