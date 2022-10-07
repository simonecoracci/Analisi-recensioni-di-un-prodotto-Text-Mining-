install.packages("word2vec")
install.packages("udpipe")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("plotrix")
install.packages("sentimentr")

library(word2vec)
library(udpipe)
library(tm)
library(SnowballC)
library(ggplot2)
library(plotly)
library(wordcloud)
library(plotrix)
library(sentimentr)

recensioni <- recensione_pentola_a_pressione_text_mining
#trasformazione recensioni in minuscolo
recensioni_pulito <- tolower(recensioni$`Review Content`)
#trasformazione in corpus
recensioni_pulito <- VCorpus(VectorSource(recensioni_pulito))
summary(recensioni_pulito)
#rimuovere punteggiatura
recensioni_pulito <- tm_map(recensioni_pulito,removePunctuation)
recensioni_pulito[[1]]$content
#rimozione parole d'ordine
recensioni_pulito <- tm_map(recensioni_pulito, removeWords, stopwords("english"))
recensioni_pulito <- tm_map(recensioni_pulito, removeWords, stopwords("italian"))
recensioni_pulito <- tm_map(recensioni_pulito, PlainTextDocument)
#Rimozione di terminazioni di parole comuni, stemming
recensioni_pulito <- tm_map(recensioni_pulito, stemDocument)
recensioni_pulito[[1]]$content
#eliminazione spazi bianchi in eccesso
recensioni_pulito <- tm_map(recensioni_pulito, stripWhitespace)
recensioni_pulito[[1]]$content
#da VCorpus a dataset
recensioni_finale <- data.frame(text=unlist(sapply(recensioni_pulito,'[',"content")))
View(recensioni_finale)
#traformazione da dataset a matrice
recensioni_matrice <- DocumentTermMatrix(recensioni_pulito)
recensioni_matrice
#frequenza delle parole
frequenze <- colSums(as.matrix(recensioni_matrice))
frequenze
#trasformazione frequenze in dataset
df <- data.frame(word = names(frequenze), frequenze = frequenze)
df
#grafico frequenze parole, ho messo (frequenze > 200) perchè senno il grafico viene troppo attaccato
hist_df <- ggplot(subset(df, frequenze > 200), aes(x = reorder(word, -frequenze), y = frequenze)) +
                  geom_bar(stat = "identity") +
                    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))
hist_df

#wordcloud
set.seed(10)
wordcloud(names(frequenze), frequenze, min.freq = 100, max.words = 40)
#pulizia stelle
stelle <- gsub(pattern = "0", replacement = "", recensioni$Rating)
stelle[300]
#tabella stelle
tabellastelle <- table(stelle) 
stelle
#coversione a numero
stelle <- as.numeric(stelle)
stelletab <- table(stelle)
#grafico stelle diagramma a torta
cols <- c("red", "orange", "blue", "violet", "green")
labs1 <- c("160", "39", "41", "74", "286")
labs2 <- c("1 stella", "2 stelle", "3 stelle", "4 stelle", "5 stelle")
pie3D(stelletab, labels = labs1, explode = 0.1, main = "Grafico stelle")
legend(x = 1.2, y = 1, setLab = FALSE, "topright", labs2 , cex = 1, fill = rainbow(length(cols)), bty="n")
#sentiment recensioni
sentiment_recensioni <- sentiment(recensioni$`Review Content`)
qplot(sentiment_recensioni$sentiment, geom = "histogram", binwidth = 0.1, main = "Review Sentiment Histogram")



