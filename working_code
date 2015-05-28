## Marshall A. Taylor and Dustin S. Stoltz ##

# Small Money Project

# Necessary Packages
library(NLP)
library(tm)
library(RColorBrewer)
library(SnowballC)
library(ggplot2)
library(graph)
library(grid)
library(Rgraphviz)
library(pheatmap)
library(RWeka)

# Feeding in documents.
documents <- file.path("file path here")
length(dir(documents))
dir(documents)

# Creating and inspecting corpus.
Money_Corp <- Corpus(DirSource(documents))
class(Money_Corp)
summary(Money_Corp)
inspect(Money_Corp[11])

# Pre-processing.
toString <- content_transformer(function(x, from, to) gsub(from, to, x))
Money_Corp <- tm_map(Money_Corp, stripWhitespace)
Money_Corp <- tm_map(Money_Corp, content_transformer(tolower))
Money_Corp <- tm_map(Money_Corp, toString, "word1", "word2")
Money_Corp <- tm_map(Money_Corp, removeNumbers)
Money_Corp <- tm_map(Money_Corp, removePunctuation)
Money_Corp <- tm_map(Money_Corp, removeWords, stopwords("english"))
Money_Corp <- tm_map(Money-Corp, stemDocument)

# Document-term matrix.
Money_dtm <- DocumentTermMatrix(Money_Corp)
inspect(Money_dtm)
summary(Money_dtm)
MoneyF <- sort(colSums(as.matrix(Money_dtm)), decreasing=T)
head(MoneyF, 10)
tail(MoneyF, 10)
MoneyW <- data.frame(word=names(MoneyF), freq=MoneyF)
head(MoneyW)
tail(MoneyW)
MoneyF2 <- sort(colSums(as.matrix(Money_dtm)), decreasing=T)
MoneyW2 <- data.frame(document=names(MoneyF2), freq=MoneyF2)
M <- as.matrix(Money_dtm)
write.csv(M, file="Money_dtm.csv")
write.csv(MoneyW, file="MoneyW.csv")
write.csv(MoneyW2, file="MoneyW_bydoc.csv")

# Some semantic correlations.
plot(Money_dtm, terms=findFreqTerms(Money_dtm, lowfreq=100) [1:15], corThreshold=.8)

# Bigrams.
options(mc.cores=1)
BiGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
Money_tdm <- TermDocumentMatrix(Money_Corp, control = list(tokenize = BiGramTokenizer))
M2 <- as.matrix(Money_tdm)
Money_NGRAMF <- sort(rowSums(as.matrix(Money_tdm)), decreasing=T)
Money_NGRAMW <- data.frame(bigrams=names(Money_NGRAMF), freq=Money_NGRAMF)
write.csv(M2, file="Money_Bigrams.csv")
write.csv(Money_NGRAMW, file="Money_Bigrams_byfreq.csv")
plot(Rio_tdm, terms=findFreqTerms(Money_tdm, lowfreq=50) [1:15], corThreshold=0.6)

# Trigrams.
TriGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
Money_tdm2 <- TermDocumentMatrix(Money_Corp, 
                              control = list(tokenize = TriGramTokenizer))
M3 <- as.matrix(Money_tdm2)
Money_NGRAMF2 <- sort(rowSums(as.matrix(Money_tdm2)), decreasing=T)
Money_NGRAMW2 <- data.frame(trigrams=names(Money_NGRAMF2), freq=Money_NGRAMF2)
write.csv(M3, file="Money_Trigrams.csv")
write.csv(Money_NGRAMW2, file="Money_Trigrams_byfreq.csv")
plot(Money_TDM2, terms=findFreqTerms(Money_tdm2, lowfreq=5) [1:15], corThreshold=0.5)
