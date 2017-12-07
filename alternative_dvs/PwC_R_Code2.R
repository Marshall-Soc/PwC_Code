### "Paying with Change," Alternative DVs ###

# Marshall A. Taylor #
# Dustin S. Stoltz #

##############################
##  PwC_R_Code2.R: Alternative DVs for "Paying with Change" article
##  Note: The paper uses a (square root transformed) absolute polarity measure
    #as an indicator of media sentiment (see the methods section of the paper 
    #for justifications). As a robustness check, I also re-ran the analyses with
    #absolute polarity scores from different R packages. In short, the results
    #hold. To run this script from scratch, you'll need to download the "PwC_Docs"
    #folder, which is nested with the outer "Data" folder in the repository. There 
    #are also some functions in here for calculating subjectivity scores.
##  Author: Marshall A. Taylor
##############################

# Necesssary packages.
library(tm)
library(sentimentr)
library(qdap)
library(tidytext)

############################
##  COMMANDS BEGIN HERE   ##
############################

# Loading in .RData file.
load("PwC_R_Data2.RData")

# Loading in corpus.
docs <- file.path("/Users/marshalltaylor/Box Sync/Coins as Protest/PwC_Docs") 
dir(docs)
corp <- Corpus(DirSource(docs))
summary(corp)

# Cleaning data. See notes for each sentiment function about which preprocessing functions to run per 
  # analysis.
corp.lower <- tm_map(corp, content_transformer(tolower))

corp.all <- tm_map(corp, stripWhitespace)
corp.all <- tm_map(corp, content_transformer(tolower))
corp.all <- tm_map(corp, removeNumbers)
corp.all <- tm_map(corp, removePunctuation)
corp.all <- tm_map(corp, removeWords, c(stopwords("english")))

# Polarity from sentimentr. Preprocessing: Lowercasing only.
sent <- get_sentences(corp.lower$content)
pol <- sentiment_by(sent, by=NULL)
pol <- as.data.frame(pol)
freq2 <- as.data.frame(freq)
pol$Doc_ID <- rownames(freq2)
write.csv(pol, file="pol.csv")

# Polarity from qdap. Preprocessing: Lowercasing only.
pol.qdap <- polarity(corp.lower$content)
pos.qdap <- pol.qdap$all$pos.words
neg.qdap <- pol.qdap$all$neg.words
df <- data.frame(matrix(ncol = 3, nrow = 280))
rownames(df) <- rownames(freq2)
colnames(df) <- c("pos","neg","wc")
for (i in 1:280){
  df[i,1] <- margin.table(table(pos.qdap[i]))
}
for (i in 1:280){
  df[i,2] <- margin.table(table(neg.qdap[i]))
}
for (i in 1:280){
  df[i,3] <- freq[i]
}

subj.qdap <- (df$pos+df$neg)/df$wc
df$subjqdap <- subj.qdap
df$polqdap <- pol.qdap$all$polarity

write.csv(df, file="subjq.csv")

# Polarity from tidytext. Preprocessing: Everything except for stemming.
nrc <- get_sentiments("nrc")
pos_nrc <- as.data.frame(nrc[which(nrc$sentiment=="positive"),])
neg_nrc <- as.data.frame(nrc[which(nrc$sentiment=="negative"),])
pos.nrc.n <- as.matrix(DocumentTermMatrix(corp.all, list(dictionary=pos_nrc$word)))
neg.nrc.n <- as.matrix(DocumentTermMatrix(corp.all, list(dictionary=neg_nrc$word)))

pos.nrc.sum <- rowSums(pos.nrc.n, na.rm=F)
neg.nrc.sum <- rowSums(neg.nrc.n, na.rm=F)

pos.nrc.total <- pos.nrc.sum/freq #Note that I am dividing by the doc word count post 
                                    #top word removal and stemming.
neg.nrc.total <- neg.nrc.sum/freq #Same here
subj.nrc.total <- (pos.nrc.sum+neg.nrc.sum)/freq
pol.nrc.total <- (pos.nrc.sum-neg.nrc.sum)/(pos.nrc.sum+neg.nrc.sum)

tidysent <- cbind(pos.nrc.total,neg.nrc.total,subj.nrc.total, 
                  pol.nrc.total)
write.csv(tidysent,file="tidysent.csv")

# These csv files were then imported into Stata for comparison with the models in the paper. Please see
  # "PwC_Stata_Code2.do."