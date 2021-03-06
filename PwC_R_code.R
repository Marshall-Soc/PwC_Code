### "Paying with Change" Article Stata Code ###
  
  # Marshall A. Taylor #
  # Dustin S. Stoltz #

##############################
##  PwC_R_Code.R: "Paying with Change" Article State Code
##  Note: Sentiment variables computed using tm.plugin.sentiment R package. The
      #event clustering variables computed using Stanford named entity 
      #recognition tool in Bash. "Payor" is "Instigator" in the text; "Payee"
      #is "Target" in the text.
##  Author: Marshall A. Taylor
##############################

# Necesssary packages.
library(NLP)
library(tm)
library(SnowballC)
library(quanteda)
library(readtext)
library(devtools)
library(tm.plugin.sentiment)
library(rex)
library(stringr)
library(LambertW)

############################
##  COMMANDS BEGIN HERE   ##
############################

# Loading in .RData file.
load("PwC_R_Data.RData")

# Loading in corpus.
docs <- file.path("/file/path/here/to/PwC_docs") #You will need to unzip this from the "Data" folder.
dir(docs)
corp <- Corpus(DirSource(docs))
summary(corp)
writeLines(as.character(corp[[203]]))

# Polarity and subjectivity scores, emotionality word counts and freqs.
stop <- function(x) removeWords(x, stopwords("english"))
control <- list(removePunctuation, removeNumbers, content_transformer(tolower),
                stop, stripWhitespace, stemDocument)
corp <- tm_map(corp, FUN=tm_reduce, tmFuns=control)
tdm <- TermDocumentMatrix(corp)
freq <- colSums(as.matrix(tdm))
as.matrix(freq)

positive <- as.matrix(TermDocumentMatrix(corp, list(dictionary=dic_gi$positive)))
negative <- as.matrix(TermDocumentMatrix(corp, list(dictionary=dic_gi$negative)))
write.csv(positive, file="positive.csv")
write.csv(negative, file="negative.csv")

possum <- colSums(positive, na.rm=F)
negsum <- colSums(negative, na.rm=F)
diff <- possum - negsum
sum <- possum + negsum
polarity <- diff/sum
subjecivity <- sum/freq
  ##The below CSV files were exported into Stata for analysis with covariates.
write.csv(polarity, file="polarity.csv")
write.csv(subjectivity, file="subjectivity.csv")
write.csv(possum, file="pos.csv")
write.csv(negsum, file="neg.csv")
write.csv(freq, file="freq.csv")

# Getting t-d matrix with POS tags for coding. See Bash POS code to see how the docs were person-tagged.
tags <- file.path("/file/path/to/stanford-ner-2015-12-09/tagged_docs/")
tags2 <- Corpus(DirSource(tags))
toString <- content_transformer(function(x, from, to) gsub(from, to, x))
tags2 <- tm_map(tags2, toString, "/", "")
tags2 <- DocumentTermMatrix(tags2, control=list(tolower=F))
tags2 <- as.matrix(tags2)
write.csv(tags2, file="tags.csv")
person_tags <- read.csv(file="p_tags.csv")
  ##I took "tags.csv" and used some Excel magic to create a matrix consisting only of person tags. I then used 
    ##this matrix to sort through the documents and identify which stories were talking 
    ##about the same people and therefore the same protest event. This is reflected in the "Event" variable
    ##in the Stata dataset. The person-tag matrix is labeled "p_tag.csv" and can be found in the Data folder.

# Making Poetics Bar chart.
margins <- read.csv("margins.csv", row.names=1)
  ##"margins.csv" is simply a spreadsheet with the adjusted predictions from line #51 in the Stata do-file.

margins <- within(margins, {
  cat <- factor(cat,
                levels=c(1,2,3,4),
                labels=c("Less P, Less L","Less P, More L","More P, Less L","More P, More L"))
})

ggplot(data=margins, aes(x=cat, y=level, fill=cat))+
  geom_bar(stat="identity", colour="black", size=.5, position="dodge", width=.9)+
  xlab("") + ylab("Predicted Media Sentiment")+
  theme_classic()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title=element_blank())+
  scale_fill_manual(values=c("#ffb3ba","#ffdfba","#baffc9","#bae1ff"))+
  coord_flip()
