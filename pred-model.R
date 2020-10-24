twitter <- readLines(con <- file("C:/Users/user/Documents/DS capstone/Coursera-SwiftKey/en_US/en_US.twitter.txt"), encoding = "UTF-8", skipNul = TRUE)
close(con)
blog <- readLines(con <- file("C:/Users/user/Documents/DS capstone/Coursera-SwiftKey/en_US/en_US.blogs.txt"), encoding = "UTF-8", skipNul = TRUE)
close(con)
con <- file("C:/Users/user/Documents/DS capstone/Coursera-SwiftKey/en_US/en_US.news.txt", "rb")
news <- readLines(con, encoding = "UTF-8")
close(con)
set.seed(123)
nsample <- news[rbinom(length(news)*0.02, length(news), 0.5)]
tsample <- twitter[rbinom(length(twitter)*0.02, length(twitter), 0.5)]
bsample <- blog[rbinom(length(blog)*0.02, length(blog), 0.5)]
samp <- c(nsample, tsample, bsample)
rm(news, twitter , blog, con)
gc()
library(tm)
mycorp <- VCorpus(VectorSource(samp), readerControl = list(language = "en"))
mycorp <- tm_map(mycorp, content_transformer(tolower))
mycorp <- tm_map(mycorp, removeNumbers)
mycorp <- tm_map(mycorp, stripWhitespace)
mycorp <- tm_map(mycorp, removePunctuation)
profane <- c("nigga", "ass", "pussy", "slut", "dick", "asshole")
mycorp <- tm_map(mycorp, removeWords, profane)
library(RWeka)
library(slam)
library(data.table)
unigram <- function(x) NGramTokenizer(x, Weka_control(min=1, max=1))
unidtm <- DocumentTermMatrix(mycorp, control = list(tokenize = unigram))
freqterm <- findFreqTerms(unidtm)
csum <- slam::col_sums(unidtm[,unidtm$dimnames$Terms %in% freqterm])
wordfreq <- data.frame(word = freqterm, Freq = csum)
wordfreq <- wordfreq[order(wordfreq$Freq, decreasing = T),]
bigram <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
bidtm <- DocumentTermMatrix(mycorp, control = list(tokenize = bigram))
freqterm2 <- findFreqTerms(bidtm)
csum2 <- slam::col_sums(bidtm[,bidtm$dimnames$Terms %in% freqterm2])
wordfreq2 <- data.frame(word = freqterm2, Freq = csum2)
wordfreq2 <- wordfreq2[order(wordfreq2$Freq, decreasing = T),]
trigram <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
tridtm <- DocumentTermMatrix(mycorp, control = list(tokenize = trigram))
freqterm3 <- findFreqTerms(tridtm)
csum3 <- slam::col_sums(tridtm[,tridtm$dimnames$Terms %in% freqterm3])
wordfreq3 <- data.frame(word = freqterm3, Freq = csum3)
wordfreq3 <- wordfreq3[order(wordfreq3$Freq, decreasing = T),]
rm(mycorp, unidtm, bidtm, tridtm)
rm(csum, csum2, csum3)
rm(freqterm, freqterm2, freqterm3)
gc()
wordfreq <- data.table(wordfreq)
wordfreq2 <- data.table(wordfreq2)
wordfreq3 <- data.table(wordfreq3)
wordfreq2[,word := gsub(" ","_",wordfreq2$word) ]
wordfreq3[, word := gsub(" ", "_",wordfreq3$word)]
Obstrig <- function(Pre  , trig = wordfreq3)
{output <- data.frame(word = vector(mode = "character", length = 0), Freq = vector(mode = "integer", length = 0))
 prefix <- paste0("^", Pre, "_")
 index <- grep(prefix, trig$word, ignore.case =T)
 output <- trig[index,]
 return(output)
}
Obstrigprob <- function(obstrig , bigr = wordfreq2, Pre , disc= 0.5)
{if(nrow(obstrig)<1) return(NULL)
  Bicount <- bigr[(bigr$word == Pre),]$Freq
  obtrigprob <- (obstrig$Freq-disc)/Bicount
  output <- data.frame(word = obstrig$word, prob = obtrigprob)
  return(output)}
getUnobservedTrigs <- function(ObservedTrigs , unigs= wordfreq){
  observedlast <- sapply(ObservedTrigs$word, FUN= function(y) paste(strsplit(as.character(y), "_")[[1]][3]))
  output <- unigs[!(unigs$word %in% observedlast),]$word
  return(output)
}
getAlphaBi <- function(Pre , bigrs=wordfreq2, disc=0.5){
  w_i_1 <- strsplit(Pre, "_")[[1]][2]
  w_i_1 <- wordfreq[wordfreq$word==w_i_1,]
  bigramcount <- bigrs[grep(paste0("^",w_i_1$word,"_"), bigrs$word),]
  if (nrow(bigramcount)<1) return(1)
  output <- 1- sum((bigramcount$Freq-disc)/w_i_1$Freq)
  return(output)
}
getBObigrams <- function(Pre , UnobservedTrig ){
  w_i_1 <- strsplit(Pre, "_")[[1]][2]
  output <- paste0(w_i_1,"_" ,UnobservedTrig)
  return(output)
}
getObsBOBigrams <- function(bigrs=wordfreq2, BObigrams){
  output <- bigrs[bigrs$word %in% BObigrams,]
  return(output)
}
getUnObsBOBigrams <- function(bigrs=wordfreq2, BObigrams, ObsBOBigrams){
  output <- BObigrams[!(BObigrams %in% ObsBOBigrams$word)]
  return(output)
}
getObsBOBigramsProbs <- function(Pre , ObsBOBigrams, unigs=wordfreq, disc =0.5){
  w_i_1 <- strsplit(Pre, "_")[[1]][2]
  w_i_1 <- unigs[unigs$word == w_i_1,]
  output <- (ObsBOBigrams$Freq-disc)/w_i_1$Freq
  output <- data.frame(word=ObsBOBigrams$word, prob=output)
  return(output)
}
getUnObsBOBigramsProbs <- function(UnObsBOBigrams, unigs=wordfreq, AlphaBigr){
  UnObsBOBigramsTails <- sapply(UnObsBOBigrams, FUN= function(y) paste(tail(strsplit(as.character(y), "_")[[1]],1)))
  UnObsBOBigramsTails <- unigs[unigs$word %in% UnObsBOBigramsTails,]
  denom <- sum(UnObsBOBigramsTails$Freq)
  output <- data.frame(word=UnObsBOBigrams, prob=(AlphaBigr*UnObsBOBigramsTails$Freq/denom))
  return(output)
}
getalphatri <- function(obstrigprob){
  alphatri <- 1 - sum(obstrigprob$prob)
  return(alphatri)
}
getUnObsTrigramProbs <- function(Pre, QboBigrams, AlphaTrig){
  sumQboBigrams <- sum(QboBigrams$prob)
  UnObsTrigrams <- paste(strsplit(Pre, "_")[[1]][1], QboBigrams$word, sep="_")
  output <- AlphaTrig*QboBigrams$prob/sumQboBigrams
  output <- data.frame(word=UnObsTrigrams, prob=output)
  return(output)
}
getNextWord <- function(ObservedTrigramProb , UnObsTrigramProb){
  QboTrigrams <- rbind(ObservedTrigramProb, UnObsTrigramProb)
  QboTrigrams <- QboTrigrams[order(-QboTrigrams$prob),]
  QboTrigrams$word <- sapply(QboTrigrams$word, FUN= function(y) paste(tail(strsplit(as.character(y), "_")[[1]],1)))
  output <- QboTrigrams[1:3,]
  return(output)
}
predictNextWord <- function(gamma, bigPre, unigs, bigrs,trigs){
  ObservedTrig <- Obstrig(bigPre, trigs)
  ObservedTrigramProb <- Obstrigprob(ObservedTrig,bigrs, bigPre,gamma)
  UnobservedTrigs <- getUnobservedTrigs(ObservedTrig, unigs)
  AlphaBigr <- getAlphaBi(bigPre, bigrs, gamma)
  BObigrams <- getBObigrams(bigPre,UnobservedTrigs)
  ObsBOBigrams <- getObsBOBigrams(bigrs, BObigrams)
  UnObsBOBigrams <- getUnObsBOBigrams(bigrs=bigrs, BObigrams=BObigrams, ObsBOBigrams=ObsBOBigrams)
  ObsBOBigramsProbs <- getObsBOBigramsProbs(bigPre, ObsBOBigrams, unigs, gamma)
  UnObsBOBigramsProbs <- getUnObsBOBigramsProbs(UnObsBOBigrams=UnObsBOBigrams, unigs=unigs, AlphaBigr=AlphaBigr)
  QboBigrams <- rbind(ObsBOBigramsProbs, UnObsBOBigramsProbs)
  AlphaTrig <- getalphatri(ObservedTrigramProb)
  UnObsTrigramProbs <- getUnObsTrigramProbs(bigPre, QboBigrams, AlphaTrig)
  output <- getNextWord(ObservedTrigramProb, UnObsTrigramProbs)
  return(output)
}