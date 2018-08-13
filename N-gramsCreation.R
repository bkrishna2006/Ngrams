
myScriptsDir <- "D:/DataScienceCapstoneR/Scripts"
myRawDataDir <- "D:/DataScienceCapstoneR/RawData"
myRefDataDir <- "D:/DataScienceCapstoneR/RefData"
mySampleDataDir <- "D:/DataScienceCapstoneR/SampleData"
myGenDataDir <- "D://DataScienceCapstoneR/GenData"

# The corpus should be available
readRDS(paste0(myGenDataDir,"/myNewCorpus.RData"))

library(quanteda)

toks <- tokens(corpus(myNewCorpus))

ngrams <- tokens_ngrams(toks, n = 1:4,concatenator = ' ')
unigrams <- tokens_ngrams(toks,n=1,concatenator = ' ')
bigrams <- tokens_ngrams(toks,n=2,concatenator = ' ')
trigrams <- tokens_ngrams(toks,n=3,concatenator = ' ')
quadgrams <- tokens_ngrams(toks,n=4,concatenator = ' ')


#5. Creating Document feature Matrix
ngram_dfm <- dfm(ngrams, verbose = FALSE)
unigram_dfm <- dfm(unigrams, verbose = FALSE)
bigram_dfm <- dfm(bigrams, verbose = FALSE)
trigram_dfm <- dfm(trigrams, verbose = FALSE)
quadgram_dfm <- dfm(quadgrams, verbose = FALSE)

#5a. Exploratory Analysis

library(ggplot2)

topN <- 10

# unigram frequency plot
topunigramVector <- topfeatures(unigram_dfm, topN)
topunigramVector <- sort(topunigramVector, decreasing = FALSE)
topunigramDf <- data.frame(words = names(topunigramVector), freq = topunigramVector)
topunigramPlot <- 
  ggplot(data = topunigramDf, 
         aes(x = factor(words, levels = words), y = freq, fill = words)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  #      theme_minimal() +
  labs(x = "unigram", y = "Frequency") +
  labs(title = "unigram Frequencies") +
  coord_flip() +
  guides(fill=FALSE) 
plot(topunigramPlot)

# bigram frequency plot
topbigramVector <- topfeatures(bigram_dfm, topN)
topbigramVector <- sort(topbigramVector, decreasing = FALSE)
topbigramDf <- data.frame(words = names(topbigramVector), freq = topbigramVector)
topbigramPlot <- 
  ggplot(data = topbigramDf, 
         aes(x = factor(words, levels = words), y = freq, fill = words)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  #      theme_minimal() +
  labs(x = "bigram", y = "Frequency") +
  labs(title = "bigram Frequencies") +
  coord_flip() +
  guides(fill=FALSE) 
plot(topbigramPlot)

# trigram frequency plot
toptrigramVector <- topfeatures(trigram_dfm, topN)
toptrigramVector <- sort(toptrigramVector, decreasing = FALSE)
toptrigramDf <- data.frame(words = names(toptrigramVector), freq = toptrigramVector)
toptrigramPlot <- 
  ggplot(data = toptrigramDf, 
         aes(x = factor(words, levels = words), y = freq, fill = words)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  #      theme_minimal() +
  labs(x = "trigram", y = "Frequency") +
  labs(title = "trigram Frequencies") +
  coord_flip() +
  guides(fill=FALSE) 
plot(toptrigramPlot)

# quadgram frequency plot
topquadgramVector <- topfeatures(quadgram_dfm, topN)
topquadgramVector <- sort(topquadgramVector, decreasing = FALSE)
topquadgramDf <- data.frame(words = names(topquadgramVector), freq = topquadgramVector)
topquadgramPlot <- 
  ggplot(data = topquadgramDf, 
         aes(x = factor(words, levels = words), y = freq, fill = words)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  #      theme_minimal() +
  labs(x = "quadgram", y = "Frequency") +
  labs(title = "quadgram Frequencies") +
  coord_flip() +
  guides(fill=FALSE) 
plot(topquadgramPlot)


# 7. 
topngramDf <- rbind(topunigramDf,topbigramDf,toptrigramDf,topquadgramDf)
topngramDf_sorted <- topngramDf[order(topngramDf$words, -topngramDf$freq),]

rm(topunigramDf,topbigramDf,toptrigramDf,topquadgramDf,
   topunigramVector,topbigramVector,toptrigramVector,topquadgramVector,
   topngramDf)
gc()


# 6 How many unique words do you need in a frequency sorted dictionary to 
# cover 50% of all word instances in the language? 90%?
# Define function to calculate coverage
CoverageCalc <- function (dfm, percent) {
  words <- sort(colSums(dfm), decreasing = TRUE)
  allwords <- length(dfm@i)
  sum = 0
  for(i in 1:allwords)
  {
    sum <- sum + words[[i]]
    if(sum >= (percent * allwords)) break
  }
  i        
}

unigramCov50 <- CoverageCalc(unigram_dfm, 0.5)
unigramCov75 <- CoverageCalc(unigram_dfm, 0.75)
unigramCov90 <- CoverageCalc(unigram_dfm, 0.9)

bigramCov50 <- CoverageCalc(bigram_dfm, 0.5)
bigramCov75 <- CoverageCalc(bigram_dfm, 0.75)
bigramCov90 <- CoverageCalc(bigram_dfm, 0.9)

trigramCov50 <- CoverageCalc(trigram_dfm, 0.5)
trigramCov75 <- CoverageCalc(trigram_dfm, 0.75)
trigramCov90 <- CoverageCalc(trigram_dfm, 0.9)

quadgramCov50 <- CoverageCalc(quadgram_dfm, 0.5)
quadgramCov75 <- CoverageCalc(quadgram_dfm, 0.75)
quadgramCov90 <- CoverageCalc(quadgram_dfm, 0.9)

Coverage_Stat <- matrix(data = c("Type", "50%", "75%", "90%",
                                 "unigram",unigramCov50,unigramCov75,unigramCov90,
                                 "bigram",bigramCov50,bigramCov75,bigramCov90,
                                 "trigram",trigramCov50,trigramCov75,trigramCov90,
                                 "quadgram", quadgramCov50,quadgramCov75,quadgramCov90),
                        nrow = 5,
                        ncol = 4,
                        byrow = TRUE
)
View(Coverage_Stat)

# from Coverage statistics table, it is decided to go with whatever is required for 90% coverage, 
# as we are working on a small sample of 1 % of the data.

# for unigram

unigramVector <- topfeatures(unigram_dfm, unigramCov75)
unigramVector <- sort(unigramVector, decreasing = FALSE)
unigramDf <- data.frame(words = names(unigramVector), freq = unigramVector)
unigramDf_sorted <- unigramDf[order(unigramDf$words, -unigramDf$freq),]

# for Biragram
bigramVector <- topfeatures(bigram_dfm, bigramCov75)
bigramVector <- sort(bigramVector, decreasing = FALSE)
bigramDf <- data.frame(words = names(bigramVector), freq = bigramVector)
bigramDf_sorted <- bigramDf[order(bigramDf$words, -bigramDf$freq),]

# for trigram
trigramVector <- topfeatures(trigram_dfm, trigramCov75)
trigramVector <- sort(trigramVector, decreasing = FALSE)
trigramDf <- data.frame(words = names(trigramVector), freq = trigramVector)
trigramDf_sorted <- trigramDf[order(trigramDf$words, -trigramDf$freq),]

# for Quadragram
quadgramVector <- topfeatures(quadgram_dfm, quadgramCov75)
quadgramVector <- sort(quadgramVector, decreasing = FALSE)
quadgramDf <- data.frame(words = names(quadgramVector), freq = quadgramVector)
quadgramDf_sorted <- quadgramDf[order(quadgramDf$words, -quadgramDf$freq),]

#for ngram
ngramDf <- rbind(unigramDf,bigramDf,trigramDf,quadgramDf)
ngramDf_sorted <- ngramDf[order(ngramDf$words, -ngramDf$freq),]

rm(unigramVector,bigramVector,trigramVector,quadgramVector,
   unigramCov50,bigramCov50,trigramCov50,quadgramCov50,
   unigramCov75,bigramCov75,trigramCov75,quadgramCov75,
   unigramCov90,bigramCov90,trigramCov90,quadgramCov90,
   unigramDf,bigramDf,trigramDf,quadgramDf,ngramDf
)
gc()

# remove Global Environment objects not required anymore 

#rm(myNewCorpus, myNewCorpus_bkup,
#   toks) 

rm(topunigramPlot, topbigramPlot,toptrigramPlot,topquadgramPlot,
   CoverageCalc)
gc()

setwd(myGenDataDir)
neededObjects <- c("unigramDf_sorted","bigramDf_sorted","trigramDf_sorted","quadgramDf_sorted","myNewCorpus")
rm(list=setdiff(ls(),neededObjects))
neededObjects <- c("unigramDf_sorted","bigramDf_sorted","trigramDf_sorted","quadgramDf_sorted","myNewCorpus")

saveRDS(neededObjects,"./Ngrams.RData")
