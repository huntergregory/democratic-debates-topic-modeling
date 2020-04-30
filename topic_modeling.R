# Load packages
library(ggplot2)
library(reshape2)
library(LDAvis)
library(lda) # cora dataset
library(servr)
theme_set(theme_bw())

# data(cora.documents) #corpus data (in bag-of-words representation)
# data(cora.vocab) #corpus vocabulary

## TESTING
# doc1 = type.convert(matrix(c(c(1, 5), c(0,8)), nrow=2))
# doc2 = type.convert(matrix(c(c(1, 3), c(2, 1)), nrow=2))
# doc3 = type.convert(matrix(c(c(3,1)), nrow=2))
# ourDocs = list(doc1, doc2)
# ourVocab = c('hi', 'test', 'bad', 'same')
# ourVocabDict['hello'] = 10

# setwd('OneDrive - Duke University/Documents/Duke/6th semester/Bayes/project/')
og20 = read.csv("debate_transcripts_v3_2020-02-26.csv")
data20 = read.csv("debates_2020_updated.csv")
dictionaryData = read.csv("dictionary.csv")
ourVocabDict = list()
for (k in 1:nrow(dictionaryData)) {
  word = as.character(dictionaryData[k,'word'])
  ourVocabDict[word] = k
}
ourVocab = names(ourVocabDict)

ourDocs; # TODO set this

K <- 10 ## No. topics
S <- 1000 ## No. iterations
alpha <- 0.1 ## scalar Dirichlet hyperparameter for topic proportions
eta <- 0.1 ## scalar Dirichlet hyperparameter for word proportions
set.seed(8675309)
result <- lda.collapsed.gibbs.sampler(ourDocs, # corpus documents
                                      K,  ## No. topics
                                      ourVocab, # corpus vocabulary
                                      S,  ## No. iterations
                                      alpha, 
                                      eta, 
                                      compute.log.likelihood=TRUE) 

## Visualize fitted model with LDAvis
theta <- t(apply(result$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(result$topics) + eta, 2, function(x) x/sum(x)))
doc.length <- sapply(ourDocs, function(x) sum(x[2, ])) 
term.frequency <- rep(0,length(ourVocab))
for (ii in 1:length(ourDocs)){
  term.frequency[ourDocs[[ii]][1,]+1] <- term.frequency[ourDocs[[ii]][1,]+1] + ourDocs[[ii]][2,]
}

visobj <- list(phi = phi,
               theta = theta,
               doc.length = doc.length,
               vocab = ourVocab,
               term.frequency = term.frequency)
json <- createJSON(phi = visobj$phi, 
                   theta = visobj$theta, 
                   doc.length = visobj$doc.length, 
                   vocab = visobj$vocab, 
                   term.frequency = visobj$term.frequency)
serVis(json)
