# Load packages
library(ggplot2)
library(reshape2)
library(LDAvis)
library(servr)

debateData = read.csv("debate_transcripts_v3_2020-02-26.csv")
documents = # TODO
vocab = # TODO

theme_set(theme_bw())

set.seed(8675309)

K <- 10 ## No. topics
S <- 1000 ## No. iterations
alpha <- 0.1 ## scalar Dirichlet hyperparameter for topic proportions
eta <- 0.1 ## scalar Dirichlet hyperparameter for word proportions

# Run Gibbs sampler
result <- lda.collapsed.gibbs.sampler(documents, # corpus documents
                                      K,  ## No. topics
                                      vocab, # corpus vocabulary
                                      S,  ## No. iterations
                                      alpha, 
                                      eta, 
                                      compute.log.likelihood=TRUE) 

## Visualize fitted model with LDAvis
theta <- t(apply(result$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(result$topics) + eta, 2, function(x) x/sum(x)))
doc.length <- sapply(documents, function(x) sum(x[2, ])) 
term.frequency <- rep(0,length(vocab))
for (ii in 1:length(documents)){
  term.frequency[documents[[ii]][1,]+1] <- term.frequency[documents[[ii]][1,]+1] + documents[[ii]][2,]
}

visobj <- list(phi = phi,
               theta = theta,
               doc.length = doc.length,
               vocab = vocab,
               term.frequency = term.frequency)
json <- createJSON(phi = visobj$phi, 
                   theta = visobj$theta, 
                   doc.length = visobj$doc.length, 
                   vocab = visobj$vocab, 
                   term.frequency = visobj$term.frequency)
serVis(json)