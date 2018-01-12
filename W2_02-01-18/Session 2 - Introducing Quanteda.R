# TA: Leslie Huang
# Course: Text as Data
# Date: 02/01/2018
# Code Credit: Patrick Chester, Kevin Munger

## 1 Setting up Quanteda 

# Clear Global Environment
rm(list = ls())

# Set working directory
setwd(getwd())

# Install the latest stable version of quanteda from CRAN
install.packages("quanteda")
library("quanteda")

# If you want to install the latest dev version, first install the package "devtools" which is used to install packages directly from Github
install.packages("devtools")
library("devtools")
devtools::install_github("quanteda/quanteda") 


# We also use devtools to install some sample data
devtools::install_github("quanteda/quanteda.corpora")

# Load it into our environment
library(quanteda.corpora)

# Read about the data available: https://github.com/quanteda/quanteda.corpora

# Note: Quanteda is still under development so it is changing! New features are being added but sometimes functions or function parameters are deprecated or renamed. This includes the very basic functions in this code demo!

# This means that many code examples, StackOverflow questions, and websites with outdated documentation, etc. may include functions or options that have been deprecated or renamed.

# Always check the quanteda version!

# If you want to ensure that your code for a project will not break if you update quanteda, I recommend using a dependency manager for R called packrat so that you can specify a dependency on a specific version of quanteda.

## 2 Running basic text analysis

# start with a short character vector
sampletxt <- "The police with their policing strategy instituted a policy of general 
iterations at the Data Science Institute."

# Let's tokenize (break vector into individual words)
tokens <- tokens(sampletxt)
?tokens

tokens <- tokens(sampletxt, remove_punct = TRUE)

# Stemming examples
# SnowballC stemmer is based on the Porter stemmer 

stems <- tokens_wordstem(tokens)
?tokens_wordstem

# Loading State of the Union corpus

data("data_corpus_stou", package = "quanteda.corpora")

# ndoc identifies the number of documents in a corpus

ndocs <- ndoc(data_corpus_stou)

# Here, we identifiy the text of the last SOTU Speech in the corpus

last_speech_text <- data_corpus_stou[ndocs]

# same as 

last_speech_text <- texts(data_corpus_stou)[ndocs]

# The DFM function creates a Document Feature Matrix from the last SOTU speech

obama_dfm <- dfm(last_speech_text)
?dfm

# Inspecting the components of a DFM object

str(obama_dfm) # You can see this in the RStudio "Environment" pane as well

obama_dfm[1,1:20]

# The topfeatures function by default shows the most frequently occuring terms in a given DFM

topfeatures(obama_dfm)

# Are all of these features relevant?

# Stopwords

# Stopwords are commonly used words that add little understanding to the content of the document by themselves

# The stopwords function takes a language as an input and produces a vector of stopwords compiled from that language

stopwords("english")

# Fun fact: Quanteda also supports stopwords for english, SMART, danish, french, greek, hungarian, 
# norwegian, russian, swedish, catalan, dutch, finnish, german, italian, portuguese, spanish, and arabic

# Here we compare a DFM from the last SOTU while without English stopwords with one that has them

obama_dfm1 <- dfm(last_speech_text, remove_punct = TRUE)
obama_dfm2 <- dfm(last_speech_text, remove = stopwords("english"), remove_punct = TRUE)

topfeatures(obama_dfm1)
topfeatures(obama_dfm2)

## 3 Visualization and Weighted DFM

# Now we will create a DFM of all the SOTU speeches

full_dfm <- dfm(data_corpus_stou, remove = stopwords("english"), remove_punct = TRUE)

topfeatures(full_dfm)

# Visualizing the text contained within the data frame

textplot_wordcloud(full_dfm)

# tfidf - Frequency weighting

weighted_dfm <- tfidf(full_dfm) # Uses the absolute frequency of terms in each document

topfeatures(weighted_dfm)
?tfidf

# tfidf - Relative frequency weighting

normalized <- tfidf(full_dfm, normalize=TRUE) # Uses the relative number of terms in each document

topfeatures(normalized)

# What do the different rankings imply?


## 4 Collocations

# bigrams

collocations(last_speech_text)
?collocations

# trigrams

collocations(last_speech_text, size=3)

# detect collocations in overall

colloc <- collocations(last_speech_text)


# remove any collocations containing a word in the stoplist

removeFeatures(colloc, stopwords("english"))

# Are there any other terms you all think are interesting?


## 5 Regular expressions

# regular expressions are a very powerful tool in wrangling text
# not a focus of this class, but something to be aware of

?regex

s_index <- grep(" s ", texts(SOTUCorpus))

?grep

# this returns every speech that contains " s "
s_texts <- grep(" s ", texts(SOTUCorpus), value=TRUE)

# Here we create a vector of documents with " s " removed
no_s <- gsub(" s ", "",  SOTUCorpus[s_index])

# Questions?