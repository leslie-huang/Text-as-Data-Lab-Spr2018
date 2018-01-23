# TA: Leslie Huang
# Course: Text as Data
# Date: 02/01/2018
# Credit for previous versions of code: Patrick Chester, Kevin Munger

## 1 Setting up

# 1.1 Workspace

# Clear Global Environment
rm(list = ls())

# Set working directory
setwd(getwd())

# 1.2 Installing quanteda

# Install the latest stable version of quanteda from CRAN
# install.packages("quanteda")
library("quanteda")

# What version of quanteda do you have and how many threads (cores) are you using? See the printout in the console

# 1.3 Devtools and the quanteda corpus

# Install the package "devtools" which is used to install packages directly from Github
# install.packages("devtools")
library("devtools")

# Use devtools to install some sample data
devtools::install_github("quanteda/quanteda.corpora")

# Load it into our environment
library(quanteda.corpora)

# Read about the data available: https://github.com/quanteda/quanteda.corpora

### Note: Quanteda is still under development so it is changing! New features are being added but sometimes functions or function parameters are deprecated or renamed. This includes the very basic functions in this code demo!

# This means that you may encounter many code examples, StackOverflow questions, and websites with outdated documentation, etc. that include functions or options that have been deprecated or renamed.

# 1.4 Managing dependencies

# If you want to ensure that your code for a project will not break if you update quanteda, I recommend using a dependency manager for R called packrat so that you can specify a dependency on a specific version of quanteda.
# Find out about setting up packrat here: https://rstudio.github.io/packrat/walkthrough.html

# 1.5 Versions of quanteda

# How would you get an older version of quanteda? (For example, if you accidentally installed the dev version from GitHub but you want to go back to the last stable release.)

# If you want to install the latest dev version of quanteda, it's on quanteda, but we will use the latest version from CRAN for stability/sanity reasons
# devtools::install_github("quanteda/quanteda") 

# - Check the CRAN archive
# use the install_version function, e.g.:
# devtools::install_version("quanteda", version = "0.99.12", repos = "http://cran.us.r-project.org")


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

textplot_wordcloud(full_dfm, comparison.cloud = TRUE)

# tfidf - Frequency weighting

weighted_dfm <- tfidf(full_dfm) # Uses the absolute frequency of terms in each document

topfeatures(weighted_dfm)
?tfidf

# tfidf - Relative frequency weighting

normalized <- tfidf(full_dfm, scheme_tf = "prop") # Uses the relative number of terms in each document

topfeatures(normalized)

# What do the different rankings imply?


## 4 Collocations

# bigrams

textstat_collocations(last_speech_text)
?textstat_collocations

# trigrams

textstat_collocations(last_speech_text, size=3)

# detect collocations in overall

colloc <- textstat_collocations(last_speech_text)

# Are there any other terms you all think are interesting?


## 5 Regular expressions

# regular expressions are a very powerful tool in wrangling text
# not a focus of this class, but something to be aware of

?regex

s_index <- grep(" s ", texts(data_corpus_stou))

?grep

# this returns every speech that contains " s " -- JUST THE LETTER S BY ITSELF
texts_with_s <- grep(" s ", texts(data_corpus_stou), value = TRUE)

# Here we create a vector of documents with " s " removed
texts_without_s <- gsub(" s ", "",  data_corpus_stou[s_index])

# Questions?