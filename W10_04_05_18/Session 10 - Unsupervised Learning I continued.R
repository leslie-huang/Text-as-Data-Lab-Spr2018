# TA: Leslie Huang
# Course: Text as Data
# Date: 4/5/2018
# Recitation 10: Unsupervised Learning I, continued

# Set up workspace
rm(list = ls())

setwd("/Users/lesliehuang/Text-as-Data-Lab-Spr2018/W10_04_05_18/")

library(quanteda)
library(quanteda.corpora)
library(lsa)

# 1 More LSA (Questions from last time)

# 1.1 From last time:

# Load the SOTU data again
data("data_corpus_sotu")

SOTU_dfm <- dfm(data_corpus_sotu[145:223,], 
                stem = T, 
                remove_punct = T, 
                remove = stopwords("english")
)

SOTU_mat <- convert(SOTU_dfm, to = "matrix")

# 1.2 LSA without local or global weighting -- SVD of term-document matrix 

SOTU_lsa_auto <- lsa(t(SOTU_mat))
SOTU_lsa_auto_mat <- t(as.textmatrix(SOTU_lsa_auto))

# Inspect how specific terms in a specific speech have been transformed
SOTU_dfm@Dimnames$docs[9]
topfeatures(SOTU_dfm[9,])

# LSA transform
sort(SOTU_lsa_auto_mat[9,], decreasing=T)[1:10]


# 1.3 LSA with local weighting 
# local weight is log TF
# global weight is IDF
# Other options listed in the documentation:
?gw_idf

# Transform the document-term matrix
SOTU_dfm_weighted <- lw_logtf(SOTU_mat) * gw_idf(SOTU_mat)

# Run LSA (auto number of dimensions)
SOTU_lsa_weighted <- lsa(t(SOTU_dfm_weighted))
SOTU_lsa_weighted_mat <- t(as.textmatrix(SOTU_lsa_weighted))


# Inspect the values
SOTU_dfm@Dimnames$docs[9]
topfeatures(SOTU_dfm[9,])

sort(SOTU_lsa_weighted_mat[9,], decreasing=T)[1:10]

# You can also compare how values for certain words change when you run LSA with different numbers of dimensions

# 2 LSA on n-grams

SOTU_bigrams_tokens <- tokens(data_corpus_sotu[145:223,], ngrams = 2, remove_punct = T)
SOTU_bigrams_dfm <- dfm(SOTU_bigrams_tokens) # obviously we cannot remove stopwords etc
SOTU_bigrams_mat <- convert(SOTU_bigrams_dfm, to = "matrix")

SOTU_bigrams_auto <- lsa(t(SOTU_bigrams_mat))
SOTU_bigrams_auto_mat <- t(as.textmatrix(SOTU_bigrams_auto))

# Inspect the values
SOTU_dfm@Dimnames$docs[9]
topfeatures(SOTU_dfm[9,])

# The bigrams won't match up with unigrams, but...

# Most common: plagued with stopwords!
sort(SOTU_bigrams_auto_mat[9,], decreasing=T)[1:10]

# Least common
sort(SOTU_bigrams_auto_mat[9,], decreasing=F)[1:10]
