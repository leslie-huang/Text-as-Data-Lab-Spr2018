# TA: Leslie Huang
# Course: Text as Data
# Date: 3/29/2018
# Recitation 9: Unsupervised Learning I

# Set up workspace
rm(list = ls())

setwd("/Users/lesliehuang/Text-as-Data-Lab-Spr2018/W9_03_29_18/")

# Loading packages
#install.packages("lsa")

library(quanteda)
library(quanteda.corpora)
library(lsa)

## 1 PCA

# 1.1 Two functions in base R:

?prcomp # SVD on the (centered) input data
?princomp # eigendecomposition on the covariance matrix of the input data -- can also use option for covariance matrix 

# Remember to center your data! -- use scale() on your matrix beforehand, or the option in prcomp()
# And don't have any missing values!

install.packages("factoextra")
library(factoextra) # makes it easy to work with PCA

# 1.2 Example
data("data_corpus_sotu")

SOTU_dfm <- dfm(data_corpus_sotu[145:223,], 
                stem = T, 
                remove_punct = T, 
                remove = stopwords("english")
)

SOTU_mat <- convert(SOTU_dfm, to = "matrix") # convert to matrix

SOTU_pca <- prcomp(SOTU_mat, center = TRUE, scale = TRUE)


# Elbow plot
plot(SOTU_pca, type = "l")

# How much variance do the first few PCs account for?
summary(SOTU_pca)

# Use the factoextra functions to help extract our results

# Eigenvalues
head(get_eigenvalue(SOTU_pca))

fviz_eig(SOTU_pca, addlabels = TRUE, ylim = c(0, 50))

# These results are not great -- why?

# Tutorial from factoextra author about how to use his package to explore and visualize PCA results: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

# See here for visualizing PCA with the ggbiplot library: https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/


## 2 Latent Semantic Analysis (LSA)

# Let's keep using the SOTU data from before

SOTU_dfm@Dimnames$docs

# Create LSA weights using TDM
SOTU_tdm_lsa <- lsa(t(SOTU_dfm))

# Check to see what a good number of dimensions is
SOTU_tdm_lsa_svd <- svd(SOTU_tdm_lsa$tk)$d

dimcalc_share(share = 0.5)(SOTU_tdm_lsa_svd)

plot(SOTU_tdm_lsa_svd)


# By default, share is set to .5; let's try .9
dimcalc_share(share = 0.9)(SOTU_tdm_lsa_svd)

# Lecture example uses dims = 5
lsa_fit_5 <- lsa(t(SOTU_dfm), 5 )

lsa_fit_mat_5 <- t(as.textmatrix(lsa_fit_5) )


# Compare features for a few speeches

SOTU_dfm@Dimnames$docs[9]
topfeatures(SOTU_dfm[9,])
sort(lsa_fit_mat_5[9,], decreasing=T)[1:10]

SOTU_dfm@Dimnames$docs[55]
topfeatures(SOTU_dfm[55,])
sort(lsa_fit_mat_5[55,], decreasing=T)[1:10]


SOTU_dfm@Dimnames$docs[72]
topfeatures(SOTU_dfm[72,])
sort(lsa_fit_mat_5[72,], decreasing=T)[1:10]

# associate(): a method to identify words that are most similar to other words using a LSA

lsa_fit_3 <- lsa(t(SOTU_dfm), 3 )

lsa_fit_mat_3 <- as.textmatrix(lsa_fit_3) 

china <- associate(lsa_fit_mat_3, "china", "cosine", threshold = .7)
china[1:10]


oil <- associate(lsa_fit_mat_3, "oil", "cosine", threshold = .7)
oil[1:10]

america<-associate(lsa_fit_mat_3, "america", "cosine", threshold = .7)
america[1:10]

health<-associate(lsa_fit_mat_3, "health", "cosine", threshold = .7)
health[1:10]



## 2 WORDFISH

# How is it different from other approaches we've used for scaling?

# 2.1 Read in conservative and labour manifestos (from Recitation 6)
setwd("../W6_02_27_18/cons_labour_manifestos")

files <- list.files( full.names=TRUE)
text <- lapply(files, readLines)
text<-unlist(lapply(text, function(x) paste(x, collapse = " ")))

# Name data
files <- unlist(files)
files <- gsub("./", "", files )
files <- gsub(".txt", "", files )

# Create metadata
year <- unlist(strsplit(files, "[^0-9]+"))
year <- year[year!=""]

party <- unlist(strsplit(files, "[^A-z]+"))
party <- party[party!="a" & party!="b"]

#create data frame
man_df <- data.frame(year = factor(as.numeric(year)),
                   party = factor(party),
                   text = text,
                   stringsAsFactors = FALSE)

lab_con_dfm <- dfm(man_df$text, 
                   stem = T, 
                   remove = stopwords("english"), 
                   remove_punct = T
                   )

# 2.2 fit wordfish

# Setting the anchor on parties
df_fit <- textmodel_wordfish(lab_con_dfm, c(1,24))

# Plot of document positions
plot(year[1:23], df_fit$theta[1:23]) # These are the conservative manifestos
points(year[24:46], df_fit$theta[24:46], pch = 8) # These are the Labour manifestos

plot(as.factor(party), df_fit$theta)

# most important features--word fixed effects
words<-df_fit$psi # values
names(words) <- df_fit$features # the words

sort(words)[1:50]

sort(words, decreasing=T)[1:50]

# Guitar plot
weights<-df_fit$beta
plot(weights, words)