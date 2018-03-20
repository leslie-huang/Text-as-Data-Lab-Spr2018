# TA: Leslie Huang
# Course: Text as Data
# Date: 3/22/2018
# Recitation 8: Supervised Learning IV

# Clear Global Environment
rm(list = ls())

# Setting WD
setwd("/Users/lesliehuang/Text-as-Data-Lab-Spr2018/W8_03_22_18/")

# 1 Questions from last time ...

# 1.1 Tuning hyperparameters? ** Beyond the scope of this class **

# install.packages("e1071")
library(e1071)

# Simple example here: https://rischanlab.github.io/SVM.html

# NB: e1071 uses libsvm(), a Python library. However, sklearn has a great SVM implementation!

# 1.2 SVM with tf-idf or raw term frequencies: which has better performance?

# We might expect that tf-idf is better because it upweights terms that discriminate more between documents 

# Example from https://rpubs.com/bmcole/reuters-text-categorization



# 2 KNN

