# TA: Leslie Huang
# Course: Text as Data
# Date: 1/25/2018
# This lab is adapted from Patrick Chester

# Download the file "national_clinton_trump_6_20_2016.csv" from the repo


################
# 1 Setting up #
################

# 1.1 Clearing environment
rm(list = ls())

# 1.2 Working directory

# Where am I?
getwd()

# Setting working directory
setwd("/Users/lesliehuang/Text-as-Data-Lab-Spr2018")

# or even
setwd(getwd())

# 1.3 Installing and loading some useful packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("xtable")

library(dplyr)
library(ggplot2)
library(xtable)

# 1.4 Loading data
dat <- read.csv("national_clinton_trump_6_20_2016.csv", stringsAsFactors = FALSE)

#######################
# 2 Working with data #
#######################

# 2.1 How to extract a vector or a subset of a data frame from a data frame

# A) Dollar sign operator
dat$Pollster

# B) Matrix identifier
dat[,"Pollster"]
dat[,c("Pollster", "Number.of.Observations")]

# C) dplyr
dat %>% select(Pollster)
dat %>% select(Pollster, Number.of.Observations)

select(dat, Pollster)

# 2.2 How to identify a row (or set of rows) in a data frame

# A) Dollar sign operator
dat$Number.of.Observations[1] # Returns the first row of the data frame
dat$Number.of.Observations[1:5] # Returns the first 5 rows of the data frame
dat$Number.of.Observations[dat$Pollster == "Quinnipiac"] # Returns all rows where Pollster = Quinnipiac

# B) Matrix identifier
dat[1, "Number.of.Observations"] 
dat[1:5, "Number.of.Observations"] 
dat[dat$Pollster == "Quinnipiac","Number.of.Observations"] 

# C) dplyr
dat %>% slice(1) %>% select(Number.of.Observations) 
dat %>% slice(1:5) %>% select(Number.of.Observations)
dat %>% filter(Pollster == "Quinnipiac") %>% select(Number.of.Observations)


# 2.3 Creating new variables in a data frame

# A) Dollar sign operator
dat$net_clinton_a <- dat$Clinton - dat$Trump

# B) Matrix identifier
dat[,"net_clinton_b"] <- dat$Clinton - dat$Trump


# C) dplyr
dat <- dat %>% mutate(
  net_clinton_c = Clinton - Trump
)

dat <- mutate(dat, net_clinton_d = Clinton - Trump)

# Are these variables equivalent to one another?
all.equal(dat$net_clinton_a,dat$net_clinton_b)  
all.equal(dat$net_clinton_b,dat$net_clinton_c) 
all.equal(dat$net_clinton_c,dat$net_clinton_d)  

# Yes. Yes they are.

# 2.4 Removing columns
dat$net_clinton_b <- NULL

# Using dplyr
dat <- subset(dat, select = -c(net_clinton_d))

# 2.5 Summarizing Data

# A) Identifying the number of rows and columns
nrow(dat)
ncol(dat)

# B) Getting a quick summary of your data
summary(dat)
str(dat)
glimpse(dat)

# C) Summarizing variables by another variable in a table
table1 <- dat %>% group_by(Pollster) %>% summarise(mean(net_clinton_a))

# D) Summarizing a variable with a histogram

# Basic R graphics
hist(dat$net_clinton_a)

# ggplot2 graphics
plot1 <- ggplot(aes(net_clinton_a), data = dat) + geom_histogram(bins = 15) + theme_light()

# 2.6 Exporting data

# Exporting table to CSV
write.csv(table1,file = "table1.csv")

# Creating LaTeX table
xtable(table1,caption = "Average Clinton Polling Advantage by Polling Firm")

# Exporting graph to pdf
pdf(width = 4, height = 3, "plot1.pdf")
plot1
dev.off()

#########################
# 3 Advanced Operations #
#########################

# 3.1 For Loops
char.var <- c() # A container for variable names that contain characters

for(i in names(dat)){ # A loop that identifies and stores variables that contain characters
  if(is.character(dat[,i])){
    char.var <- c(char.var, i)
    print(i)
  }
}

# 3.2 Apply functions
names(dat) <- sapply(names(dat), function(i){
  gsub("\\.", "_", i) # Replaces all instances of "." with an "_"
  gsub("__", "_", i) # Replaces all instances of "__" with "_"
})

# 3.3 User written functions
abs.dist.fun<-function(vec1, vec2){ # Calculates the absolute distance between two vectors
  sqrt(sum(abs(vec1 - vec2)))
}

# Python users: R cannot return multiple values from a function -- you will have to return a list of the values you want to return. There is also no equivalent to "yield"  

x <- rnorm(10) # Creates a vector of random normally distributed numbers
y <- x*2 + 3

abs.dist.fun(x,y)

####################
# 4 Free Resources #
####################

# UCLA
# http://www.ats.ucla.edu/stat/r/

# Rbloggers
# https://www.r-bloggers.com/how-to-learn-r-2/

# Data Camp
# https://www.datacamp.com/

# Also, don't forget Google and Stackoverflow!

