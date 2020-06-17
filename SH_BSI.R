#Load Libraries
library(tidyverse)      # data manipulation & plotting
library(stringr)        # text cleaning and regular expressions
library(tidytext)       # provides additional text mining functions
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#Load Lemke UDFs
source("functions.R")

#Setting the computer directory
getwd()
setwd("/Users/michael_lemke/desktop/class")

#Create an empty variable to store text
sherlock_holmes <- c()

#Load Chapter Text
sherlock_holmes$CH03 <- readLines("SH_BSI_Ch03.txt")
sherlock_holmes$CH18 <- readLines("SH_BSI_Ch18.txt")
sherlock_holmes$CH19 <- readLines("SH_BSI_Ch19.txt")
sherlock_holmes$CH20 <- readLines("SH_BSI_Ch20.txt")
sherlock_holmes$CH21 <- readLines("SH_BSI_Ch21.txt")
sherlock_holmes$CH22 <- readLines("SH_BSI_Ch22.txt")
sherlock_holmes$CH27 <- readLines("SH_BSI_Ch27.txt")
sherlock_holmes$CH28 <- readLines("SH_BSI_Ch28.txt")

#Convert vector to character vector
sherlock_holmes <- as.character(sherlock_holmes)

#Load the data as a corpus
docs <- Corpus(VectorSource(sherlock_holmes))

#Funciton Space
par(mfrow=c(2,2))
quick_wordfreq(sherlock_holmes[2:4], c(""), "Chapter 18-20")
quick_wordcloud(sherlock_holmes[5:6], c(""))

quick_wordfreq(sherlock_holmes[5:6], c(""), "Chapter 21-22")
quick_wordcloud(sherlock_holmes[5:6], c(""))

#What are sentiments?
sentiments

# Sentiments Value
get_sentiments("bing")

#Print Emotional Counts
titles <- c("All")
books <- list(sherlock_holmes)
print_em_counts(titles,books)

#Climax
titles <- c("All", "Ch 3", "Ch 18-20","Ch 21-22")
books <- list(sherlock_holmes, sherlock_holmes[1], sherlock_holmes[2:4],sherlock_holmes[5:6])
sentiment_graph(titles, books)

bar_chart_em_freq(titles,books)

book_sent(sherlock_holmes)




