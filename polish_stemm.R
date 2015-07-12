# Polish Stemming script for R
# Author: Daniel Szulc (https://pl.linkedin.com/in/dszulc)
# Based on morfologik dictionary by Marcin Miłkowski et al. 
# (http://morfologik.blogspot.com/2013/03/morfologik-20-polimorf.html)

#loading packages

library(tm)
library(data.table)
library(stringi)


#global settings

#which directory contains polimorfologik.txt file?
dictionary_dir <-"./dictionary/" 

#where is a text file you want to stemm?
text_file <-"./input/Polish_test/PL_wikipedia.txt"     


# initialize dictionary
        file <- paste0(dictionary_dir,"polimorfologik.txt")
        dictionary <- fread(file,header = FALSE)
        colnames(dictionary) <- c("word","stem","POS")
        
        dictionary$word <- stri_conv(dictionary$word,from = "UTF-8",to = "windows-1250")
        dictionary$stem <- stri_conv(dictionary$stem,from = "UTF-8",to = "windows-1250")
        
        
        pl_stopwords <- readLines(paste0(dictionary_dir,"polish_stopwords_win1250.txt"))

# functions 

toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x,fixed = TRUE))
toSpaceWord <- content_transformer(function(x, pattern) gsub(paste0(" ",pattern," "), " ", x,fixed = TRUE))

removeStopwords <- function(corpus, stopwords) {
        
        tmp <- corpus
        for (i in 1:length(stopwords)) {
                
                tmp <- tm_map(tmp, toSpaceWord, stopwords[i]) 
        }
        corpus<<-tmp
}

findPolishStem <-function(full_word) {
        
        library(data.table)
        
        tmp <- dictionary[word==full_word,stem]
        if (length(tmp)==0) to_return <- full_word
        else if (length(tmp)==1) to_return <- tmp
        else {
                to_return <- tmp[which.min(nchar(tmp))]
        }
        to_return
        
}
stemPolishDocument <- function(content) {
        tmp <- strsplit(paste(content, collapse=" "), " ", fixed=TRUE)
        tmp <- tmp[[1]]
        
        tmp <- sapply(tmp, findPolishStem)
        tmp <- paste(tmp, collapse = " ")
        tmp        
}

# reading text file
text <- readLines(text_file)

# creating a corpus
corpus <- Corpus(VectorSource(text))
before <- corpus[[1]][1]$content  # to demonstrate the effect of script
corpus <- tm_map(corpus, content_transformer(tolower))

#to remove punctuation we can't rely on removePunctuation() as it interfere
#with polish chars (especiallly with "ł")
# therefore we have to deal with punctuation as below
corpus <- tm_map(corpus, toSpace, ".")
corpus <- tm_map(corpus, toSpace, ",")
corpus <- tm_map(corpus, toSpace, ":")
corpus <- tm_map(corpus, toSpace, ";")
corpus <- tm_map(corpus, toSpace, "?")
corpus <- tm_map(corpus, toSpace, "!")
corpus <- tm_map(corpus, toSpace, "-")
corpus <- tm_map(corpus, toSpace, "+")
corpus <- tm_map(corpus, toSpace, "(")
corpus <- tm_map(corpus, toSpace, ")")
corpus <- tm_map(corpus, toSpace, "]")
corpus <- tm_map(corpus, toSpace, "[")
corpus <- tm_map(corpus, toSpace, "\"")
corpus <- tm_map(corpus, toSpace, "”")
corpus <- tm_map(corpus, toSpace, "„")

removeStopwords(corpus, pl_stopwords)  # we have to use our own function to remove stopwords
# same reason as in case of punctuation

corpus <- tm_map(corpus, content_transformer(stemPolishDocument))


after<- corpus[[1]][1]$content   # to demonstrate the effect of script

before
after


