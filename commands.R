library(readr)
library(magrittr)
library(tokenizers)
library(collections)
library(data.table)

#read in the raw data
dataBlog <- read_lines("./final/en_US/en_US.blogs.txt")
dataNews <- read_lines("./final/en_US/en_US.news.txt")
dataTwitter <- read_lines("./final/en_US/en_US.twitter.txt")


#clean and tokenize the raw data
tokBlog <- makeToken(dataBlog)
tokBlog <- makeTokUnk(tokBlog)

tokNews <- makeToken(dataNews) 
tokNews <- makeTokUnk(tokNews)

tokTwitter <- makeToken(dataTwitter)
tokTwitter <- makeTokUnk(tokTwitter)


######(optional)######
#depending on the machine's memory limitation, the following might be needed to 
#split the tokens into smaller objects.
nsplit <- 10
idx <- splitTok(tokBlog,nsplit)
for(i in 1:nsplit) assign(paste0("tok",i), tokBlog[idx[i]:idx[i+1]])

idx <- splitTok(tokNews,nsplit)
for(i in 1:nsplit) assign(paste0("tok",i+10), tokNews[idx[i]:idx[i+1]])

idx <- splitTok(tokTwitter,nsplit)
for(i in 1:nsplit) assign(paste0("tok",i+20), tokTwitter[idx[i]:idx[i+1]])
######################


#define dictionary for building a tree (refer to the model-construction documentation for details)
d <- dict()

#build a tree from first 50% of data using the split objects from lines 27-35
for(file in c(rep(1:5,each=3)+rep(c(0,10,20),5))){
  dataname <- paste0("tok",file)
  buildDict(d,get(dataname),4,TRUE)
  # save(d, file=paste0("dict_",dataname,".Rda")) #save the output after each round just to be safe
}

#build a data table from the tree to enable deployment on Shinyapp.io
buildDT(d,5,2,T)

