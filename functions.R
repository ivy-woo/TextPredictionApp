library(magrittr)
library(tokenizers)
library(data.table)
library(collections)


makeToken <- function(data){
  tok <- tokenize_sentences(data) %>% 
    unlist() %>%
    tokenize_words(strip_punct = F, strip_numeric = T) #%>%
######################################
# This part of the function is not open to prevent plagiarism by other participants of the same project course.
# Please write to the repo owner if you are interested in the code, she is happy for any constructive discussion.
######################################
}


junk <- function(token, percentage=0.9, monitor=TRUE){
  h <- dict()
  
  if(monitor){
    i <-0
    progress <- 0
    print("Building dictionary:")
  }
  
  for(word in token){
    if(monitor){
      i <- i+1
      if(round(i/length(token)*100) > progress){
        progress <- round(i/length(token)*100)
        print(paste(progress,"%"))
      }
    }
    if(word %in% c(",",".","<s>")) next
    if(!h$has(word)) h$set(word,1)
    else h$set(word, h$get(word)+1)
  }
  
  i <- 0
  cnt <- numeric(h$size())
  names(cnt) <- h$keys()
  
  if(monitor){
    progress <- 0
    print("Making count vector:")
  } 

######################################
# This part of the function is not open to prevent plagiarism by other participants of the same project course.
# Please write to the repo owner if you are interested in the code, she is happy for any constructive discussion.
######################################
  
  cat("In total the", i , "vocabularies constitute", temp/sum(cnt)*100, "% of total word counts.\n")
  cat("Remaining", length(cnt)-i, "words classified as junk.")
  
  if(i==length(cnt)) return()
  return(names(cnt)[(i+1):length(cnt)])
}


makeTokUnk <- function(token,percentage=0.9){
  junks <- junk(token,percentage)
  token[which(token %in% junks)] <- "<unk>"
  return(token)
}


splitTok <- function(token,nsplit=10){
  s <- which(token=="<s>")
  l <- c(1,1:(nsplit-1)*round(length(s)/nsplit))
  return(c(s[l],length(token)))
}


buildDict <- function(h, token, ngram=4, monitor=TRUE){
  if(monitor){
    start <- Sys.time()
    print(paste("Start time:", start))
    progress <- 0
  }
  
  i <- 1
  while(i < length(token)){
    if(token[i]=="<s>"){
      sentenceLength <- min(which(token[i:(i+ngram-1)]==".",useNames = F), ngram)
      que <- queue(token[i:(i+sentenceLength-1)])
      sync(h,que,sentenceLength)
      i <- i + sentenceLength
    }
    else{
      que$pop()
      que$push(token[i])
      for(j in 2:ngram) increment(h,que$as_list()[(que$size()-j+1):que$size()])
      i <- i+1
    }
    if(monitor){
      if(round(i/length(token)*100) > progress){
        progress <- round(i/length(token)*100)
        print(paste(progress,"%,", Sys.time()))
      }
    }
  }
  
  if(monitor){
    end <- Sys.time()
    print(paste("End time:",end))
    print(paste("Time taken:",end-start))
  }
}


sync <- function(h,que,ngram){
  for(a in 1:(ngram-1)){
    for(b in (a+1):ngram){
      increment(h,que$as_list()[a:b])
    } 
  }
}


increment <- function(h,quelist){
  g = h
  quelist <- unlist(quelist)
  l <- length(quelist)
  key <- ifelse(l>=3, paste(quelist[1:(l-2)],collapse=" "), "/e")
  target <- quelist[l]
  
  if(!g$has(quelist[l-1])) g$set(quelist[l-1],dict())
  g = g$get(quelist[l-1])
  
######################################
# This part of the function is not open to prevent plagiarism by other participants of the same project course.
# Please write to the repo owner if you are interested in the code, she is happy for any constructive discussion.
######################################
}


recommendation <- function(h,nrec=-1,monitor=TRUE){
  if(monitor){
    progress <- 0
    i <- 0
  }
  for(key1 in h$keys()){
    g = h$get(key1)
    for(key2 in g$keys()){
      rec <- sort(unlist(g$get(key2)), decreasing=T)
      unk <- match("<unk>",names(rec))
      if(!is.na(unk)) rec <- rec[-unk]
      n <- ifelse(nrec<0, length(rec), min(nrec,length(rec)))
      g$set(key2, names(rec[1:n]))
    }
    if(monitor){
      i <- i+1
      if(round(i/h$size()*100) > progress){
        progress <- round(i/h$size()*100)
        print(paste(progress,"%"))
      }
    }
  }
}


buildDT <- function(h,nrec=-1,mincnt=1,monitor=TRUE){
  if(monitor){
    start <- Sys.time()
    print(paste("Start time:", start))
    progress <- 0
    i <- 0
  }
  nrow <- cntgrams(h)
  txt <- character(nrow)
  pred <- character(nrow)
  j <- 1
  for(key1 in h$keys()){
    g = h$get(key1)
    for(key2 in g$keys()){
      rec <- unlist(g$get(key2))
      if(mincnt>1){
        rm <- which(rec<mincnt)
        if(length(rm)>0) rec <- rec[-rm]
      }  
      unk <- match("<unk>",names(rec))
      if(!is.na(unk)) rec <- rec[-unk]
      rec <- sort(rec, decreasing=T)
      n <- ifelse(nrec<0, length(rec), min(nrec,length(rec)))
      if(key2=="/e") txt[j] <- key1
      else txt[j] <- paste(key2,key1)
      if(n>0) pred[j] <- paste(names(rec)[1:n],collapse="/")
      j <- j+1
    }
    if(monitor){
      i <- i+1
      if(round(i/h$size()*100) > progress){
        progress <- round(i/h$size()*100)
        print(paste(progress,"%"))
      }
    }
  }
  DT <- data.table(txt=txt, pred=pred)
  rm(txt,pred)
  DT <- DT[pred!=""]
  setkey(DT,txt)
  if(monitor){
    end <- Sys.time()
    print(paste("End time:",end))
    print(paste("Time taken:",end-start))
  }
  return(DT)
}


cntgrams <- function(h){
  cnt <- 0
  for(key in h$keys()) cnt <- cnt + h$get(key)$size()
  return(cnt)
}


lookup <- function(DT, path, ngram=4, nxt=5){
  path <- splitInput(tolower(path))
  
  l <- length(path)
  if(l<(ngram-1)) path <- c("<s>",path)
  else path <- path[(l-ngram+2):l]
  l <- length(path)
  
  output <- list(pred=rep(NA,nxt),key="",nas=1)
  
  output <- searchDT(output,DT,path)
  
  if(!is.na(output$nas)){
    path[[l]] <- "<unk>"
    output <- searchDT(output,DT,path)
  }
  
  cat("Last path looked up:",output$key, "\n")
  cat("Predictions:",output$pred,"\n")
}


splitInput <- function(string){
  m <- gregexpr(" |,",string)
  split <- unlist(regmatches(string, m, invert=NA))
  empty <- which(split==" " | split=="")
  if(length(empty)==0) return(split)
  else return(split[-empty])
}


searchDT <- function(output,DT,path){
  output$key <- paste(c("notused",path),collapse=" ")
  
  unk <- FALSE
  while(!is.na(output$nas)){
    chr <- unlist(strsplit(output$key," "))
    if(length(chr)==1) break
    if(unk) output$key <- paste(c("<unk>",chr[-1]),collapse=" ")
    else output$key <- paste(chr[-1],collapse=" ")
    
    rec <- DT[txt==output$key,pred]
    if(length(rec)!=0){
      rec <- unlist(strsplit(rec,"/"))
      rec <- rec[!(rec %in% output$pred)]
      rec <- rec[1:min(sum(is.na(output$pred)),length(rec))] 
      output$pred[1:length(rec)+output$nas-1] <- rec
      output$nas <- match(NA,output$pred)
    }
    unk <- !unk
  }
  
  return(output)
}

