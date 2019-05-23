setwd("~/JHU_DataScienceCapstone")
con <- file("Data/en_US.twitter.txt", "rb")
twitterData <- readLines(con)
close(con)
con <- file("Data/en_US.blogs.txt", "rb")
blogData <- readLines(con)
close(con)
con <- file("Data/en_US.news.txt", "rb")
newsData <- readLines(con)
close(con)



twitterData_stepsize <- floor(length(twitterData)/33)
blogData_stepsize <- floor(length(blogData)/33)
newsData_stepsize <- floor(length(newsData)/33)

twitterStart <- 1
twitterStop <- twitterData_stepsize
blogStart <- 1
blogStop <- blogData_stepsize
newsStart <- 1
newsStop <- newsData_stepsize

start_time <- proc.time()

for(i in 2:33){
  
  print(paste0("starting ",i," of 33"))
  if(!file.exists(paste0("~/JHU_DataScienceCapstone/Data/BiGramSplit",i,".Rda"))){
    library(tm)
    library(slam)
    library("RWeka")
    
    corpusTwitter <- VCorpus(VectorSource(twitterData[twitterStart:twitterStop]))
    corpusBlog <- VCorpus(VectorSource(blogData[blogStart:blogStop]))
    corpusNews <- VCorpus(VectorSource(newsData[newsStart:newsStop]))
    
    corpusTwitter <- tm_map(corpusTwitter, stripWhitespace)
    corpusBlog <- tm_map(corpusBlog, stripWhitespace)
    corpusNews <- tm_map(corpusNews, stripWhitespace)
    
    #DTMtwitter_uni <- DocumentTermMatrix(corpusTwitter)
    #DTMblog_uni <- DocumentTermMatrix(corpusBlog)
    #DTMnews_uni <- DocumentTermMatrix(corpusNews)
    
    BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2, delimiters = " "))
    TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3, delimiters = " "))
    FourgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4, delimiters = " "))
    
    DTMtwitter_bi <- DocumentTermMatrix(corpusTwitter, control=list(tokenize=BigramTokenizer
                                                                    ,removePunctuation = FALSE
                                                                    ,stopwords = FALSE
                                                                    ,stemming = FALSE
                                                                    ,tolower = FALSE
    )
    )
    DTMblog_bi <- DocumentTermMatrix(corpusBlog, control=list(tokenize=BigramTokenizer
                                                              ,removePunctuation = FALSE
                                                              ,stopwords = FALSE
                                                              ,stemming = FALSE
                                                              ,tolower = FALSE
    )
    )
    DTMnews_bi <- DocumentTermMatrix(corpusNews, control=list(tokenize=BigramTokenizer
                                                              ,removePunctuation = FALSE
                                                              ,stopwords = FALSE
                                                              ,stemming = FALSE
                                                              ,tolower = FALSE
    )
    )
    
    DTMtwitter_tri <- DocumentTermMatrix(corpusTwitter, control=list(tokenize=TrigramTokenizer
                                                                     ,removePunctuation = FALSE
                                                                     ,stopwords = FALSE
                                                                     ,stemming = FALSE
                                                                     ,tolower = FALSE
    )
    )
    DTMblog_tri <- DocumentTermMatrix(corpusBlog, control=list(tokenize=TrigramTokenizer
                                                               ,removePunctuation = FALSE
                                                               ,stopwords = FALSE
                                                               ,stemming = FALSE
                                                               ,tolower = FALSE
    )
    )
    DTMnews_tri <- DocumentTermMatrix(corpusNews, control=list(tokenize=TrigramTokenizer
                                                               ,removePunctuation = FALSE
                                                               ,stopwords = FALSE
                                                               ,stemming = FALSE
                                                               ,tolower = FALSE
    )
    )
    
    DTMtwitter_four <- DocumentTermMatrix(corpusTwitter, control=list(tokenize=FourgramTokenizer
                                                                      ,removePunctuation = FALSE
                                                                      ,stopwords = FALSE
                                                                      ,stemming = FALSE
                                                                      ,tolower = FALSE
    )
    )
    DTMblog_four <- DocumentTermMatrix(corpusBlog, control=list(tokenize=FourgramTokenizer
                                                                ,removePunctuation = FALSE
                                                                ,stopwords = FALSE
                                                                ,stemming = FALSE
                                                                ,tolower = FALSE
    )
    )
    DTMnews_four <- DocumentTermMatrix(corpusNews, control=list(tokenize=FourgramTokenizer
                                                                ,removePunctuation = FALSE
                                                                ,stopwords = FALSE
                                                                ,stemming = FALSE
                                                                ,tolower = FALSE
    )
    )
    
    library(stringr)
    
    BigramsSplit_twitter <- col_sums(DTMtwitter_bi)
    BigramsSplit_twitter <-data.frame(freq=BigramsSplit_twitter,terms=names(BigramsSplit_twitter))
    BigramsSplit2_twitter <- data.frame(str_split(BigramsSplit_twitter$terms, " ", simplify = TRUE))
    BigramsSplit_twitter <- cbind(BigramsSplit2_twitter,BigramsSplit_twitter$freq)
    names(BigramsSplit_twitter)<-c("X1","X2","Freq")
    
    TrigramsSplit_twitter <- col_sums(DTMtwitter_tri)
    TrigramsSplit_twitter <-data.frame(freq=TrigramsSplit_twitter,terms=names(TrigramsSplit_twitter))
    TrigramsSplit2_twitter <- data.frame(str_split(TrigramsSplit_twitter$terms, " ", simplify = TRUE))
    TrigramsSplit_twitter <- cbind(TrigramsSplit2_twitter,TrigramsSplit_twitter$freq)
    names(TrigramsSplit_twitter)<-c("X1","X2","X3","Freq")
    
    FourgramsSplit_twitter <- col_sums(DTMtwitter_four)
    FourgramsSplit_twitter <-data.frame(freq=FourgramsSplit_twitter,terms=names(FourgramsSplit_twitter))
    FourgramsSplit2_twitter <- data.frame(str_split(FourgramsSplit_twitter$terms, " ", simplify = TRUE))
    FourgramsSplit_twitter <- cbind(FourgramsSplit2_twitter,FourgramsSplit_twitter$freq)
    names(FourgramsSplit_twitter)<-c("X1","X2","X3","X4","Freq")
    
    BigramsSplit_blog <- col_sums(DTMblog_bi)
    BigramsSplit_blog <-data.frame(freq=BigramsSplit_blog,terms=names(BigramsSplit_blog))
    BigramsSplit2_blog <- data.frame(str_split(BigramsSplit_blog$terms, " ", simplify = TRUE))
    BigramsSplit_blog <- cbind(BigramsSplit2_blog,BigramsSplit_blog$freq)
    names(BigramsSplit_blog)<-c("X1","X2","Freq")
    
    TrigramsSplit_blog <- col_sums(DTMblog_tri)
    TrigramsSplit_blog <-data.frame(freq=TrigramsSplit_blog,terms=names(TrigramsSplit_blog))
    TrigramsSplit2_blog <- data.frame(str_split(TrigramsSplit_blog$terms, " ", simplify = TRUE))
    TrigramsSplit_blog <- cbind(TrigramsSplit2_blog,TrigramsSplit_blog$freq)
    names(TrigramsSplit_blog)<-c("X1","X2","X3","Freq")
    
    FourgramsSplit_blog <- col_sums(DTMblog_four)
    FourgramsSplit_blog <-data.frame(freq=FourgramsSplit_blog,terms=names(FourgramsSplit_blog))
    FourgramsSplit2_blog <- data.frame(str_split(FourgramsSplit_blog$terms, " ", simplify = TRUE))
    FourgramsSplit_blog <- cbind(FourgramsSplit2_blog,FourgramsSplit_blog$freq)
    names(FourgramsSplit_blog)<-c("X1","X2","X3","X4","Freq")
    
    
    BigramsSplit_news <- col_sums(DTMnews_bi)
    BigramsSplit_news <-data.frame(freq=BigramsSplit_news,terms=names(BigramsSplit_news))
    BigramsSplit2_news <- data.frame(str_split(BigramsSplit_news$terms, " ", simplify = TRUE))
    BigramsSplit_news <- cbind(BigramsSplit2_news,BigramsSplit_news$freq)
    names(BigramsSplit_news)<-c("X1","X2","Freq")
    
    TrigramsSplit_news <- col_sums(DTMnews_tri)
    TrigramsSplit_news <-data.frame(freq=TrigramsSplit_news,terms=names(TrigramsSplit_news))
    TrigramsSplit2_news <- data.frame(str_split(TrigramsSplit_news$terms, " ", simplify = TRUE))
    TrigramsSplit_news <- cbind(TrigramsSplit2_news,TrigramsSplit_news$freq)
    names(TrigramsSplit_news)<-c("X1","X2","X3","Freq")
    
    FourgramsSplit_news <- col_sums(DTMnews_four)
    FourgramsSplit_news <-data.frame(freq=FourgramsSplit_news,terms=names(FourgramsSplit_news))
    FourgramsSplit2_news <- data.frame(str_split(FourgramsSplit_news$terms, " ", simplify = TRUE))
    FourgramsSplit_news <- cbind(FourgramsSplit2_news,FourgramsSplit_news$freq)
    names(FourgramsSplit_news)<-c("X1","X2","X3","X4","Freq")
    
    BiGramSplit <- rbind(BigramsSplit_twitter,BigramsSplit_blog,BigramsSplit_news)
    BiGramSplit$DocType <- c(rep("Twitter",nrow(BigramsSplit_twitter)),rep("Blog",nrow(BigramsSplit_blog)),rep("News",nrow(BigramsSplit_news)))
    TriGramSplit <- rbind(TrigramsSplit_twitter,TrigramsSplit_blog,TrigramsSplit_news)
    TriGramSplit$DocType <- c(rep("Twitter",nrow(TrigramsSplit_twitter)),rep("Blog",nrow(TrigramsSplit_blog)),rep("News",nrow(TrigramsSplit_news)))
    FourGramSplit <- rbind(FourgramsSplit_twitter,FourgramsSplit_blog,FourgramsSplit_news)
    FourGramSplit$DocType <- c(rep("Twitter",nrow(FourgramsSplit_twitter)),rep("Blog",nrow(FourgramsSplit_blog)),rep("News",nrow(FourgramsSplit_news)))
    
    setwd("~/JHU_DataScienceCapstone/Data/")
    save(BiGramSplit,file=paste0("BiGramSplit",i,".Rda"))
    save(TriGramSplit,file=paste0("TriGramSplit",i,".Rda"))
    save(FourGramSplit,file=paste0("FourGramSplit",i,".Rda"))
  }

  
  

  
  print(paste0("completed ",i," of 33"))
  Run_time <- proc.time() - start_time
  print(paste0(Run_time[3]/60," minutes so far, ",Run_time[3]/60 * (32 - i) / i, " remaining"))
  
  twitterStart <- twitterStop + 1
  twitterStop <- twitterStop + twitterData_stepsize
  blogStart <- blogStop + 1
  blogStop <- blogStop + blogData_stepsize
  newsStart <- newsStop + 1
  newsStop <- newsStop + newsData_stepsize
}

setwd("~/JHU_DataScienceCapstone/Data/")
i <- 1
load(file=paste0("BiGramSplit",i,".Rda"))
BiGramsComplete <- BiGramSplit
for (i in 2:18){
  if(file.exists(paste0("~/JHU_DataScienceCapstone/Data/BiGramSplit",i,".Rda"))){
    load(file=paste0("BiGramSplit",i,".Rda"))
    BiGramsComplete <- rbind(BiGramsComplete,BiGramSplit)
  }
}
BiGramsComplete<-BiGramsComplete[BiGramsComplete$Freq > 1,]
BiGramsComplete <- aggregate(Freq ~ ., data=BiGramsComplete, FUN=sum)
save(BiGramsComplete,file="BiGramsComplete.Rda")

##Note, I had to run the above loop multiple times, and I quit at 18 completed steps.
##somewhere along the steps, i messed up as was evidenced by the fact that XXX13.Rmd files perfectly equaled xxx14.Rmd files.
##So I deleted the xxx14.Rmd files and continued on below

rm(list=ls())
setwd("~/JHU_DataScienceCapstone/Data/")

i <- 1
load(file=paste0("TriGramSplit",i,".Rda"))
TriGramsComplete <- TriGramSplit
for (i in 2:18){
  if(file.exists(paste0("~/JHU_DataScienceCapstone/Data/TriGramSplit",i,".Rda"))){
    load(file=paste0("TriGramSplit",i,".Rda"))
    TriGramsComplete <- rbind(TriGramsComplete,TriGramSplit)
  }
}
TriGramsComplete<-TriGramsComplete[TriGramsComplete$Freq > 1,]
TriGramsComplete <- aggregate(Freq ~ ., data=TriGramsComplete, FUN=sum)
save(TriGramsComplete,file="TriGramsComplete.Rda")

rm(list=ls())
setwd("~/JHU_DataScienceCapstone/Data/")

i <- 1
load(file=paste0("FourGramSplit",i,".Rda"))
FourGramsComplete <- FourGramSplit
for (i in 2:18){
  if(file.exists(paste0("~/JHU_DataScienceCapstone/Data/FourGramSplit",i,".Rda"))){
    load(file=paste0("FourGramSplit",i,".Rda"))
    FourGramsComplete <- rbind(FourGramsComplete,FourGramSplit)
  }
}
FourGramsComplete<-FourGramsComplete[FourGramsComplete$Freq > 1,]
FourGramsComplete <- aggregate(Freq ~ ., data=FourGramsComplete, FUN=sum)
save(FourGramsComplete,file="FourGramsComplete.Rda")

rm(list=ls())

load(file="BiGramsComplete.Rda")
BiGramsComplete$X1 <- as.character(BiGramsComplete$X1)
BiGramsComplete$X2 <- as.character(BiGramsComplete$X2)
save(BiGramsComplete,file="BiGramsComplete.Rda")
load(file="TriGramsComplete.Rda")
TriGramsComplete$X1 <- as.character(TriGramsComplete$X1)
TriGramsComplete$X2 <- as.character(TriGramsComplete$X2)
TriGramsComplete$X3 <- as.character(TriGramsComplete$X3)
save(TriGramsComplete,file="TriGramsComplete.Rda")
load(file="FourGramsComplete.Rda")
FourGramsComplete$X1 <- as.character(FourGramsComplete$X1)
FourGramsComplete$X2 <- as.character(FourGramsComplete$X2)
FourGramsComplete$X3 <- as.character(FourGramsComplete$X3)
FourGramsComplete$X4 <- as.character(FourGramsComplete$X4)
save(FourGramsComplete,file="FourGramsComplete.Rda")


#Start actual prediction work
setwd("~/JHU_DataScienceCapstone/Data/")
library(stringr)
library(tm)
library(RWeka)
library(slam)
library(dplyr)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2, delimiters = " "))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3, delimiters = " "))
FourgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4, delimiters = " "))
 
load(file="BiGramsComplete.Rda")
load(file="TriGramsComplete.Rda")
load(file="FourGramsComplete.Rda")
PredictFunction <- function(text = "I",type="Twitter"){
  #back off degradation (not as sophisticated as Katz)
  deg_const <- .5
  
  #Determine how many terms there are:
  if(text == ""){
    return("Please enter words or a phrase!")
  } else {
    text <- stripWhitespace(text)
    text <- trimws(text)
    
    number_words <- length(str_locate_all(text," ")[[1]][,1])+1
    space_indeces <- str_locate_all(text," ")[[1]][,1]
    
    if(number_words >= 3){
      if(number_words > 3){
        text_last_three <- substr(text,space_indeces[length(space_indeces)-2]+1,nchar(text))
      } else {
        text_last_three <- text
      }
      TrigramsSplit <- data.frame(str_split(text_last_three," ",simplify = TRUE))
      names(TrigramsSplit)<-c("X1","X2","X3")
      
      text_last_two <- substr(text,space_indeces[length(space_indeces)-1]+1,nchar(text))
      BigramsSplit <- data.frame(str_split(text_last_two," ",simplify = TRUE))
      names(BigramsSplit)<-c("X1","X2")
      
      text_last_one <- substr(text,space_indeces[length(space_indeces)]+1,nchar(text))
      OnegramsSplit <- data.frame(str_split(text_last_one," ",simplify = TRUE))
      names(OnegramsSplit)<-c("X1")
      
      temp4<-FourGramsComplete[FourGramsComplete$X1 == TrigramsSplit$X1 & FourGramsComplete$X2 == TrigramsSplit$X2 & FourGramsComplete$X3 == TrigramsSplit$X3,]
      if(type != "All"){
        temp4 <- temp4[temp4$DocType == type,]
      }
      temp3<-TriGramsComplete[TriGramsComplete$X1 == BigramsSplit$X1 & TriGramsComplete$X2 == BigramsSplit$X2,]
      if(type != "All"){
        temp3 <- temp3[temp3$DocType == type,]
      }
      temp2<-BiGramsComplete[BiGramsComplete$X1 == OnegramsSplit$X1,]
      if(type != "All"){
        temp2 <- temp2[temp2$DocType == type,]
      }
      temp_comb <- bind_rows(temp4,temp3,temp2)
      temp_comb$aggby <- c(rep("four",nrow(temp4)),rep("three",nrow(temp3)),rep("two",nrow(temp2)))
      temp_agg<-aggregate(Freq ~ aggby, data=temp_comb, FUN=sum)
      names(temp_agg)[2] <- "tot_Freq"
      temp_comb <- left_join(temp_comb,temp_agg,by=c("aggby" = "aggby"))
      temp_comb$init_prob <- temp_comb$Freq / temp_comb$tot_Freq
      temp_comb$final_prob <- temp_comb$init_prob * ifelse(temp_comb$aggby == "four", 1, ifelse(temp_comb$aggby == "three", deg_const, deg_const^2))
      temp_comb <- temp_comb[order(temp_comb$final_prob, decreasing = TRUE),]
      return_row <- temp_comb[1:10,]
      return_list <- c(rep("",10))
      for(i in 1:10){
        return_list[i] <- ifelse(return_row$aggby[i] == "four", return_row$X4[i],ifelse(return_row$aggby[i] == "three",return_row$X3[i],return_row$X2[i]))
      }
      return(unique(return_list)[1:3])
      
    } else {
      
      
      
      
      if(number_words == 2){
        text_last_two <- text
        BigramsSplit <- data.frame(str_split(text_last_two," ",simplify = TRUE))
        names(BigramsSplit)<-c("X1","X2")
        
        text_last_one <- substr(text,space_indeces[length(space_indeces)]+1,nchar(text))
        OnegramsSplit <- data.frame(str_split(text_last_one," ",simplify = TRUE))
        names(OnegramsSplit)<-c("X1")
        
        temp3<-TriGramsComplete[TriGramsComplete$X1 == BigramsSplit$X1 & TriGramsComplete$X2 == BigramsSplit$X2,]
        if(type != "All"){
          temp3 <- temp3[temp3$DocType == type,]
        }
        temp2<-BiGramsComplete[BiGramsComplete$X1 == OnegramsSplit$X1,]
        if(type != "All"){
          temp2 <- temp2[temp2$DocType == type,]
        }
        temp_comb <- bind_rows(temp3,temp2)
        temp_comb$aggby <- c(rep("three",nrow(temp3)),rep("two",nrow(temp2)))
        temp_agg<-aggregate(Freq ~ aggby, data=temp_comb, FUN=sum)
        names(temp_agg)[2] <- "tot_Freq"
        temp_comb <- left_join(temp_comb,temp_agg,by=c("aggby" = "aggby"))
        temp_comb$init_prob <- temp_comb$Freq / temp_comb$tot_Freq
        temp_comb$final_prob <- temp_comb$init_prob * ifelse(temp_comb$aggby == "four", 1, ifelse(temp_comb$aggby == "three", deg_const, deg_const^2))
        temp_comb <- temp_comb[order(temp_comb$final_prob, decreasing = TRUE),]
        return_row <- temp_comb[1:10,]
        return_list <- c(rep("",10))
        for(i in 1:10){
          return_list[i] <- ifelse(return_row$aggby[i] == "four", return_row$X4[i],ifelse(return_row$aggby[i] == "three",return_row$X3[i],return_row$X2[i]))
        }
        return(unique(return_list)[1:3])        
        
      } else {
        
        
        
        
        text_last_one <- text
        OnegramsSplit <- data.frame(str_split(text_last_one," ",simplify = TRUE))
        names(OnegramsSplit)<-c("X1")
        
        temp2<-BiGramsComplete[BiGramsComplete$X1 == OnegramsSplit$X1,]
        if(type != "All"){
          temp2 <- temp2[temp2$DocType == type,]
        }
        
        temp_comb <- temp2
        temp_comb$aggby <- c(rep("two",nrow(temp2)))
        temp_agg<-aggregate(Freq ~ aggby, data=temp_comb, FUN=sum)
        names(temp_agg)[2] <- "tot_Freq"
        temp_comb <- left_join(temp_comb,temp_agg,by=c("aggby" = "aggby"))
        temp_comb$init_prob <- temp_comb$Freq / temp_comb$tot_Freq
        temp_comb$final_prob <- temp_comb$init_prob * ifelse(temp_comb$aggby == "four", 1, ifelse(temp_comb$aggby == "three", deg_const, deg_const^2))
        temp_comb <- temp_comb[order(temp_comb$final_prob, decreasing = TRUE),]
        return_row <- temp_comb[1:10,]
        return_list <- c(rep("",10))
        for(i in 1:10){
          return_list[i] <- ifelse(return_row$aggby[i] == "four", return_row$X4[i],ifelse(return_row$aggby[i] == "three",return_row$X3[i],return_row$X2[i]))
        }
        return(unique(return_list)[1:3])
        
      }
    }
  }


  
  
}
