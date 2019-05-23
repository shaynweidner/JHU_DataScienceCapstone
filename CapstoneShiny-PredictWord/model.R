
#Start actual prediction work
#setwd("~/JHU_DataScienceCapstone/Data/")
library(stringr)
library(tm)
library(RWeka)
library(slam)
library(dplyr)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2, delimiters = " "))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3, delimiters = " "))
FourgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4, delimiters = " "))

load(file="Data/BiGramsComplete.Rda")
load(file="Data/TriGramsComplete.Rda")
load(file="Data/FourGramsComplete.Rda")
PredictFunction <- function(text = "I",type="Twitter"){
  #dumb back off degradation (not as sophisticated as Katz)
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
