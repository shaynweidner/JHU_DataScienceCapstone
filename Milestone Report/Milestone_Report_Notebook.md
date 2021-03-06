---
title: "JHU Data Science Capstone: Milestone Report"
author: "Shayn Weidner"
date: "May 13, 2019"
output:
  html_document:
    keep_md: true
---


# Introduction
The goal of this capstone is to create a predictive text model; that is, we will try to predict the next word in a sequence given the word (or words) preceding it.  In order to develop our predictive text model, we will be using a large corpus of twitter, blog, and news text that has been given to us from [here](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip).  Within this compressed folder were several files, some in different languages.  The ones we are concerned with are the english language twitter, blog, and news files.  The goals for this Milestone Report are to:

1. Demonstrate that you've downloaded the data and have successfully loaded it in.
2. Create a basic report of summary statistics about the data sets.
3. Report any interesting findings that you amassed so far.
4. Get feedback on your plans for creating a prediction algorithm and Shiny app.

We are to be graded based on:

1. Does the link lead to an HTML page describing the exploratory analysis of the training data set?
2. Has the data scientist done basic summaries of the three files? Word counts, line counts and basic data tables?
3. Has the data scientist made basic plots, such as histograms to illustrate features of the data?
4. Was the report written in a brief, concise style, in a way that a non-data scientist manager could appreciate?

# Analysis
## Pull in data

```r
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
head(twitterData,2)
```

```
## [1] "How are you? Btw thanks for the RT. You gonna be in DC anytime soon? Love to see you. Been way, way too long."  
## [2] "When you meet someone special... you'll know. Your heart will beat more rapidly and you'll smile for no reason."
```

```r
head(blogData,2)
```

```
## [1] "In the years thereafter, most of the Oil fields and platforms were named after pagan �\200�gods�\200\235."
## [2] "We love you Mr. Brown."
```

```r
head(newsData,2)
```

```
## [1] "He wasn't home alone, apparently."                                                                                                                        
## [2] "The St. Louis plant had to close. It would die of old age. Workers had been making cars there since the onset of mass automotive production in the 1920s."
```


I had to use the binary read ("rb") setting in *file()*, because it threw an error when trying to read some of the data in normal read mode.

Interesting that the news and blog corpora items are as short as they are...I wonder if they broke up entire articles/posts into paragraphs.  At any rate, let's continue with the EDA.

## Exploratory Data Analysis

### Summary Statistics

In all honesty, I don't find the summary statistics very useful here, but we're being graded based on whether we have some, so I'm throwing word count and line count in there

```r
library(stringr)
docCounts = data.frame(Media=c("Twitter"
                          ,"Blog"
                          ,"News")
                        ,Word_Count=c(sum(stringr::str_count(twitterData, "\\S+"))
                           ,sum(stringr::str_count(blogData, "\\S+"))
                           ,sum(stringr::str_count(newsData, "\\S+"))
                           )
                       )
docCounts$Line_Count = c(length(twitterData)
                         ,length(blogData)
                         ,length(newsData))
docCounts
```

```
##     Media Word_Count Line_Count
## 1 Twitter   30373792    2360148
## 2    Blog   37334441     899288
## 3    News   34372598    1010242
```

So we have more tweets to build our model off of than News, and we have fewer blogs than the other two.  Surprisingly the word count is similar between the three different media types

### Unigrams
I want to see the most common words without any stemming/lemmatization/stopword-removal (sampling down because my machine can't handle all of those documents).  Let's get those words first:


```r
library(tm)
library(slam)
set.seed(5-6-2019)
twitterData <- twitterData[sample(1:length(twitterData),20000)]
blogData <- blogData[sample(1:length(blogData),20000)]
newsData <- newsData[sample(1:length(newsData),20000)]

corpusTwitter <- VCorpus(VectorSource(twitterData))
corpusBlog <- VCorpus(VectorSource(blogData))
corpusNews <- VCorpus(VectorSource(newsData))

DTMtwitter <- DocumentTermMatrix(corpusTwitter
                                 , control = list(tokenize="words"
                                                  ,removePunctuation = FALSE
                                                  ,stopwords = FALSE
                                                  ,stemming = FALSE
                                                  ,tolower = FALSE
                                                  )
                                 )
DTMblog <- DocumentTermMatrix(corpusBlog
                                 , control = list(tokenize="words"
                                                  ,removePunctuation = FALSE
                                                  ,stopwords = FALSE
                                                  ,stemming = FALSE
                                                  ,tolower = FALSE
                                                  )
                                 )
DTMnews <- DocumentTermMatrix(corpusNews
                                 , control = list(tokenize="words"
                                                  ,removePunctuation = FALSE
                                                  ,stopwords = FALSE
                                                  ,stemming = FALSE
                                                  ,tolower = FALSE
                                                  )
                                 )

twitterCounts <- data.frame(col_sums(DTMtwitter))
twitterCounts$term <- row.names(twitterCounts)
blogCounts <- data.frame(col_sums(DTMblog))
blogCounts$term <- row.names(blogCounts)
newsCounts <- data.frame(col_sums(DTMnews))
newsCounts$term <- row.names(newsCounts)

twitterCounts <- twitterCounts[order(twitterCounts$col_sums.DTMtwitter., decreasing = TRUE),]
blogCounts <- blogCounts[order(blogCounts$col_sums.DTMblog., decreasing = TRUE),]
newsCounts <- newsCounts[order(newsCounts$col_sums.DTMnews., decreasing = TRUE),]

twitterCounts <- twitterCounts[1:30,]
twitterCounts$term <- factor(twitterCounts$term, levels = twitterCounts$term)
blogCounts <- blogCounts[1:30,]
blogCounts$term <- factor(blogCounts$term, levels = blogCounts$term)
newsCounts <- newsCounts[1:30,]
newsCounts$term <- factor(newsCounts$term, levels = newsCounts$term)
```

And the histograms of 30 most common words:


```r
library(ggplot2)
print(ggplot(data = twitterCounts, aes(x=term, y=col_sums.DTMtwitter.)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Word") + ylab("Freq in Sample") + ggtitle("Twitter Word Frequency"))
```

![](Milestone_Report_Notebook_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
print(ggplot(data = blogCounts, aes(x=term, y=col_sums.DTMblog.)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Word") + ylab("Freq in Sample") + ggtitle("Blog Word Frequency"))
```

![](Milestone_Report_Notebook_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

```r
print(ggplot(data = newsCounts, aes(x=term, y=col_sums.DTMnews.)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Word") + ylab("Freq in Sample") + ggtitle("News Word Frequency"))
```

![](Milestone_Report_Notebook_files/figure-html/unnamed-chunk-4-3.png)<!-- -->


The results look logical: "the", "and", "for", "that", and "with" should all appear more oftan than the others, and "you" should be much more common in tweets than in blogs, and much less common in news articles than tweets or blogs.  So separate models for (or a model that accounts for) the different sources could be better than a model that doesn't distinguish between the different texts.  Next let's clean up the corpora, and we'll look at bigrams and trigrams.

I initially was going to remove punctuation, remove numbers, lowercase everything, and lemmatize.  But I've decided against everything except for removing extra whitespace because we'll eventually want to predict what comes after numbers/punctuation as well as predict them after other words (I'm leaving the code here just to show that I'm not lazy.  For instance, we don't want to predict a lemmatized word, because that often won't make sense.  We also would want to predict a capitalized word after a period.  So I'll do my bigram/trigram analysis without that pre-processing done:


```r
library(textstem)
#corpusTwitter <- tm_map(corpusTwitter, removePunctuation)
#corpusTwitter <- tm_map(corpusTwitter, removeNumbers)
#corpusTwitter <- tm_map(corpusTwitter, content_transformer(tolower))
#corpusTwitter <- tm_map(corpusTwitter, removeWords, stopwords("en"))
corpusTwitter <- tm_map(corpusTwitter, stripWhitespace)
#corpusTwitter <- tm_map(corpusTwitter, content_transformer(lemmatize_words))

#corpusBlog <- tm_map(corpusBlog, removePunctuation)
#corpusBlog <- tm_map(corpusBlog, removeNumbers)
#corpusBlog <- tm_map(corpusBlog, content_transformer(tolower))
#corpusBlog <- tm_map(corpusBlog, removeWords, stopwords("en"))
corpusBlog <- tm_map(corpusBlog, stripWhitespace)
#corpusBlog <- tm_map(corpusBlog, content_transformer(lemmatize_words))

#corpusNews <- tm_map(corpusNews, removePunctuation)
#corpusNews <- tm_map(corpusNews, removeNumbers)
#corpusNews <- tm_map(corpusNews, content_transformer(tolower))
#corpusNews <- tm_map(corpusNews, removeWords, stopwords("en"))
corpusNews <- tm_map(corpusNews, stripWhitespace)
#corpusNews <- tm_map(corpusNews, content_transformer(lemmatize_words))
```

Now let's look at the bi- and trigrams after cleaning (I don't expect the top unigrams to change much since the top 30 in each document class were lower case and didn't have punctuation.  I grabbed the *BigramTokenizer()* and *TrigramTokenizer()* function ideas from someone on stackoverflow:


```r
library("RWeka")

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2, delimiters = " "))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3, delimiters = " "))

DTMtwitter <- DocumentTermMatrix(corpusTwitter, control=list(tokenize=BigramTokenizer
                                                  ,removePunctuation = FALSE
                                                  ,stopwords = FALSE
                                                  ,stemming = FALSE
                                                  ,tolower = FALSE
                                                  )
                                 )
DTMblog <- DocumentTermMatrix(corpusBlog, control=list(tokenize=BigramTokenizer
                                                  ,removePunctuation = FALSE
                                                  ,stopwords = FALSE
                                                  ,stemming = FALSE
                                                  ,tolower = FALSE
                                                  )
                                 )
DTMnews <- DocumentTermMatrix(corpusNews, control=list(tokenize=BigramTokenizer
                                                  ,removePunctuation = FALSE
                                                  ,stopwords = FALSE
                                                  ,stemming = FALSE
                                                  ,tolower = FALSE
                                                  )
                                 )

twitterCounts <- data.frame(col_sums(DTMtwitter))
twitterCounts$term <- row.names(twitterCounts)
blogCounts <- data.frame(col_sums(DTMblog))
blogCounts$term <- row.names(blogCounts)
newsCounts <- data.frame(col_sums(DTMnews))
newsCounts$term <- row.names(newsCounts)

twitterCounts <- twitterCounts[order(twitterCounts$col_sums.DTMtwitter., decreasing = TRUE),]
blogCounts <- blogCounts[order(blogCounts$col_sums.DTMblog., decreasing = TRUE),]
newsCounts <- newsCounts[order(newsCounts$col_sums.DTMnews., decreasing = TRUE),]

twitterCounts <- twitterCounts[1:30,]
twitterCounts$term <- factor(twitterCounts$term, levels = twitterCounts$term)
blogCounts <- blogCounts[1:30,]
blogCounts$term <- factor(blogCounts$term, levels = blogCounts$term)
newsCounts <- newsCounts[1:30,]
newsCounts$term <- factor(newsCounts$term, levels = newsCounts$term)

print(ggplot(data = twitterCounts, aes(x=term, y=col_sums.DTMtwitter.)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Bigram") + ylab("Freq in Sample") + ggtitle("Twitter Bigram Frequency"))
```

![](Milestone_Report_Notebook_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
print(ggplot(data = blogCounts, aes(x=term, y=col_sums.DTMblog.)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Bigram") + ylab("Freq in Sample") + ggtitle("Blog Bigram Frequency"))
```

![](Milestone_Report_Notebook_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

```r
print(ggplot(data = newsCounts, aes(x=term, y=col_sums.DTMnews.)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Bigram") + ylab("Freq in Sample") + ggtitle("News Bigram Frequency"))
```

![](Milestone_Report_Notebook_files/figure-html/unnamed-chunk-6-3.png)<!-- -->

```r
DTMtwitter <- DocumentTermMatrix(corpusTwitter, control=list(tokenize=TrigramTokenizer
                                                  ,removePunctuation = FALSE
                                                  ,stopwords = FALSE
                                                  ,stemming = FALSE
                                                  ,tolower = FALSE
                                                  )
                                 )
DTMblog <- DocumentTermMatrix(corpusBlog, control=list(tokenize=TrigramTokenizer
                                                  ,removePunctuation = FALSE
                                                  ,stopwords = FALSE
                                                  ,stemming = FALSE
                                                  ,tolower = FALSE
                                                  )
                                 )
DTMnews <- DocumentTermMatrix(corpusNews, control=list(tokenize=TrigramTokenizer
                                                  ,removePunctuation = FALSE
                                                  ,stopwords = FALSE
                                                  ,stemming = FALSE
                                                  ,tolower = FALSE
                                                  )
                                 )

twitterCounts <- data.frame(col_sums(DTMtwitter))
twitterCounts$term <- row.names(twitterCounts)
blogCounts <- data.frame(col_sums(DTMblog))
blogCounts$term <- row.names(blogCounts)
newsCounts <- data.frame(col_sums(DTMnews))
newsCounts$term <- row.names(newsCounts)

twitterCounts <- twitterCounts[order(twitterCounts$col_sums.DTMtwitter., decreasing = TRUE),]
blogCounts <- blogCounts[order(blogCounts$col_sums.DTMblog., decreasing = TRUE),]
newsCounts <- newsCounts[order(newsCounts$col_sums.DTMnews., decreasing = TRUE),]

twitterCounts <- twitterCounts[1:30,]
twitterCounts$term <- factor(twitterCounts$term, levels = twitterCounts$term)
blogCounts <- blogCounts[1:30,]
blogCounts$term <- factor(blogCounts$term, levels = blogCounts$term)
newsCounts <- newsCounts[1:30,]
newsCounts$term <- factor(newsCounts$term, levels = newsCounts$term)

print(ggplot(data = twitterCounts, aes(x=term, y=col_sums.DTMtwitter.)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Trigram") + ylab("Freq in Sample") + ggtitle("Twitter Trigram Frequency"))
```

![](Milestone_Report_Notebook_files/figure-html/unnamed-chunk-6-4.png)<!-- -->

```r
print(ggplot(data = blogCounts, aes(x=term, y=col_sums.DTMblog.)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Trigram") + ylab("Freq in Sample") + ggtitle("Blog Trigram Frequency"))
```

![](Milestone_Report_Notebook_files/figure-html/unnamed-chunk-6-5.png)<!-- -->

```r
print(ggplot(data = newsCounts, aes(x=term, y=col_sums.DTMnews.)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Trigram") + ylab("Freq in Sample") + ggtitle("News Trigram Frequency"))
```

![](Milestone_Report_Notebook_files/figure-html/unnamed-chunk-6-6.png)<!-- -->

While there are a lot of similarities in the results between media, there are enough differences that I will build separate predictions depending on the medium.  Twitter posts are limited to character counts, so of course they'll have different writing styles!  Also, twitter posts tend to be much more personal than blog posts and news articles.  And we should seek to predict those differences.

## Next steps
The end goal for this capstone (based on what I've gathered from watching future weeks' videos and readings) is to develop a prediction model to predict the next word in a sequence of text, deploy it to shiny, and also develop a small slidify presentation to pitch it.  My goal is to implemnt a backoff model since I've never done that before and trying new things is waesome, but I'll try some other things along the way, and maybe that's what will make it into the final model.  Ultimately, I've also got to consider the processing limitations of the free shiny server I'll be using (who knows, maybe I'll stand up my own server to host it there!).
