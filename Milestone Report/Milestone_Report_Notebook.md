---
title: "JHU Data Science Capstone: Milestone Report"
author: "Shayn Weidner"
date: "May 6, 2019"
output:
  html_document:
    keep_md: true
---

#Preface

I'm starting this milestone report without really knowing what is expected of it, because it won't be unlocked until 5/13, and today is 5/6, and honestly, who really wants to wait for a week to see what you're supposed to do next.  I'm guessing they want us to clean the data and perform some EDA, and it's a milestone so you probably need a plan for what else you'll do (and hey, the non-peer-reviewed tasks can be completed as soon as I want, so I have a good idea of what is expected of us for the final product) but if I'm wrong I'll make some corrections/additions.

#Introduction
The goal of this capstone is to create a predictive text model; that is, we will try to predict the next word in a sequence given the word (or words) preceding it.  In order to develop our predictive text model, we will be using a large corpus of twitter, blog, and news text that has been given to us from [here](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip).  Within this compressed folder were several files, some in different languages.  The ones we are concerned with are the english language twitter, blog, and news files:


```r
setwd("~/JHU_DataScienceCapstone")
con <- file("Data/en_US/en_US.twitter.txt", "rb")
twitterData <- readLines(con)
close(con)
con <- file("Data/en_US/en_US.blogs.txt", "rb")
blogData <- readLines(con)
close(con)
con <- file("Data/en_US/en_US.news.txt", "rb")
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
## [1] "In the years thereafter, most of the Oil fields and platforms were named after pagan â\200œgodsâ\200\235."
## [2] "We love you Mr. Brown."
```

```r
head(newsData,2)
```

```
## [1] "He wasn't home alone, apparently."                                                                                                                        
## [2] "The St. Louis plant had to close. It would die of old age. Workers had been making cars there since the onset of mass automotive production in the 1920s."
```


I had to use the binary read ("rb") setting in *file()*, because it threw an error when trying to read from it in normal read mode.
