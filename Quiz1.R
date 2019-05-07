
con <- file("Data/en_US/en_US.twitter.txt", "rb")
twitterData <- readLines(con)
close(con)
con <- file("Data/en_US/en_US.blogs.txt", "rb")
blogData <- readLines(con)
close(con)
con <- file("Data/en_US/en_US.news.txt", "rb")
newsData <- readLines(con)
close(con)

summary(nchar(twitterData))
summary(nchar(blogData))
summary(nchar(newsData))

grepl("love",twitterData)

sum(grepl("love",twitterData)) / sum(grepl("hate",twitterData))

twitterData[which(grepl("biostats",twitterData))]