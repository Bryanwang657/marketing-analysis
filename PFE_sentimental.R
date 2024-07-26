##you only need to install the packages for the first time
install.packages("tm")
install.packages("stopwords")
install.packages("twitteR")
install.packages("tidytext")
install.packages("ggplot2")
install.packages("syuzhet")
install.packages("lubridate")
install.packages("dplyr")
install.packages("wordcloud2")
install.packages("Rcpp")

###if you have installed the packages before, 
##start to run the code from here
library(tm)
library(twitteR)
library(tidytext)
library(stopwords)
library(ggplot2)
library(syuzhet)
library(lubridate)
library(dplyr)
library(wordcloud2)
library("openssl")
library("httpuv")


##sentimental analysis
##sentimental words
##read orignial tweets ##because we have removed some words when we create wordcloud
tweets=read.csv("UA0409.csv",header=TRUE)
##transfer invalid utf-8 strings
Encoding(tweets$text) <- "UTF-8"
tweets$text=iconv(tweets$text, "UTF-8", "UTF-8",sub='')
##clean characters with no meaning
tweets$text <- gsub("@\\w+", "", tweets$text)
tweets$text <- gsub("#\\w+", "", tweets$text)
tweets$text  <-  gsub("amp", "", tweets$text) ##ampersand encode broken
tweets$text <- gsub("https?://.+", "", tweets$text)
tweets$text <- gsub("\\d+\\w*\\d*", "", tweets$text)
tweets$text <- gsub("[^\x01-\x7F]", "", tweets$text)
##use nrc lexicon
st=get_nrc_sentiment(tweets$text)
barplot(colSums(st)/sum(colSums(st)),
        las=2, 
        col=rainbow(10), ##color of each bar
        ylab="share",  ##label for y axis 
        main="share out of all sentiment words", ##main title
        sub="Case of UA; Date: April 09", ##sub-title !!this is the place you need to adjust
        cex.sub=0.8, ##font size of sub-title
        cex.names=0.9) ##font size of "sentiment"

##histogram of feeling
feeling=get_sentiment(tweets$text, method="syuzhet")
head(feeling)
##change feeling to a dataframe with one variable score
feeling=data.frame(score=feeling)
ggplot(feeling, aes(x=score))+ 
  geom_histogram(binwidth=1, color="black", fill="white")+
  ggtitle("Histogram of Feeling in the Post Mentioning UA \n Date: April 09 ") ##!!you should change the title here
##average feeling
mean(feeling$score)

##show the feeling trend


tweets1=read.csv("UA0409.csv",header=TRUE)
tweets2=read.csv("UA0410.csv",header=TRUE)


tweets=rbind(tweets1,tweets2) ##combine data
##transfer invalid utf-8 strings
Encoding(tweets$text) <- "UTF-8"
tweets$text=iconv(tweets$text, "UTF-8", "UTF-8",sub='')
##text cleaning
tweets$text <- gsub("@\\w+", "", tweets$text)
tweets$text <- gsub("#\\w+", "", tweets$text)
tweets$text  <-  gsub("amp", "", tweets$text) ##ampersand encode broken
tweets$text <- gsub("https?://.+", "", tweets$text)
tweets$text <- gsub("\\d+\\w*\\d*", "", tweets$text)
tweets$text <- gsub("[^\x01-\x7F]", "", tweets$text)
feeling=get_sentiment(tweets$text, method="syuzhet")

##average feeling over time
##create a dataframe with variables: score and date
sentiment_df=data.frame(score=feeling, date=as.Date(tweets$created))
##aggregate feeling over date: calcualte average feeling for each date
sentiment_agg=aggregate(sentiment_df$score,list(sentiment_df$date),mean)
##rename the colnames to date and will_feeling
colnames(sentiment_agg)=c("date","feeling")
head(sentiment_agg)
##line plot 
ggplot(data=sentiment_agg,aes(x=date,y=feeling,group=1))+geom_line()


