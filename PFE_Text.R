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

##text cleaning
tweets=read.csv("biden1104_1106.csv",header=TRUE)
Encoding(tweets$text) <- "UTF-8"
tweets$text=iconv(tweets$text, "UTF-8", "UTF-8",sub='')
# Remove mentions, hashtag
tweets$text <- gsub("@\\w+", "", tweets$text)
tweets$text <- gsub("#\\w+", "", tweets$text)
tweets$text  <-  gsub("amp", "", tweets$text) ##ampersand encode broken
tweets$text <- gsub("https?://.+", "", tweets$text)
tweets$text <- gsub("\\d+\\w*\\d*", "", tweets$text)
tweets$text <- gsub("[^\x01-\x7F]", "", tweets$text)

##convert to corpus
corpus=Corpus(VectorSource(tweets$text))
##the following steps are data cleaning based on tm package##
##change to lower case
corpus=tm_map(corpus,tolower)
##remove numbers
corpus = tm_map(corpus, removeNumbers)
##remove Punctuation
corpus = tm_map(corpus, removePunctuation)
##remove stopwords such as your, her, the
##use code stopwords("english") to check the list
corpus = tm_map(corpus, removeWords, stopwords("english"))
##remove stopwords in twitter
corpus = tm_map(corpus, removeWords, c("im","dont","doesnt","didnt","hasnt","havent","hadnt","hes","shes","its","lets"))
##remove URL link
removeUrl=function(x) gsub("http[[:alnum:]]*","",x)
corpus = tm_map(corpus, removeUrl)
##remove additional space
corpus = tm_map(corpus, stripWhitespace)

##remove additional words defined by each case
##!!! this is the place you need adjust according to your case
corpus = tm_map(corpus, removeWords, c("UA", "United","airline","united"))


##count the frequency of words##
## convert to term matrix

dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.999)
dataset = as.data.frame(as.matrix(dtm))
##sort words by frequency
v = sort(colSums(dataset),decreasing=TRUE)
## data with words and frequency
d = data.frame(word=names(v),freq=v)

##word in the top 20 in terms of frequency !!! you can change the number here
wordfreq=d[1:30,]

##bar plot
ggplot(data=wordfreq, aes(x=reorder(word, freq), y=freq)) +
  geom_bar(stat = 'identity')+coord_flip()+xlab("word")+ylab("count")
##wordcloud
##word with frequency more than 2 !!!you can change the threshold here
wordfreq=d[d$freq>=2,]
wordcloud2(data=wordfreq,size=1,color = "random-light", shape="circle", backgroundColor = "grey")
##wordcloud option: size--word size; color--word color; shape: shape of wordcloud; 
##backgroundColor: color of background



##sentimental analysis
##sentimental words
##read orignial tweets ##because we have removed some words when we create wordcloud
tweets=read.csv("UA0410.csv",header=TRUE)
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

