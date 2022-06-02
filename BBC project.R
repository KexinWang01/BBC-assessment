#Setup the working directory
setwd("~/Desktop/Columbia_University/Job Application/Technical Interview/BBC/bbc 2")

#install and load packages
install.packages(c("plyr", "readr"))
library(plyr)
library(readr)
library(stringr)
library(dplyr)
library(data.table)

#Load data
myfolder = "entertainment"
entertainment_bbc = list.files(path=myfolder, pattern = "*.txt", full.names = T, recursive = TRUE)
entertainment_bbc
entertainment_data = ldply(entertainment_bbc, read_file)
entertainment_data

myfolder = "business"
business_bbc = list.files(path=myfolder, pattern = "*.txt", full.names = T, recursive = TRUE)
business_bbc
business_data = ldply(business_bbc, read_file)
business_data

myfolder = "politics"
politics_bbc = list.files(path=myfolder, pattern = "*.txt", full.names = T, recursive = TRUE)
politics_bbc
politics_data = ldply(politics_bbc, read_file)
politics_data 

myfolder = "sport"
sport_bbc = list.files(path=myfolder, pattern = "*.txt", full.names = T, recursive = TRUE)
sport_bbc
sport_data = ldply(sport_bbc, read_file)
sport_data

myfolder = "tech"
tech_bbc = list.files(path=myfolder, pattern = "*.txt", full.names = T, recursive = TRUE)
tech_bbc
tech_data = ldply(tech_bbc, read_file)
tech_data

#Change column names
colnames(entertainment_data) <- 'Contents'
colnames(business_data) <- 'Contents'
colnames(politics_data) <- 'Contents'
colnames(sport_data) <- 'Contents'
colnames(tech_data) <- 'Contents'


#Entertainment data
#Clean and Tokenize
library(tm); library(SnowballC); library(magrittr)
corpus_e = Corpus(VectorSource(entertainment_data$Contents))
corpus_e = 
  corpus_e %>%
  tm_map(content_transformer(tolower))%>%
  tm_map(content_transformer(FUN = function(x)gsub(pattern = 'http[[:alnum:][:punct:]]*', replacement = ' ',x = x)))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeWords, c(stopwords('english')))
corpus_e[[1]][1]
  
dict_e = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(entertainment_data$Contents))), lowfreq = 0)
dict_corpus_e = Corpus(VectorSource(dict_e))

corpus_e = 
  corpus_e %>%
  tm_map(stemDocument) %>%
  tm_map(stripWhitespace)
corpus_e[[1]][1]

#Create tf (document term matrix)
dtm_e = DocumentTermMatrix(corpus_e)
xdtm_e = removeSparseTerms(dtm_e, sparse = 0.95)
xdtm_e = as.data.frame(as.matrix(xdtm_e))
colnames(xdtm_e) = stemCompletion(x = colnames(xdtm_e), dictionary = dict_corpus_e, type = 'prevalent')
colnames(xdtm_e) = make.names(colnames(xdtm_e))
sort(colSums(xdtm_e), decreasing = T)

#Create tfidf (document term matrix)
dtm_tfidf_e = DocumentTermMatrix(x=corpus_e,control = list(weighting=function(x) weightTfIdf(x,normalize=F)))
xdtm_tfidf_e = removeSparseTerms(dtm_tfidf_e,sparse = 0.95)
xdtm_tfidf_e = as.data.frame(as.matrix(xdtm_tfidf_e))
colnames(xdtm_tfidf_e) = stemCompletion(x = colnames(xdtm_tfidf_e),
                                      dictionary = dict_corpus_e,
                                      type='prevalent')
colnames(xdtm_tfidf_e) = make.names(colnames(xdtm_tfidf_e))
sort(colSums(xdtm_tfidf_e), decreasing = T)

library(tidyr); library(dplyr); library(ggplot2); library(ggthemes)
data.frame(term = colnames(xdtm_e),tf = colMeans(xdtm_e), tfidf = colMeans(xdtm_tfidf_e))%>%
  arrange(desc(tf))%>%
  top_n(20)%>%
  gather(key=weighting_method,value=weight,2:3)%>%
  ggplot(aes(x=term,y=weight,fill=weighting_method))+
  geom_col(position='dodge')+
  coord_flip()+
  theme_economist()
#From the graph, we can see the most common topics in entertainment data is the "best", "film", "music", "award", "band", "song", "show", "album", "oscar", etc..


#Business data
#Clean and Tokenize
library(tm); library(SnowballC); library(magrittr)
corpus_b = Corpus(VectorSource(business_data$Contents))
corpus_b = 
  corpus_b %>%
  tm_map(content_transformer(tolower))%>%
  tm_map(content_transformer(FUN = function(x)gsub(pattern = 'http[[:alnum:][:punct:]]*', replacement = ' ',x = x)))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeWords, c(stopwords('english')))
corpus_b[[1]][1]

dict_b = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(business_data$Contents))), lowfreq = 0)
dict_corpus_b = Corpus(VectorSource(dict_b))

corpus_b = 
  corpus_b %>%
  tm_map(stemDocument) %>%
  tm_map(stripWhitespace)
corpus_b[[1]][1]

#Create tf (document term matrix)
dtm_b = DocumentTermMatrix(corpus_b)
xdtm_b = removeSparseTerms(dtm_b, sparse = 0.95)
xdtm_b = as.data.frame(as.matrix(xdtm_b))
colnames(xdtm_b) = stemCompletion(x = colnames(xdtm_b), dictionary = dict_corpus_b, type = 'prevalent')
colnames(xdtm_b) = make.names(colnames(xdtm_b))
sort(colSums(xdtm_b), decreasing = T)

#Create tfidf (document term matrix)
dtm_tfidf_b = DocumentTermMatrix(x=corpus_b,control = list(weighting=function(x) weightTfIdf(x,normalize=F)))
xdtm_tfidf_b = removeSparseTerms(dtm_tfidf_b,sparse = 0.95)
xdtm_tfidf_b = as.data.frame(as.matrix(xdtm_tfidf_b))
colnames(xdtm_tfidf_b) = stemCompletion(x = colnames(xdtm_tfidf_b),
                                        dictionary = dict_corpus_b,
                                        type='prevalent')
colnames(xdtm_tfidf_b) = make.names(colnames(xdtm_tfidf_b))
sort(colSums(xdtm_tfidf_b), decreasing = T)

library(tidyr); library(dplyr); library(ggplot2); library(ggthemes)
data.frame(term = colnames(xdtm_b),tf = colMeans(xdtm_b), tfidf = colMeans(xdtm_tfidf_b))%>%
  arrange(desc(tf))%>%
  top_n(20)%>%
  gather(key=weighting_method,value=weight,2:3)%>%
  ggplot(aes(x=term,y=weight,fill=weighting_method))+
  geom_col(position='dodge')+
  coord_flip()+
  theme_economist()
#From the output, we can see the most common topics in business data are "yukon.", "bank", "sale", "oil", "rate", "economic", "china", "price", "share", "firm", "market", etc..


#Politics data
#Clean and Tokenize
library(tm); library(SnowballC); library(magrittr)
corpus_p = Corpus(VectorSource(politics_data$Contents))
corpus_p = 
  corpus_p %>%
  tm_map(content_transformer(tolower))%>%
  tm_map(content_transformer(FUN = function(x)gsub(pattern = 'http[[:alnum:][:punct:]]*', replacement = ' ',x = x)))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeWords, c(stopwords('english')))
corpus_p[[1]][1]

dict_p = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(politics_data$Contents))), lowfreq = 0)
dict_corpus_p = Corpus(VectorSource(dict_p))

corpus_p = 
  corpus_p %>%
  tm_map(stemDocument) %>%
  tm_map(stripWhitespace)
corpus_p[[1]][1]

#Create tf (document term matrix)
dtm_p = DocumentTermMatrix(corpus_p)
xdtm_p = removeSparseTerms(dtm_p, sparse = 0.95)
xdtm_p = as.data.frame(as.matrix(xdtm_p))
colnames(xdtm_p) = stemCompletion(x = colnames(xdtm_p), dictionary = dict_corpus_p, type = 'prevalent')
colnames(xdtm_p) = make.names(colnames(xdtm_p))
sort(colSums(xdtm_p), decreasing = T)

#Create tfidf (document term matrix)
dtm_tfidf_p = DocumentTermMatrix(x=corpus_p,control = list(weighting=function(x) weightTfIdf(x,normalize=F)))
xdtm_tfidf_p = removeSparseTerms(dtm_tfidf_p,sparse = 0.95)
xdtm_tfidf_p = as.data.frame(as.matrix(xdtm_tfidf_p))
colnames(xdtm_tfidf_p) = stemCompletion(x = colnames(xdtm_tfidf_p),
                                        dictionary = dict_corpus_p,
                                        type='prevalent')
colnames(xdtm_tfidf_p) = make.names(colnames(xdtm_tfidf_p))
sort(colSums(xdtm_tfidf_p), decreasing = T)

library(tidyr); library(dplyr); library(ggplot2); library(ggthemes)
data.frame(term = colnames(xdtm_p),tf = colMeans(xdtm_p), tfidf = colMeans(xdtm_tfidf_p))%>%
  arrange(desc(tf))%>%
  top_n(20)%>%
  gather(key=weighting_method,value=weight,2:3)%>%
  ggplot(aes(x=term,y=weight,fill=weighting_method))+
  geom_col(position='dodge')+
  coord_flip()+
  theme_economist()
#From the graph, we can see the most common topics in politics data are "brown", "lord", "tax", "blair", "labour", "partial", "elect", “howard", "tories", "police", etc..


#Sport data
#Clean and Tokenize
library(tm); library(SnowballC); library(magrittr)
corpus_s = Corpus(VectorSource(sport_data$Contents))
corpus_s = 
  corpus_s %>%
  tm_map(content_transformer(tolower))%>%
  tm_map(content_transformer(FUN = function(x)gsub(pattern = 'http[[:alnum:][:punct:]]*', replacement = ' ',x = x)))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeWords, c(stopwords('english')))
corpus_s[[1]][1]

dict_s = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(sport_data$Contents))), lowfreq = 0)
dict_corpus_s = Corpus(VectorSource(dict_s))

corpus_s = 
  corpus_s %>%
  tm_map(stemDocument) %>%
  tm_map(stripWhitespace)
corpus_s[[1]][1]

#Create tf (document term matrix)
dtm_s = DocumentTermMatrix(corpus_s)
xdtm_s = removeSparseTerms(dtm_s, sparse = 0.95)
xdtm_s = as.data.frame(as.matrix(xdtm_s))
colnames(xdtm_s) = stemCompletion(x = colnames(xdtm_s), dictionary = dict_corpus_s, type = 'prevalent')
colnames(xdtm_s) = make.names(colnames(xdtm_s))
sort(colSums(xdtm_s), decreasing = T)

#Create tfidf (document term matrix)
dtm_tfidf_s = DocumentTermMatrix(x=corpus_s,control = list(weighting=function(x) weightTfIdf(x,normalize=F)))
xdtm_tfidf_s = removeSparseTerms(dtm_tfidf_s,sparse = 0.95)
xdtm_tfidf_s = as.data.frame(as.matrix(xdtm_tfidf_s))
colnames(xdtm_tfidf_s) = stemCompletion(x = colnames(xdtm_tfidf_s),
                                        dictionary = dict_corpus_s,
                                        type='prevalent')
colnames(xdtm_tfidf_s) = make.names(colnames(xdtm_tfidf_s))
sort(colSums(xdtm_tfidf_s), decreasing = T)

library(tidyr); library(dplyr); library(ggplot2); library(ggthemes)
data.frame(term = colnames(xdtm_s),tf = colMeans(xdtm_s), tfidf = colMeans(xdtm_tfidf_s))%>%
  arrange(desc(tf))%>%
  top_n(20)%>%
  gather(key=weighting_method,value=weight,2:3)%>%
  ggplot(aes(x=term,y=weight,fill=weighting_method))+
  geom_col(position='dodge')+
  coord_flip()+
  theme_economist()
#From the graph, we can see the most common topics in sport data are "england", "wales", "ireland", "chelsea", "X", "club", "liverpool", "play", "game", "player", "win", etc..

#Tech data
#Clean and Tokenize
library(tm); library(SnowballC); library(magrittr)
corpus_t = Corpus(VectorSource(tech_data$Contents))
corpus_t = 
  corpus_t %>%
  tm_map(content_transformer(tolower))%>%
  tm_map(content_transformer(FUN = function(x)gsub(pattern = 'http[[:alnum:][:punct:]]*', replacement = ' ',x = x)))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeWords, c(stopwords('english')))
corpus_t[[1]][1]

dict_t = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(tech_data$Contents))), lowfreq = 0)
dict_corpus_t = Corpus(VectorSource(dict_t))

corpus_t = 
  corpus_t %>%
  tm_map(stemDocument) %>%
  tm_map(stripWhitespace)
corpus_t[[1]][1]

#Create tf (document term matrix)
dtm_t = DocumentTermMatrix(corpus_t)
xdtm_t = removeSparseTerms(dtm_t, sparse = 0.95)
xdtm_t = as.data.frame(as.matrix(xdtm_t))
colnames(xdtm_t) = stemCompletion(x = colnames(xdtm_t), dictionary = dict_corpus_t, type = 'prevalent')
colnames(xdtm_t) = make.names(colnames(xdtm_t))
sort(colSums(xdtm_t), decreasing = T)

#Create tfidf (document term matrix)
dtm_tfidf_t = DocumentTermMatrix(x=corpus_t,control = list(weighting=function(x) weightTfIdf(x,normalize=F)))
xdtm_tfidf_t = removeSparseTerms(dtm_tfidf_t,sparse = 0.95)
xdtm_tfidf_t = as.data.frame(as.matrix(xdtm_tfidf_t))
colnames(xdtm_tfidf_t) = stemCompletion(x = colnames(xdtm_tfidf_t),
                                        dictionary = dict_corpus_t,
                                        type='prevalent')
colnames(xdtm_tfidf_t) = make.names(colnames(xdtm_tfidf_t))
sort(colSums(xdtm_tfidf_t), decreasing = T)

library(tidyr); library(dplyr); library(ggplot2); library(ggthemes)
data.frame(term = colnames(xdtm_t),tf = colMeans(xdtm_t), tfidf = colMeans(xdtm_tfidf_t))%>%
  arrange(desc(tf))%>%
  top_n(20)%>%
  gather(key=weighting_method,value=weight,2:3)%>%
  ggplot(aes(x=term,y=weight,fill=weighting_method))+
  geom_col(position='dodge')+
  coord_flip()+
  theme_economist()
#From the graph, we can see the most common topics in tech data are "game", "mobile", "phone", "search", "site", "music", "blog", "service", "broadband", "microsoft", etc..

#The articles that talked about G20 countries.
fileNames <- Sys.glob(file.path("~/Desktop/Columbia_University/Job Application/Technical Interview/BBC/bbc 2", "*", "*.txt"))
fileNames
G20_file <- c()
for (filename in fileNames) {
  if (length(grep("Argentina|Argentine|Australia|Brazil|Canada|France|French|Germany|India|Indonesia|Italy|Italian|Japan|Korea|Mexico|Russia|Saudi Arabia|South Africa|Turkey|United Kingdom|UK|Britain|United States of America|USA|U.S.A|United States|US|U.S.|America|European Union|EU", readLines(filename))) > 0){ 
    print(filename)
    G20_file[filename] <- filename
  }
}
head(G20_file)
ncol(G20_file)

