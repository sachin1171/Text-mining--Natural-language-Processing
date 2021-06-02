################Task 1 - Amazon################################################

install.packages("rvest")
install.packages("XML")
install.packages("magrittr")

library(rvest)
library(XML)
library(magrittr)

#amazon url
aurl <- "https://www.amazon.in/Airdopes-441-Technology-Immersive-Resistance/product-reviews/B084DS51NC/ref=cm_cr_arp_d_paging_btm_next_2?ie=UTF8&reviewerType=all_reviews&pageNumber=1"

amazon_reviews <- NULL

for (i in 1:30){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>% html_nodes(".review-text") %>% html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}

write.table(amazon_reviews,"BoAtEarbuds.txt")
getwd()

#sentiment analysis

install.packages("tm")
library(tm)

txt <- amazon_reviews

# Convert the character data to corpus type
x <- Corpus(VectorSource(txt))

inspect(x)
head(x)

x <- tm_map(x, function(x) iconv(enc2utf8(x), sub='byte'))

# Data Cleansing
x1 <- tm_map(x, tolower)#converting all characters to lower

x1 <- tm_map(x1, removePunctuation)#removing all punctuations

x1 <- tm_map(x1, removeNumbers)#removing digits

x1 <- tm_map(x1, removeWords, stopwords('english'))#removing stop words

inspect(x1[1])

# striping white spaces 
x1 <- tm_map(x1, stripWhitespace)
inspect(x1[1])

# Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(x1)

dtm <- t(tdm) # transpose
#or
dtm <- DocumentTermMatrix(x1)

# To remove sparse entries upon a specific value
corpus.dtm.frequent <- removeSparseTerms(tdm, 0.99) 

tdm <- as.matrix(tdm)
dim(tdm)

tdm[1:20, 1:20]

# Bar plot
w <- rowSums(tdm)
w
w_sub <- subset(w, w >= 200)
w_sub
barplot(w_sub, las=2, col = rainbow(30))

# few Term can be removable which are related to brand
x1 <- tm_map(x1, removeWords, c('buds', 'boat', 'bud'))
x1 <- tm_map(x1, stripWhitespace)

tdm <- TermDocumentMatrix(x1)

tdm <- as.matrix(tdm)
tdm[200:205, 1:20]

# Bar plot after removal of the term 'buds'
w <- rowSums(tdm)
w
w_sub <- subset(w, w >= 200)
w_sub
barplot(w_sub, las=2, col = rainbow(30))

# Word cloud
install.packages("wordcloud")
library(wordcloud)

wordcloud(words = names(w_sub), freq = w_sub)

w_sub1 <- sort(rowSums(tdm), decreasing = TRUE)
head(w_sub1)

wordcloud(words = names(w_sub1), freq = w_sub1) # all words are considered

# better visualization
windows()
wordcloud(words = names(w_sub1), freq = w_sub1, random.order=F, colors=rainbow(30), scale = c(2,0.5), rot.per = 0.4)

windows()
wordcloud(words = names(w_sub1), freq = w_sub1, random.order=F, colors= rainbow(30),scale=c(3,0.5),rot.per=0.3)


windowsFonts(JP1 = windowsFont("MS Gothic"))
par(family = "JP1")
wordcloud(x1, scale= c(2,0.5))


#Wordcloud2

install.packages("wordcloud2")
library(wordcloud2)

w1 <- data.frame(names(w_sub), w_sub)
colnames(w1) <- c('word', 'freq')

wordcloud2(w1, size=0.3, shape='circle')#visualizing wordcloud as circle
wordcloud2(w1, size=0.3, shape = 'triangle')#visualizing wordcloud as triangle
wordcloud2(w1, size=0.3, shape = 'star')#visualizing wordcloud as star

#Bigram 
install.packages("RWeka")
library(RWeka)
library(wordcloud)

minfreq_bigram <- 2
bitoken <- NGramTokenizer(x1, Weka_control(min = 2, max = 2))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq, decreasing = TRUE), ]

wordcloud(sort_two$bitoken, sort_two$Freq, random.order = F, scale = c(2, 0.35), min.freq = minfreq_bigram, colors = brewer.pal(8, "Dark2"), max.words = 150)

# lOADING Positive and Negative words  
pos.words <- readLines(file.choose())	# read-in positive-words.txt
neg.words <- readLines(file.choose()) 	# read-in negative-words.txt
stopwdrds <-  readLines(file.choose())  # read-in stop-words.txt

### Positive word cloud ###
pos.matches <- match(names(w_sub1), pos.words)
pos.matches <- !is.na(pos.matches)
freq_pos <- w_sub1[pos.matches]
names <- names(freq_pos)
windows()
wordcloud(names, freq_pos, scale=c(4,1), colors = brewer.pal(8,"Dark2"))


### Matching Negative words ###
neg.matches <- match(names(w_sub1), neg.words)
neg.matches <- !is.na(neg.matches)
freq_neg <- w_sub1[neg.matches]
names <- names(freq_neg)
windows()
wordcloud(names, freq_neg, scale=c(4,.5), colors = brewer.pal(8, "Dark2"))

################Task 1 - Snapdeal################################################
install.packages("rvest")
install.packages("XML")
install.packages("magrittr")

library(rvest)
library(XML)
library(magrittr)

#snapdeal url
aurl <- "https://www.snapdeal.com/product/asian-white-mesh-textile-sport/662912850947/reviews?page=2&sortBy=RECENCY#defRevPDP"

snapdeal_reviews <- NULL

for (i in 1:30){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>% html_nodes(".user-review") %>% html_text()
  snapdeal_reviews <- c(snapdeal_reviews,rev)
}

write.table(snapdeal_reviews,"snapdeal.txt")
getwd()

#sentiment analysis

install.packages("tm")
library(tm)

txt <- snapdeal_reviews

# Convert the character data to corpus type
x <- Corpus(VectorSource(txt))

inspect(x[11])
head(x)

x <- tm_map(x, function(x) iconv(enc2utf8(x), sub='byte'))

# Data Cleansing
x1 <- tm_map(x, tolower)#converting all characters to lower

x1 <- tm_map(x1, removePunctuation)#removing all punctuations

x1 <- tm_map(x1, removeNumbers)#removing digits

x1 <- tm_map(x1, removeWords, stopwords('english'))#removing stop words

inspect(x1[11])

# striping white spaces 
x1 <- tm_map(x1, stripWhitespace)
inspect(x1[11])

# Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(x1)

dtm <- t(tdm) # transpose
#or
dtm <- DocumentTermMatrix(x1)

# To remove sparse entries upon a specific value
corpus.dtm.frequent <- removeSparseTerms(tdm, 0.99) 

tdm <- as.matrix(tdm)
dim(tdm)

tdm[10:20,30:50]

# Bar plot
w <- rowSums(tdm)
w
w_sub <- subset(w, w >= 100)
w_sub
barplot(w_sub, las=2, col = rainbow(30))

# few Term can be removable which are related to brand
x1 <- tm_map(x1, removeWords, c('buyer','verified','nov','aug'))
x1 <- tm_map(x1, stripWhitespace)

tdm <- TermDocumentMatrix(x1)

tdm <- as.matrix(tdm)
tdm[10:20,30:50]

# Bar plot after removal of the terms
w <- rowSums(tdm)
w
w_sub <- subset(w, w >= 75)
w_sub
barplot(w_sub, las=2, col = rainbow(30))

# Word cloud
install.packages("wordcloud")
library(wordcloud)

wordcloud(words = names(w_sub), freq = w_sub)

w_sub1 <- sort(rowSums(tdm), decreasing = TRUE)
head(w_sub1)

wordcloud(words = names(w_sub1), freq = w_sub1) # all words are considered

# better visualization
windows()
wordcloud(words = names(w_sub1), freq = w_sub1, random.order=F, colors=rainbow(30), scale = c(2,0.5), rot.per = 0.4)

windows()
wordcloud(words = names(w_sub1), freq = w_sub1, random.order=F, colors= rainbow(30),scale=c(3,0.5),rot.per=0.3)


windowsFonts(JP1 = windowsFont("MS Gothic"))
par(family = "JP1")
wordcloud(x1, scale= c(2,0.5))


#Wordcloud2

install.packages("wordcloud2")
library(wordcloud2)

w1 <- data.frame(names(w_sub), w_sub)
colnames(w1) <- c('word', 'freq')

wordcloud2(w1, size=0.3, shape='circle')#visualizing wordcloud as circle
wordcloud2(w1, size=0.3, shape = 'triangle')#visualizing wordcloud as triangle
wordcloud2(w1, size=0.3, shape = 'star')#visualizing wordcloud as star

#Bigram 
install.packages("RWeka")
library(RWeka)
library(wordcloud)

minfreq_bigram <- 2
bitoken <- NGramTokenizer(x1, Weka_control(min = 2, max = 2))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq, decreasing = TRUE), ]

wordcloud(sort_two$bitoken, sort_two$Freq, random.order = F, scale = c(2, 0.35), min.freq = minfreq_bigram, colors = brewer.pal(8, "Dark2"), max.words = 150)

# lOADING Positive and Negative words  
pos.words <- readLines(file.choose())	# read-in positive-words.txt
neg.words <- readLines(file.choose()) 	# read-in negative-words.txt
stopwdrds <-  readLines(file.choose())  # read-in stop-words.txt

### Positive word cloud ###
pos.matches <- match(names(w_sub1), pos.words)
pos.matches <- !is.na(pos.matches)
freq_pos <- w_sub1[pos.matches]
names <- names(freq_pos)
windows()
wordcloud(names, freq_pos, scale=c(4,1), colors = brewer.pal(8,"Dark2"))


### Matching Negative words ###
neg.matches <- match(names(w_sub1), neg.words)
neg.matches <- !is.na(neg.matches)
freq_neg <- w_sub1[neg.matches]
names <- names(freq_neg)
windows()
wordcloud(names, freq_neg, scale=c(4,.5), colors = brewer.pal(8, "Dark2"))

################Task 2 - IMDB################################################
install.packages("rvest")
install.packages("XML")
install.packages("magrittr")

library(rvest)
library(XML)
library(magrittr)

#IMDB url
aurl <- "https://www.imdb.com/title/tt7286456/reviews?ref_=tt_ov_rt"

imdb_reviews <- NULL

for (i in 1:30){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>% html_nodes(".show-more__control") %>% html_text()
  imdb_reviews <- c(imdb_reviews,rev)
}

write.table(imdb_reviews,"Joker.txt")
getwd()

#sentiment analysis

install.packages("tm")
library(tm)

txt <- imdb_reviews

# Convert the character data to corpus type
x <- Corpus(VectorSource(txt))

inspect(x[1])
head(x)

x <- tm_map(x, function(x) iconv(enc2utf8(x), sub='byte'))

# Data Cleansing
x1 <- tm_map(x, tolower)#converting all characters to lower

x1 <- tm_map(x1, removePunctuation)#removing all punctuations

x1 <- tm_map(x1, removeNumbers)#removing digits

x1 <- tm_map(x1, removeWords, stopwords('english'))#removing stop words

inspect(x1[1])

# striping white spaces 
x1 <- tm_map(x1, stripWhitespace)
inspect(x1[1])

# Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(x1)

dtm <- t(tdm) # transpose
#or
dtm <- DocumentTermMatrix(x1)

# To remove sparse entries upon a specific value
corpus.dtm.frequent <- removeSparseTerms(tdm, 0.99) 

tdm <- as.matrix(tdm)
dim(tdm)

tdm[500:520, 30:50]

# Bar plot
w <- rowSums(tdm)
w
w_sub <- subset(w, w >= 350)
w_sub
barplot(w_sub, las=2, col = rainbow(30))

# few Term can be removable which are related to brand
x1 <- tm_map(x1, removeWords, c('movie','joker','film','just'))
x1 <- tm_map(x1, stripWhitespace)

tdm <- TermDocumentMatrix(x1)

tdm <- as.matrix(tdm)
tdm[200:205, 1:20]

# Bar plot after removal of the terms
w <- rowSums(tdm)
w
w_sub <- subset(w, w >= 300)
w_sub
barplot(w_sub, las=2, col = rainbow(30))

# Word cloud
install.packages("wordcloud")
library(wordcloud)

wordcloud(words = names(w_sub), freq = w_sub)

w_sub1 <- sort(rowSums(tdm), decreasing = TRUE)
head(w_sub1)

wordcloud(words = names(w_sub1), freq = w_sub1) # all words are considered

# better visualization
windows()
wordcloud(words = names(w_sub1), freq = w_sub1, random.order=F, colors=rainbow(30), scale = c(2,0.5), rot.per = 0.4)

windows()
wordcloud(words = names(w_sub1), freq = w_sub1, random.order=F, colors= rainbow(30),scale=c(3,0.5),rot.per=0.3)


windowsFonts(JP1 = windowsFont("MS Gothic"))
par(family = "JP1")
wordcloud(x1, scale= c(2,0.5))


#Wordcloud2

install.packages("wordcloud2")
library(wordcloud2)

w1 <- data.frame(names(w_sub), w_sub)
colnames(w1) <- c('word', 'freq')

wordcloud2(w1, size=0.3, shape='circle')#visualizing wordcloud as circle
wordcloud2(w1, size=0.3, shape = 'triangle')#visualizing wordcloud as triangle
wordcloud2(w1, size=0.3, shape = 'star')#visualizing wordcloud as star

#Bigram 
install.packages("RWeka")
library(RWeka)
library(wordcloud)

minfreq_bigram <- 2
bitoken <- NGramTokenizer(x1, Weka_control(min = 2, max = 2))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq, decreasing = TRUE), ]

wordcloud(sort_two$bitoken, sort_two$Freq, random.order = F, scale = c(2, 0.35), min.freq = minfreq_bigram, colors = brewer.pal(8, "Dark2"), max.words = 150)

# lOADING Positive and Negative words  
pos.words <- readLines(file.choose())	# read-in positive-words.txt
neg.words <- readLines(file.choose()) 	# read-in negative-words.txt
stopwdrds <-  readLines(file.choose())  # read-in stop-words.txt

### Positive word cloud ###
pos.matches <- match(names(w_sub1), pos.words)
pos.matches <- !is.na(pos.matches)
freq_pos <- w_sub1[pos.matches]
names <- names(freq_pos)
windows()
wordcloud(names, freq_pos, scale=c(4,1), colors = brewer.pal(8,"Dark2"))


### Matching Negative words ###
neg.matches <- match(names(w_sub1), neg.words)
neg.matches <- !is.na(neg.matches)
freq_neg <- w_sub1[neg.matches]
names <- names(freq_neg)
windows()
wordcloud(names, freq_neg, scale=c(4,.5), colors = brewer.pal(8, "Dark2"))


#############################Task 2 - hotel review###############################################
install.packages("rvest")
install.packages("XML")
install.packages("magrittr")

library(rvest)
library(XML)
library(magrittr)


hotel_reviews <- read.csv(file.choose())
#file downloaded from : https://www.kaggle.com/andrewmvd/trip-advisor-hotel-reviews
#file is included

hotel_reviews <- hotel_reviews[1]

write.table(hotel_reviews,"hotel_reviews.txt")
getwd()

#sentiment analysis

install.packages("tm")
library(tm)

txt <- hotel_reviews

# Convert the character data to corpus type
x <- Corpus(VectorSource(txt))

#inspect(x[1])
head(x)

x <- tm_map(x, function(x) iconv(enc2utf8(x), sub='byte'))

# Data Cleansing
x1 <- tm_map(x, tolower)#converting all characters to lower

x1 <- tm_map(x1, removePunctuation)#removing all punctuations

x1 <- tm_map(x1, removeNumbers)#removing digits

x1 <- tm_map(x1, removeWords, stopwords('english'))#removing stop words

#inspect(x1[1])

# striping white spaces 
x1 <- tm_map(x1, stripWhitespace)
inspect(x1[1])

# Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(x1)

dtm <- t(tdm) # transpose
#or
dtm <- DocumentTermMatrix(x1)

# To remove sparse entries upon a specific value
corpus.dtm.frequent <- removeSparseTerms(tdm, 0.99) 

tdm <- as.matrix(tdm)
dim(tdm)

tdm[500:520,]

# Bar plot
w <- rowSums(tdm)
w
w_sub <- subset(w, w >= 10000)
w_sub
barplot(w_sub, las=2, col = rainbow(30))

# few Term can be removable which are related to brand
x1 <- tm_map(x1, removeWords, c('hotel','location','room','rooms','stay','stayed'))
x1 <- tm_map(x1, stripWhitespace)

tdm <- TermDocumentMatrix(x1)

tdm <- as.matrix(tdm)
dim(tdm)
tdm[200:250,]

# Bar plot after removal of the terms
w <- rowSums(tdm)
w
w_sub <- subset(w, w >= 7000)
w_sub
barplot(w_sub, las=2, col = rainbow(30))

# Word cloud
install.packages("wordcloud")
library(wordcloud)

wordcloud(words = names(w_sub), freq = w_sub)

w_sub1 <- sort(rowSums(tdm), decreasing = TRUE)
head(w_sub1)

wordcloud(words = names(w_sub1), freq = w_sub1) # all words are considered

# better visualization
windows()
wordcloud(words = names(w_sub1), freq = w_sub1, random.order=F, colors=rainbow(30), scale = c(2,0.5), rot.per = 0.4)

windows()
wordcloud(words = names(w_sub1), freq = w_sub1, random.order=F, colors= rainbow(30),scale=c(3,0.5),rot.per=0.3)


windowsFonts(JP1 = windowsFont("MS Gothic"))
par(family = "JP1")
wordcloud(x1, scale= c(2,0.5))


#Wordcloud2

install.packages("wordcloud2")
library(wordcloud2)

w1 <- data.frame(names(w_sub), w_sub)
colnames(w1) <- c('word', 'freq')

wordcloud2(w1, size=0.3, shape='circle')#visualizing wordcloud as circle
wordcloud2(w1, size=0.3, shape = 'triangle')#visualizing wordcloud as triangle
wordcloud2(w1, size=0.3, shape = 'star')#visualizing wordcloud as star

#Bigram 
install.packages("RWeka")
library(RWeka)
library(wordcloud)

minfreq_bigram <- 2
bitoken <- NGramTokenizer(x1, Weka_control(min = 2, max = 2))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq, decreasing = TRUE), ]

wordcloud(sort_two$bitoken, sort_two$Freq, random.order = F, scale = c(2, 0.35), min.freq = minfreq_bigram, colors = brewer.pal(8, "Dark2"), max.words = 150)

# lOADING Positive and Negative words  
pos.words <- readLines(file.choose())	# read-in positive-words.txt
neg.words <- readLines(file.choose()) 	# read-in negative-words.txt

### Positive word cloud ###
pos.matches <- match(names(w_sub1), pos.words)
pos.matches <- !is.na(pos.matches)
freq_pos <- w_sub1[pos.matches]
names <- names(freq_pos)
windows()
wordcloud(names, freq_pos, scale=c(4,1), colors = brewer.pal(8,"Dark2"))


### Matching Negative words ###
neg.matches <- match(names(w_sub1), neg.words)
neg.matches <- !is.na(neg.matches)
freq_neg <- w_sub1[neg.matches]
names <- names(freq_neg)
windows()
wordcloud(names, freq_neg, scale=c(4,.5), colors = brewer.pal(8, "Dark2"))