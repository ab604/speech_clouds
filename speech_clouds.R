# Wordcloud from politico speeches
# A.Bailey 4th October 2017

# Load libs --------------------------------------------------------------------
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(dplyr)

# Read in speeches -------------------------------------------------------------
corb <- readLines("corbyn.txt")
may <- readLines("may.txt")

speechtext <- list(corb,may)

docs <- Corpus(VectorSource(speechtext)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(tolower)  %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(removeWords, c("labour","conservative","tories","tory")) %>% 
  tm_map(stripWhitespace) %>%
  tm_map(PlainTextDocument)

tdm <- TermDocumentMatrix(docs) %>%
  as.matrix()
colnames(tdm) <- c("Corbyn","May")

head(tdm)

corbtdm <- as.matrix(tdm[,1])
corbtdm <- as.matrix(corbtdm[order(corbtdm,decreasing = TRUE),])

maytdm <- as.matrix(tdm[,2])
maytdm <- as.matrix(maytdm[order(maytdm,decreasing = TRUE),])

# Create matrices --------------------------------------------------------------
dtm <- TermDocumentMatrix(corb_doc)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

dtm2 <- TermDocumentMatrix(may_doc)
m2 <- as.matrix(dtm2)
v2 <- sort(rowSums(m2),decreasing=TRUE)
d2 <- data.frame(word = names(v2),freq=v2)
head(d2, 10)

# Make wordcloud ---------------------------------------------------------------
set.seed(1234)
pdf("speechcloud.pdf")
par(mfrow=c(1,2))
wordcloud(rownames(corbtdm),corbtdm, min.freq = 1, scale=c(5, .2),
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
 
wordcloud(rownames(maytdm),maytdm, min.freq = 1, scale=c(5, .2),
                     max.words=200, random.order=FALSE, rot.per=0.35, 
                     colors=brewer.pal(8, "Dark2"))
dev.off()

par(mfrow=c(1,1))
pdf("comparison_cloud.pdf")
comparison.cloud(tdm, random.order=FALSE, 
                 colors = c("indianred3","lightsteelblue3"), 
                 title.size=2.5, max.words=400)
dev.off()


commonality.cloud(tdm, random.order=FALSE, scale=c(5, .5),colors = brewer.pal(4, "Dark2"), max.words=400)
