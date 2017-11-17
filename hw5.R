rm(list = ls())
library(RCurl)
library(XML)
library(bitops)
library(stringr)
##Romeo and Juliet,Julius Caesar,hamlet
url=paste(c("http://publicliterature.org/pdf/2ws1610.pdf","http://publicliterature.org/pdf/2ws2410.pdf","D:/研二/研二上/大数据与互联网金融/HW5/hamlet.pdf") ) 
abs=lapply(url, FUN = function(x) htmlParse(x, encoding = "Latin-1"))
clean_txt = function(x) {
  cleantxt = xpathApply(x, "//body//text()
                        [not(ancestor :: script)][ not(ancestor :: style)] 
                        [not(ancestor :: noscript)] " ,xmlValue)
  cleantxt = paste(cleantxt, collapse="\n")
  cleantxt = str_replace_all(cleantxt, "\n", " ")
  cleantxt = str_replace_all(cleantxt, "\r", "")
  cleantxt = str_replace_all(cleantxt, "\t", "")
  cleantxt = str_replace_all(cleantxt, "<br>", "")
  return(cleantxt)
}
cleantxt = lapply(abs,clean_txt)
vec_abs = unlist(cleantxt)
vec_abs

#Create a corpus & Term Document Matrix
library(tm)
library(SnowballC)
abs      = Corpus(VectorSource(vec_abs))
abs_dtm  = DocumentTermMatrix(abs, control = list(
  stemming = TRUE, stopwords = TRUE, minWordLength = 3,
  removeNumbers = TRUE, removePunctuation = TRUE))
dim(abs_dtm)
inspect(abs_dtm)
#Find the words that occur more than 3 times
findFreqTerms(abs_dtm, 3)
#Remove sparse terms
removeSparseTerms(abs_dtm, 0.5)
inspect(removeSparseTerms(abs_dtm, 0.5))

#wordcloud
library(ggplot2) 
library(wordcloud)
freq = colSums(as.matrix(abs_dtm))   
wf   = data.frame(word=names(freq), freq=freq)   
plot = ggplot(subset(wf, freq>100), aes(word, freq))    
plot = plot + geom_bar(stat="identity")   
plot = plot + theme(axis.text.x=element_text(angle=45, hjust=1))   
plot  
freq  = colSums(as.matrix(abs_dtm))   
dark2 = brewer.pal(8, "Dark2")  
wordcloud(names(freq), freq, max.words=200, rot.per=0.1, colors=dark2)
#png("comment_cloud.png", width = 800, height = 800)
#wordcloud(names(freq),freq,scale= c(5, 1.5), min.freq = 1, max.words = 100,colors=dark2,family="myFont3",shape="circle")
dev.off() 
#histogram 
hist(freq, col = "grey", breaks = 20,ylim = c(0, 5000), xlab = "words",main="Histogram of words")
hist(freq, col = "grey", breaks = 20,ylim = c(0, 20), xlab = "words",main="Histogram of words")

#setiment

