#illustration of tm package
install.packages("extrafont")
library(tm)
# load the extrafont package
library(extrafont)
par(family="AppleMyungjo") # 한글용 폰트로 설정


# Load the data 
mydata <- read.csv("data/tokenized_reviews_ta_1.csv", header = FALSE, stringsAsFactors=FALSE)
original_data <- read.csv("data/text4.csv", header = FALSE, stringsAsFactors = FALSE)
colnames(mydata) <- c("City", "Attraction", "Date", "Grade", "Title", "Review")
colnames(original_data) <- c("City", "Attraction", "Date", "Grade", "Title", "Review")


#Usage of tm package
corp <- Corpus(VectorSource(mydata$Review))

# 빈도수 20 이하 제거 
minFreq <- 20
TermsDocsMat <- TermDocumentMatrix(corp, control = list(removePunctuation = FALSE, bounds = list(global = c(minFreq,Inf))))
#create the Document x Terms Matrix
DocsTermsMat <- DocumentTermMatrix(corp, control = list(removePunctuation = FALSE, bounds = list(global = c(minFreq,Inf))))

tdm <- as.matrix(TermsDocsMat)
dtm <- as.matrix(DocsTermsMat)

inspect(corp)
# inspect part of the matrix
tdm[1:10,1:5]
# inspect frequent words
c <- unlist(corp)

(freq.terms <- findFreqTerms(TermsDocsMat, lowfreq= 100))

# 예시 매력이 들어가는 corpus나 문장 구조 확인
query <- "매력"
words <- rownames(findAssocs(TermsDocsMat, query, .005))[1:20]
find <- colnames(dtm) %in% words
corr <- cor(dtm[,find])


library(corrplot)
par(family="AppleMyungjo")
corrplot(corr, type = "upper")

###Show terms frequencies with histogram
# can see the Zipf's law !
term.freq <- rowSums(tdm)
term.freq <- subset(term.freq, term.freq>=minFreq)
word_freqs = sort(term.freq, decreasing=FALSE) 
vocab <- names(word_freqs)
# create a data frame with words and their frequencies
df = data.frame(terms=vocab, freq=word_freqs)

library(ggplot2)
df$terms <- factor( df$terms, levels=unique(as.character(df$terms)) )
ggplot(df, aes(terms,freq)) + geom_bar(stat= "identity")+ 
  scale_x_discrete(name="Terms",labels=df$terms) +
  xlab("Terms") + ylab("Freq") + coord_flip()
