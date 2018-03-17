#illustration of tm package
install.packages("extrafont")
library(tm)
# load the extrafont package
library(extrafont)
par(family="AppleMyungjo") # 한글용 폰트로 설정


# ------------
mydata <- read.csv("data/tokenized_reviews_ta_1.csv", header = FALSE, stringsAsFactors=FALSE)
original_data <- read.csv("data/text4.csv", header = FALSE, stringsAsFactors = FALSE)
colnames(mydata) <- c("City", "Attraction", "Date", "Grade", "Title", "Review")
colnames(original_data) <- c("City", "Attraction", "Date", "Grade", "Title", "Review")

stop_words <- read.csv("stopwords1.csv", sep=",", header = FALSE)

#stopwords characterㅎ
stop_words <- as.character(stop_words$V1)
# myStopwords <- c(stopwords('english'), stop_words)
#Usage of tm package
#create a corpus
corp <- Corpus(VectorSource(mydata$Review))
#apply several operations: lowercase, remove stopwords etc.
# corp <- tm_map(corp, removeWords, stopwords("english"))
# #corp <- tm_map(corp, tolower)
# #create a customized stopwords list & apply it to corpus
# corp <- tm_map(corp, removeWords, myStopwords)
#create the Terms x Document Matrix with some options
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
ggplot(df, aes(terms,freq)) + geom_bar(stat= "identity") + scale_x_discrete(name="Terms", labels=df$terms) + xlab("Terms") + ylab("Freq") + coord_flip()
