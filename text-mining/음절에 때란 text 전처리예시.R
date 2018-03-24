doc<-unlist(read.table("the_rainbow_passage_revised.txt",header = FALSE))

doc<-as.character(doc)
doc1<-unlist(strsplit(gsub("s$","",gsub("'","",gsub(" ", "" , gsub("\\,","",gsub("\\." , "",tolower(doc)))))),"-"))

barplot(table(doc1))
nchar( gsub( "[^X]", "", gsub( "[aeiouy]+", "X",doc1)))


# 패키지 사용
install.packages("qdap")
library(qdap)
syllable_sum(doc1)


# 다른 방법
gsub("([ae,ee,ue,ou,oo])","i",doc1)
grep(c("a","e","i","o","u"),strsplit(doc1[43],""))
strsplit(doc1[43],c("a","e","i","o","u"))

gsub("[b-e]",".", txt)
txt <- c("The", "licenses", "for", "most", "software", "are",
         "designed", "to", "take", "away", "your", "freedom",
         "to", "share", "and", "change", "it.",
         "", "By", "contrast,", "the", "GNU", "General", "Public", "License",
         "is", "intended", "to", "guarantee", "your", "freedom", "to",
         "share", "and", "change", "free", "software", "--",
         "to", "make", "sure", "the", "software", "is",
         "free", "for", "all", "its", "users")
( i <- grep("[gu]", txt) ) # indices
     

strsplit(doc1," ")
strsplit(doc,",")
strsplit(doc)

grep("[a-z]", letters)
gsub("([ab])", "\\1_\\1_", "abc and ABC")

x <- c(as = "asfef", qu = "qwerty", "yuiop[", "b", "stuff.blah.yech")
# split x on the letter e
strsplit(x, "e")
a <- "stackoverflow"
b <- gsub("[^aeiouAEIOU]","C",a)
