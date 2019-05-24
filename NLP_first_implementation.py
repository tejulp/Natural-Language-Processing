# Natural-Language-Processing

con <- file("obama.txt", "r", blocking = FALSE) #store text file in a variable
text <- readLines(con) #read above variable line-by-line and store it in another variable
close(con) #close connections
df <- data.frame(text) #create a data frame of the second variable
textdata <- df[df$text,] 
text
View(df)
View(textdata)
doc.vec <- VectorSource(textdata)
doc.corpus <- Corpus(doc.vec)
summary(doc.corpus)
docs <- tm_map(doc.corpus, removePunctuation) #removing punctuation
docs <- tm_map(docs, removeNumbers) #removing numbers
docs <- tm_map(docs, tolower) #convert capital letters to small letters
#length(stopwords("english"))
#stopwords("english")
docs <- tm_map(docs, removeWords, stopwords("english")) #remove all words which hold no analytical value
docs <- tm_map(docs, removeWords, c("department", "email")) #removing some words seaparately which hold no value
docs <- tm_map(docs, stemDocument) #getting stem of the word by removing endings like 'ing', 'es', 's'
docs <- tm_map(docs, stripWhitespace) #due to words that are deleted white space is formed which has to be removed
#after pre-processing docs has to be converted to proper plain text document which can be read
docs <- tm_map(docs, PlainTextDocument) #tells R to treat your preprocessed documents as text documents
#Term document matrix -- to view docs
dtm <- DocumentTermMatrix(docs)
dtm
#inspect(dtm) -- because this will fill up our terminal so we can only check a few elements
inspect(dtm[1:5, 1:20])
dim(dtm) #number of documents and terms
tdm <- TermDocumentMatrix(docs) #transpose of dtm which will show a relation between words and characters 
tdm
inspect(tdm)
dim(tdm) #number of terms and documents
#organizing terms by their frequency
freq <- colSums(as.matrix(dtm)) #total no. of times a particular word in matrix occurs is stored in 'freq'
freq #frequency of words occuring
length(freq)
ord <- order(freq) 
ord
m <- as.matrix(dtm) #dtm is converted into matrix form and stored in a variable form
dim(m)
write.csv(m, file="dtm.csv") #excel file is created in working directory
dtms <- removeSparseTerms(dtm, 0.8)
inspect(dtms)
freq[head(ord)]
freq[tail(ord)]
head(table(freq), 20) #frequencies of frequency i.e no. of times a particular frequency occurs
freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE) #sorting in decreasing order
freq
head(freq, 14)
findFreqTerms(dtm, lowfreq = 25) #to identify terms that appear frequently i.e here more than 25 times
wf <- data.frame(word=names(freq), freq=freq) #data frame is created for all words and their respective frequencies in adjancent column
head(wf)
p <- ggplot(subset(wf, freq>25), aes(word, freq)) #25 as only 'will' is present above 50, so we increase no. of words to plot
p <- p + geom_bar(stat = "identity")
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) #angle specifies angle at which word on x-axis is written and hjust specifies spacing between 2 bar plots
p
findAssocs(dtm, c("american", "send"), corlimit = 0.5) #find almost similar terms with the terms mentioned that have a correlation limit above 50%
set.seed(142) #random number generator
wordcloud(names(freq), freq, min.freq = 25) #all words having a min frequency of 25 are displayed with size of 'will' being biggest as it has highest frequency and so on followed by other words
set.seed(142)
wordcloud(names(freq), freq, max.words = 100) #all words that come under 100 most frequently used are displayed
set.seed(142)
wordcloud(names(freq), freq, min.freq = 25, scale = c(5,0.1), colors = brewer.pal(6, "Dark2")) #here scale shows size of word
d <- dist(t(dtms), method = "euclidian") #method 'euclidian' determines difference between frequency of words
fit <- hclust(d=d, method = "ward.D") #this creates a cluster of all the differences and plots it
fit
plot(fit, hang=-1)
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k = 5) #k=5 specifies number of groups that are to be formed which results in the tree being cut by cutree function
rect.hclust(fit, k = 5, border = "red") #Draws rectangles around the branches of a dendrogram highlighting the corresponding clusters
#in groups k specifies the number of groups that are to be made from the plot and rect.hclust creates boxes around cluster of words

