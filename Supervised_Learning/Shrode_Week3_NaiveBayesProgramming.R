# Libraries ---------------------------------------------
library("tm") #text mining package: tm_map()
library("SnowballC") #used for stemming, wordStem(), stemDocument()
library(wordcloud) #wordcloud generator
library(e1071) #Naive Bayes
library(gmodels) #CrossTable()
library(caret) #ConfusionMatrix()
library(DataExplorer)
library("RColorBrewer") #colors for wordcloud


# Load Data ---------------------------------------------
temp <- tempfile() #create temporary file
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00228/smsspamcollection.zip",temp) #get online file
sms_file <- unz(temp, "SMSSpamCollection") #extract file from temporary file
spam_df <- read.csv2(sms_file, header= FALSE, sep= "\t", quote= "", col.names= c("type","text"), stringsAsFactors= FALSE) #read file
unlink(temp) #remove temp file using unlink


# Exploratory Analysis ---------------------------------------------
str(spam_df)
spam_df$type <- factor(spam_df$type) #type should be changed to factor because it is a categorical variable
str(spam_df$type)
table(spam_df$type)
plot_intro(spam_df)
plot_missing(spam_df)
plot_bar(spam_df)

# Cleaning and Standardizing Text Data ---------------------------------------------
  # tm package
sms_corpus <- VCorpus(VectorSource(spam_df$text)) 
print(sms_corpus) #notice, from dataset description: 3375+425+450+1002+322 = 5574
inspect(sms_corpus[1:2]) #can use list operators
as.character(sms_corpus[[1]]) #view actual message text of one message
lapply(sms_corpus[1:2], as.character) #view actual message text of multiple messages

sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower)) #standardize message to use only lowercase characters
as.character(sms_corpus_clean[[1]]) #confirm transformation

sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers) #remove numbers
  #use getTransformations() to get built-in transformations
as.character(sms_corpus_clean[[1]]) #confirm transformation

sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords()) #remove stopwords
as.character(sms_corpus_clean[[1]]) #confirm transformation

sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation) #remove Punctuation
as.character(sms_corpus_clean[[1]]) #confirm transformation

  #SnowballC package
wordStem(c("learn", "learned", "learning", "learns")) #returns root form of terms
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument) #apply wordStem to entire corpus to perform stemming (stemDocument() needs SnowballC)
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace) #final step: remove additional whitespace
lapply(sms_corpus[1:5], as.character) #confirm transformations
lapply(sms_corpus_clean[1:5], as.character) #compare to original corpus text


## clean corpus function

clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(tolower)) #lowercase
  corpus <- tm_map(corpus, removeNumbers) #remove numbers
  corpus <- tm_map(corpus, removeWords, stopwords()) #remove stopwords
  corpus <- tm_map(corpus, removePunctuation) #remove Punctuation
  corpus <- tm_map(corpus, stemDocument) #apply wordStem to entire corpus to perform stemming (stemDocument() needs SnowballC)
  corpus <- tm_map(corpus, stripWhitespace)
}

# Splitting Text Documents (SMS Messages) into Tokens (Words) ---------------------------------------------
  #Split through tokenization with tm package
sms_dtm <- DocumentTermMatrix(sms_corpus_clean) #create a DTM sparse matrix; will contain tokenized corpus
  #could also preprocess during this step using:
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  removePunctuation = TRUE,
  stopwords = TRUE,
  stemming = TRUE
))

#compare dtm to preprocessing with dtm
sms_dtm
sms_dtm2


# Split Data into Training and Test Datasets ---------------------------------------------
cat("Original DataSet Size:", NROW(sms_dtm))
  #can split manually because sms messages are in a random order; 70:30
cat("Training Set Size:", round(NROW(sms_dtm)*.70))
cat("Testing Set Size:", round(NROW(sms_dtm)*.30))

sms_dtm_train <- sms_dtm[1:3902,]
sms_train_labels <- spam_df[1:3902,]$type
sms_dtm_test <- sms_dtm[3902:5574,]
sms_test_labels <- spam_df[3902:5574,]$type

  #confirm proportions
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

# Word Clouds ---------------------------------------------

wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE, colors = brewer.pal(8,"Accent")) #wordcloud library; wordcloud for full clean corpus

spam_wc <- subset(spam_df, type == "spam") #get documents for type = spam
ham_wc <- subset(spam_df, type == "ham") #get documents for type = ham

spam_corpus <- VCorpus(VectorSource(spam_wc$text))
spam_corpus_clean <- clean_corpus(spam_corpus)

ham_corpus <- VCorpus(VectorSource(ham_wc$text))
ham_corpus_clean <- clean_corpus(ham_corpus)


wordcloud(spam_corpus_clean, random.order = FALSE, colors = brewer.pal(8,"Spectral"), max.words = 100) #wordcloud for spam #wordcloud for spam
wordcloud(ham_corpus_clean, random.order = FALSE, colors = brewer.pal(8,"PRGn"), max.words = 100) #wordcloud for ham

# Get Frequent Words  ---------------------------------------------

## plot frequency table of words/Build Term Document Matrix (#5)


tdm <- TermDocumentMatrix(sms_corpus_clean)
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d, 10)
barplot(d[1:20,]$freq, las = 2, names.arg = d[1:20,]$word,
        col ="purple", main ="Most frequent words",
        ylab = "Word frequencies")



##spam



spam_tdm <- TermDocumentMatrix(spam_corpus_clean)
spam_m <- as.matrix(spam_tdm)
spam_v <- sort(rowSums(spam_m), decreasing = TRUE)
spam_d <- data.frame(word = names(spam_v), freq = spam_v)
head(spam_d, 10)
barplot(spam_d[1:20,]$freq, las = 2, names.arg = spam_d[1:20,]$word,
        col ="gold", main ="Most Frequent Spam Words",
        ylab = "Word Frequencies")

findAssocs(spam_tdm, terms = "free", corlimit = 0.25)

##ham
 


ham_tdm <- TermDocumentMatrix(ham_corpus_clean)
ham_m <- as.matrix(ham_tdm)
ham_v <- sort(rowSums(ham_m), decreasing = TRUE)
ham_d <- data.frame(word = names(ham_v), freq = ham_v)
head(ham_d, 10)
barplot(ham_d[1:20,]$freq, las = 2, names.arg = ham_d[1:20,]$word,
        col ="pink", main ="Most Frequent Ham Words",
        ylab = "Word Frequencies")


## reduce number of features for model


sms_freq_words <- findFreqTerms(sms_dtm_train, 5) #tm package; display words appearing at least 5 times in train matrix
str(sms_freq_words) #look at contents of freq words above
sms_dtm_freq_train <- sms_dtm_train[,sms_freq_words] #filter train and test DTM's to include terms in vector above
sms_dtm_freq_test <- sms_dtm_test[,sms_freq_words]

str(sms_dtm_freq_train)
str(sms_dtm_freq_test)

head(sms_dtm_freq_train)

# Training A Model ---------------------------------------------

#need categorical features; the cells in the sparse matrix are numeric and measure the number of times a word appears in a message. 
#We need to change this to a categorical variable that simply indicates yes or no depending on whether the word appears at all.

convert_counts <- function(x){
  x <- ifelse(x >0, "Yes", "No")
}


#apply function to columns in matrix; MARGIN = 2 is for columns
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)


sms_model <- naiveBayes(sms_train, sms_train_labels) #build model; e1071 package


# Evaluate Model ---------------------------------------------

sms_test_prediction <- predict(sms_model, sms_test) #test predictions on unseen data (test data)
CrossTable(sms_test_prediction, sms_test_labels, prop.chisq = FALSE, prop.t = FALSE,
           dnn = c("predicted", "actual")) #gmodels package; compare predictions to true values

#2.1% error, now use Laplace estimator
sms_model_2 <- naiveBayes(sms_train, sms_train_labels,laplace = 1)
sms_test_prediction_2 <- predict(sms_model_2,sms_test)
CrossTable(sms_test_prediction_2, sms_test_labels, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c("predicted", "actual"))
