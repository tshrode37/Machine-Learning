## Libraries -----------------------------------------------------

library(sqldf)
library(DataExplorer)
library(dplyr)
library(ggplot2)
library(arules) #transactions
library(RColorBrewer)
library(arulesViz)

## Load Data Set -------------------------------------------------

load("C:/Users/07hoc/OneDrive/Documents/Data Science Documents/bxBooks.RData") #load dataset
  #specify the path of the file that you saved earlier if it is not in the current working directory

## View Data --------------------------------------------------------

head(bxBookRatings) #view bxBookRatings: contains UserID (int), ISBN (chr), and BookRating (int)
str(bxBookRatings)
plot_intro(bxBookRatings)


head(bxBooks) #view bxBooks: contains ISBN (chr), BookTitle (chr), BookAuthor (chr), YearOfPublication (int), Publisher (chr), ImagesURLsizeS (chr), ImagesURLsizeM (chr), ImagesURLsizeL  (chr)
str(bxBooks) #can remove ImagesURLS, M, L
plot_intro(bxBooks)

head(bxUsers) #view bxUsers: contains UserID (int), Location (chr), Age (chr)
str(bxUsers)
plot_intro(bxUsers)


## Data Cleaning --------------------------------------------------------

#remove underscore from colnames
colnames(bxBookRatings) <- gsub(".", "_", colnames(bxBookRatings), fixed=T) #substitute characters in  bxBookRatings colnames
colnames(bxBooks) <- gsub(".", "_", colnames(bxBooks), fixed=T) #substitute characters in bxBooks colnames
colnames(bxUsers) <- gsub(".", "_", colnames(bxUsers), fixed=T) #substitute characters in bxUsers colnames


#remove 3 URL columns in bxBooks, but keep original dataset
bxBooks_subset <- bxBooks[,-c(6:8)]
head(bxBooks_subset, 20)
tail(bxBooks_subset$Book_Title, 20) #5: "Petite histoire de la dÃ?Â©sinformation"


#get rid of special characters in Book_Title
Sys.setlocale('LC_ALL','C') # to deal with the non-US characters
tail(bxBooks_subset$Book_Title, 20) #5: "Petite histoire de la dC?B)sinformation"

#https://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r

head(bxBooks_subset$Book_Title, 20)
tail(bxBooks_subset$Book_Title, 20)

sum(grepl('[[:punct:]]', bxBooks_subset$Book_Title))
booktokens <- gsub("[[:punct:]]", "", bxBooks_subset$Book_Title) #remove punctuation !"#$%&'()*+,-./:;<=>?@[]^_`{|}~ 


tail(booktokens, 20)


sum(grepl("[[:space:]]+$", booktokens))
cleantitle <- sub("[[:space:]]+$","",booktokens)
tail(cleantitle, 20)


#create new data frame with ISBN, lowercase titles, and book titles
Books <- data.frame(ISBN=bxBooks_subset$ISBN, token=booktokens, title=bxBooks_subset$Book_Title) 
head(Books)

length(Books$token)
length(unique((Books$token))) #check number of unique book titles


## Manipulate Data Frame with **sqldf** package --------------------------------------------------------

#final df needs to consist of unique ISBN, rating, userID, title, author, publication, publisher

#select the a unique ISBN number for every token (number of unique tokens)
bookmap <- sqldf('SELECT min(ISBN) as unique_isbn, 
                        token 
                 FROM Books
                 GROUP BY token')
  #select minimum ISBN (from group by option) and save as unique_isbn, and select token from Books df and group by token

dim(bookmap)
head(bookmap) #contains unique ISBN and clean+lowercase book titles

# displaymap has a title for every unique token
#displaymap <- sqldf('SELECT Books.title as title,
                           #bookmap.token as token
                  # FROM Books,
                   #     bookmap
                   #WHERE Books.ISBN=bookmap.unique_isbn')
  #select title from Books df and token from bookmap where ISBN in Books = unique_isbn 

dim(displaymap)
head(displaymap)

# get publication information and authors
publication <- sqldf("SELECT Books.token as token, bxBooks_subset.Book_Author, bxBooks_subset.Year_Of_Publication, bxBooks_subset.Publisher
                      FROM Books, bxBooks_subset
                     WHERE bxBooks_Subset.ISBN=Books.ISBN")

dim(publication)
head(publication)

# now, need ratings and userID
ratings <- sqldf('SELECT bxBookRatings.User_ID as user_id, Books.token as token, bxBookRatings.Book_Rating as rating
                    FROM Books, bxBookRatings
                    WHERE bxBookRatings.ISBN = Books.ISBN')

  #can load bxBookRatings as ratings
  # select User id from bxBookRatings and lowercase+clean titles from Books
    #where bxBookRatings ISBN are in Books df
  #Only want rated books that are bxBooks data

dim(ratings)
head(ratings)

#merge data

#bookdata <- merge(ratings, displaymap, by="token")
bookdata <- merge(ratings, bookmap, by="token")
bookdata <- merge(bookdata, publication, by = "token")
#bookdata <- merge(bookdata, bookmap, by = "token")

dim(bookdata)
head(bookdata) 

#remove duplicate data: https://www.datanovia.com/en/lessons/identify-and-remove-duplicate-data-in-r/

sum(duplicated(bookdata))

dim(bookdata)
dim(bookdata %>% distinct())


final_book <- bookdata %>% distinct()

## Data Exploration --------------------------------------------------------

plot_intro(final_book)
colnames(final_book)

#distribution of ratings
ggplot(data = final_book) + aes(x = rating, fill = factor(rating)) +
  geom_bar() +
  labs(title = "Distribution of Book Ratings", y = "Count", x = "Rating", fill = "rating") +
  theme_light() 

#top book titles
sorted_titles <- sort(table(final_book$token), decreasing = TRUE)
top_titles <- as.data.frame(sorted_titles[1:30]) #https://stackoverflow.com/questions/26788049/plot-table-objects-with-ggplot


ggplot(data = top_titles) + aes(x = reorder(Var1, Freq), y= Freq, fill = Var1) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Top 30 Book Titles", y = "Count", x = "Book Title") +
  theme_light() +
  coord_flip(xlim = NULL, ylim = NULL, expand = FALSE, clip = "on")


#top year_of_publication
sorted_year <- sort(table(final_book$Year_Of_Publication), decreasing = TRUE)
top_year <- as.data.frame(sorted_year[1:30])


ggplot(data = top_year) + aes(x = reorder(Var1, Freq), y= Freq, fill = Var1) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Top 30 Publication Years", y = "Count", x = "Year") +
  theme_light() +
  coord_flip(xlim = NULL, ylim = NULL, expand = FALSE, clip = "on")



#top publishers
sorted_publisher <- sort(table(final_book$Publisher), decreasing = TRUE)
top_publisher <- as.data.frame(sorted_publisher[1:30])


ggplot(data = top_publisher) + aes(x = reorder(Var1, Freq), y= Freq, fill = Var1) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Top 30 Publishers", y = "Count", x = "Publisher") +
  theme_light() +
  coord_flip(xlim = NULL, ylim = NULL, expand = FALSE, clip = "on")


#top authors
sorted_author <- sort(table(final_book$Book_Author), decreasing = TRUE)
top_author <- as.data.frame(sorted_author[1:30])


ggplot(data = top_author) + aes(x = reorder(Var1, Freq), y= Freq, fill = Var1) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Top 30 Authors", y = "Count", x = "Author") +
  theme_light() +
  coord_flip(xlim = NULL, ylim = NULL, expand = FALSE, clip = "on")


#display.brewer.all()

## Convert attributes to transactions --------------------------------------------------------

#convert attributes to factors
final_book <- final_book %>% mutate(unique_isbn=as.factor(unique_isbn))
final_book <- final_book %>% mutate(user_id=as.factor(user_id))
final_book <- final_book %>% mutate(rating=as.factor(rating))
final_book <- final_book %>% mutate(token=as.factor(token))
final_book <- final_book %>% mutate(Book_Author=as.factor(Book_Author))
final_book <- final_book %>% mutate(Year_Of_Publication=as.factor(Year_Of_Publication))
final_book <- final_book %>% mutate(Publisher=as.factor(Publisher))

str(final_book)

#final_book$unique_isbn <- as.factor(final_book$unique_isbn)
#final_book$user_id <- as.factor(final_book$user_id)
#final_book$rating <- as.factor(final_book$rating)
#final_book$token <- as.factor(final_book$token)
#final_book$Book_Author <- as.factor(final_book$Book_Author)
#final_book$Year_Of_Publication <- as.factor(final_book$Year_Of_Publication)
#final_book$Publisher <- as.factor(final_book$Publisher)


#save in tab-separated format

write.table(final_book, file="books_merged.tsv", sep="\t", row.names = FALSE,
            col.names = c("Book_Title", "User_ID", "Rating", "ISBN", 
                          "Book_Author", "Year_Of_Publication", "Publisher"))

#convert written file to transaction class

transaction_book <- read.transactions('books_merged.tsv', format = "single",
                             sep="\t",
                             cols=c("User_ID", "Book_Title"), header = TRUE,
                             rm.duplicates = TRUE)
## Explore the Transaction Data --------------------------------------------------------

transaction_book
class(transaction_book)
transaction_book[2]
dim(transaction_book)
colnames(transaction_book)[1:5]
summary(transaction_book)

#examine the distribution of transaction sizes (basket sizes)
booksizes <- size(transaction_book)
summary(booksizes) #Most customers (at least half of them, in fact) only expressed interest in one book. But someone has expressed interest in more than 10,000

quantile(booksizes, probs = seq(0,1,0.1)) #Looks at the basket size distribution, in 10% increments

ggplot(data.frame(count = booksizes)) +
  geom_density(aes(x = count)) +
  scale_x_log10() #plot distribution
    #90% of customers expressed interest in fewer than 15 books; most of the remaining customers expressed interest in up to about 100 books or so



itemFrequencyPlot(transaction_book, topN = 20, type = "absolute", col = brewer.pal(8,"Pastel2"), main = "Absolute Item Frequency Plot")
#"absolute": plot numeric frequencies of each item independently.

itemFrequencyPlot(transaction_book, topN = 20, type = "relative", col = brewer.pal(8,"Pastel2"), main = "Relative Item Frequency Plot")
#"relative":  plot how many times these items have appeared as compared to others.

## Generate Rules--------------------------------------------------------

transaction_book_2 <- transaction_book[booksizes > 1] #want customers who had interest in more than 1 book

association_rules1 <- apriori(transaction_book_2, parameter = list(support = 0.005, conf = 0.70))
summary(association_rules1)
inspect(sort(association_rules1, by = "confidence", decreasing = TRUE)) #print and sort rules by confidence




#note: High confidence and low support assist in extracting strong relationships.
association_rules2 <- apriori(transaction_book, parameter = list(support = 0.001, conf = 0.70))
summary(association_rules2) 
inspect(sort(association_rules2[1:5], by = "confidence", decreasing = TRUE)) #print and sort rules by confidence
  #lhs: IF, rhs: THEN

measures <- interestMeasure(association_rules2,
                            measure = c("coverage", "fishersExactTest"),
                            transactions = transaction_book_2)
summary(measures)

## Visualizing Association Rules --------------------------------------------------------

subrules <- association_rules2[quality(association_rules2)$confidence >0.50]
plot(subrules, method = "scatterplot", measure = c("support", "confidence"), shading = "lift")
plot(subrules, method = "two-key plot", measure = c("support", "confidence"), shading = "order")


#plot top 10 rules

topsubrules <- head(subrules, 10, by = "confidence")
plot(topsubrules, method = "graph",  engine = "htmlwidget")
##--------------------------------------------------------

