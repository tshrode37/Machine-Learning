## Load Libraries------------------------------------------------------
library(DataExplorer) #https://cran.r-project.org/web/packages/DataExplorer/vignettes/dataexplorer-intro.html#alternative
library("caret")
library(class) #https://www.rdocumentation.org/packages/DMwR/versions/0.4.1/topics/kNN
library("e1071")

## Load Data------------------------------------------------------
heart <- read.csv(file.choose(), header = T)
View(heart)

## Rename Column Names------------------------------------------------------
  #https://stackoverflow.com/questions/6081439/changing-column-names-of-a-data-frame
colnames(heart)
colnames(heart) <- c( "age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num")
colnames(heart)

## Exploratory Analysis------------------------------------------------------
summary(heart) #notice, characters in "ca" and "thal"
str(heart)
introduce(heart)
plot_intro(heart)
plot_missing(heart)
plot_bar(heart) #visualize frequency distributions for all discrete features
plot_bar(heart, with = "trestbps") # look at bivariate frequency distribution
plot_histogram(heart) #visualize distributions for all continuous features
plot_correlation(na.omit(heart)) #visualize correlation heatmap for all non-missing features
plot_correlation(na.omit(heart), type = "c") #continuous variables
plot_correlation(na.omit(heart), type = "d") #discrete variables
plot_boxplot(heart, by = 'sex') # visualize the distribution of all continuous features based on sex with a boxplot
plot_scatterplot(heart, by = 'age')


##Handle Missing Data------------------------------------------------------
colSums(is.na(heart)) #compute total NAs in each column
#recall, "ca" and "thal" columns contain "?" (both are discrete features)
unique(heart$ca)
unique(heart$thal)

#replace ? with NA
heart[heart == "?"] <- NA #https://stackoverflow.com/questions/11036989/replace-all-0-values-to-na
  # OR
  #na_strings <- c("NA", "N A", "N / A", "N/A", "N/ A", "Not Available", "NOt available", "?")
  #heart[heart == na_strings] <- NA
sum(is.na(heart))
plot_missing(heart)
complete_heart <- heart[complete.cases(heart), ]
sum(is.na(complete_heart))

## Normalize Data------------------------------------------------------
norm <- function(x) {return ((x - min(x)) / (max(x) - min(x)))} #https://www.statology.org/how-to-normalize-data-in-r/
sapply(complete_heart, class) #https://stackoverflow.com/questions/21125222/determine-the-data-types-of-a-data-frames-columns
  #want to normalize numerical columns: age, chol, oldpeak, thalach, trestbp
  #exclude sex, fbs, exang; already 0, 1 values
  #exclude cp, num, restecg, slope, ca, thal; factors
norm_heart <- as.data.frame(lapply(complete_heart[,c(1,4,5,8,10)],norm))
View(norm_heart)

## Convert Factors to Dummy Variables------------------------------------------------------
  #https://amunategui.github.io/dummyVar-Walkthrough/
  #exclude sex, fbs, exang; already 0, 1 values
  #cp, restecg, slope, ca, thal; factors
cat_heart <- as.data.frame(lapply(complete_heart[,c(3,7,11:13)], as.factor))
dmy <- dummyVars("~.", data = cat_heart, fullRank = TRUE)
dmy_df <- as.data.frame(predict(dmy, newdata = cat_heart))
print(dmy_df)

  #Change predicted/response variable (num) to 2 classes: 0 = No heart disease, Value >= 1 = Heart disease
  #Then, convert to factor variable with as.factor
unique(complete_heart$num)
complete_heart$num <- as.factor(with(complete_heart, ifelse(num >=1, 1,0))) 
  #if num >= 1, then convert to 1, else, leave num = 0
unique(complete_heart$num)

## Combine all variables back together------------------------------------------------------
  #https://statisticsglobe.com/cbind-r-command-example/#:~:text=Basic%20R%20Syntax%3A&text=The%20name%20of%20the%20cbind,or%20data%20frames%20by%20columns.
  #norm_heart, dmy_df, cat_heart, complete_heart$sex, complete_heart$fbs, complete_heart$exang, complete_heart$num
final_df <- cbind(complete_heart$sex, complete_heart$fbs, complete_heart$exang, norm_heart, dmy_df, cat_heart, complete_heart$num)
View(final_df)
dim(final_df) #https://stat.ethz.ch/R-manual/R-devel/library/base/html/dim.html

## Divide Data into Train/Test Set Using Caret Package------------------------------------------------------
  #Want 70/30 Split
  #https://topepo.github.io/caret/data-splitting.html
set.seed(346) #https://www.edureka.co/community/51489/what-is-set-seed-in-r#:~:text=set%20seed%20(value)%20where%20value,of%20the%20random%20number%20seed.&text=In%20the%20above%20line%2C123,random%20numbers%20results%20by%20seed.
index <- createDataPartition(final_df$`complete_heart$num`, p =0.7, list = FALSE)
dim(index)
sqrt(208)
#optimal k usually is sqrt(n) #https://www.analyticsvidhya.com/blog/2015/08/learning-concept-knn-algorithms-programming/
heartTrain <- final_df[index,] #index for training set
dim(heartTrain)
heartTest <- final_df[-index,] #not index for test set
dim(heartTest)

## KNN Algorithm------------------------------------------------------
labels <- final_df[26][index,] # https://stackoverflow.com/questions/54837161/error-train-and-class-have-different-lengths
knn_1 <- knn(train = heartTrain, test = heartTest, cl =labels, k =1)
test_labels <- final_df[-index,26] #https://towardsdatascience.com/k-nearest-neighbors-algorithm-with-examples-in-r-simply-explained-knn-1f2c88da405c
table_1 <- table(knn_1, test_labels) #confusion matrix
table_1
accuracy_1 <- 100*sum(test_labels == knn_1)/NROW(test_labels)
accuracy_1


confusionMatrix(table_1) #confusion matrix with caret and e1071 package

  #https://rstudio-pubs-static.s3.amazonaws.com/316172_a857ca788d1441f8be1bcd1e31f0e875.html
#loop to determine most accurate K
i = 1
k_optimal = 1
for(i in 1:19){
  knn_model <- knn(heartTrain, heartTest, cl=labels, k=i) #https://rstudio-pubs-static.s3.amazonaws.com/316172_a857ca788d1441f8be1bcd1e31f0e875.html
  k_optimal[i] <- confusionMatrix(knn_model, test_labels)$overall['Accuracy'] #https://stackoverflow.com/questions/24348973/how-to-retrieve-overall-accuracy-value-from-confusionmatrix-in-r
  k=i
  cat(k,'=',k_optimal[i], '\n') 
}
plot(k_optimal, type="b", xlab="K- Value",ylab="Accuracy level")

##Optimal k = 3----------------------------------------------------
knn_3 <- knn(train = heartTrain, test = heartTest, cl =labels, k =3)
table(knn_3, test_labels) #confusion matrix
100*sum(test_labels == knn_3)/NROW(test_labels)

confusionMatrix(knn_3, test_labels)

#confusionMatrix(knn_1, test_labels)
#library(gmodels)
#CrossTable(test_labels, knn_3, prop.chisq = FALSE) #https://www.analyticsvidhya.com/blog/2015/08/learning-concept-knn-algorithms-programming/
