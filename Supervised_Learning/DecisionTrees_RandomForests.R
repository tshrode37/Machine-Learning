## Libraries ---------------------------------------------------------
library(DataExplorer)
library(caret)
library(rpart.plot)
library(rpart)
library(rattle)
library("randomForest")
library(ggplot2)


## Load Dataset ---------------------------------------------------------

wine_df <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep = ';')
head(wine_df)

## Data Exploration ---------------------------------------------------------

introduce(wine_df)
plot_intro(wine_df)
#plot_bar(wine_df) #discrete features
plot_histogram(wine_df) #continuous features
table(wine_df$quality)
plot_correlation(wine_df)
cor_data <- subset(wine_df[,1:11])
cor <- cor(cor_data)
findCorrelation(cor, cutoff=0.5) 
#plot_correlation(wine_df, type = "d")
#plot_correlation(wine_df, type = "c")
plot_boxplot(wine_df, by = "quality")
plot_scatterplot(wine_df, by = "quality")


## Convert Response Variable to Factor and Group Levels ---------------------------------------------------------

str(wine_df)

sum(wine_df$quality == '3', wine_df$quality == '4', wine_df$quality == '5')
sum(wine_df$quality == '6', wine_df$quality == '7', wine_df$quality == '8')

wine_df$quality <- as.factor(with(wine_df, ifelse(quality >=6, 1, 0))) #group quality variable; convert response variable to factor (this is a classification tree) 
str(wine_df$quality)
table(wine_df$quality)

## Normalize Columns ---------------------------------------------------------

norm <- function(x) {return ((x - min(x)) / (max(x) - min(x)))} 
wine_norm <- as.data.frame(lapply(wine_df[,c(1:11)],norm)) #want to normalize numerical columns

## Combine Normalize Columns with Response Column ---------------------------------------------------------

final_df <- cbind(wine_norm, wine_df$quality)
head(final_df)
names(final_df)[names(final_df) == "wine_df$quality"] <- "quality" #https://www.datanovia.com/en/lessons/rename-data-frame-columns-in-r/#:~:text=144%20more%20rows-,Renaming%20columns%20with%20R%20base%20functions,column%20names%20where%20name%20%3D%20Sepal.
colnames(final_df)

## Create Train/Test Set ---------------------------------------------------------
set.seed(789)
index <- createDataPartition(final_df$quality, p =0.7, list = FALSE)
wineTrain <- final_df[index,] #index for training set
train_labels <- final_df[12][index,]
dim(wineTrain)
wineTest <- final_df[-index,] #not index for test set
test_labels <- final_df[-index,12]
dim(wineTest)

## Train Model + Plots ---------------------------------------------------------

wine_dtree <- rpart(quality ~., wineTrain, method = "class", cp = 0)
printcp(wine_dtree) 
plotcp(wine_dtree)
summary(wine_dtree)
prp(wine_dtree, box.palette = "Purples", tweak = 1.0)
rpart.plot(wine_dtree, box.palette = "Reds", tweak = 1.5)
fancyRpartPlot(wine_dtree, cex = 0.2, main="Decision Tree", caption = "rattle::fancyRpartPlot (Wine Decision Tree)")


## Evaluate Model ---------------------------------------------------------
wine_test_pred <- predict(wine_dtree, newdata = wineTest, type = "class")
confusionMatrix(wine_test_pred, test_labels)

## Prune Trees ---------------------------------------------------------
min(wine_dtree$cptable[,'xerror']) #Find the minimum cross-validation error of the classification tree model
which.min(wine_dtree$cptable[,'xerror']) #Locate the record with the minimum cross-validation errors

min_cp = wine_dtree$cptable[which.min(wine_dtree$cptable[,'xerror']),'CP'] 

prune_wine_dtree <- prune(wine_dtree, cp = min_cp)
fancyRpartPlot(prune_wine_dtree, cex = 0.5, uniform=TRUE, main="Pruned Classification Tree", caption = "rattle::fancyRpartPlot (Pruned Wine Data)")

wine_prune_test_pred <- predict(prune_wine_dtree, newdata = wineTest, type = "class")
confusionMatrix(wine_prune_test_pred, test_labels)

## Random Forests ---------------------------------------------------------

wine_forest <- randomForest(quality ~., wineTrain, method = "class")
print(wine_forest)
#plot(wine_forest)


wine_forest_test_pred <- predict(wine_forest, newdata = wineTest, type = "class")
confusionMatrix(wine_forest_test_pred, test_labels)

## Variable Importance ---------------------------------------------------------

#decision tree
wine_dtree_imp = varImp(wine_dtree, scale=FALSE)

ggplot(wine_dtree_imp, aes(x=reorder(rownames(wine_dtree_imp),Overall), y=Overall)) +
  geom_point( color="orangered1", size=4, alpha=0.6)+
  geom_segment(aes(x=rownames(wine_dtree_imp), xend=rownames(wine_dtree_imp), y=0, yend=Overall), 
               color='black') +
  xlab('Variable')+
  ylab('Overall Importance')+ coord_flip()+ggtitle('Variable Importance for Decision Tree (Wine Data)')

#pruned decision tree
wine_prun_tree_imp = varImp(prune_wine_dtree, scale=FALSE)

ggplot(wine_prun_tree_imp, aes(x=reorder(rownames(wine_prun_tree_imp),Overall), y=Overall)) +
  +     geom_point( color="darkorchid1", size=4, alpha=0.6)+
  +     geom_segment(aes(x=rownames(wine_prun_tree_imp), xend=rownames(wine_prun_tree_imp), y=0, yend=Overall), 
                     +                  color='black') +
  +     xlab('Variable')+
  +     ylab('Overall Importance')+ coord_flip()+ggtitle('Variable Importance for Pruned Decision Tree (Wine Data)')

#random forest
importance(wine_forest)
varImpPlot(wine_forest, cex = 0.5)
