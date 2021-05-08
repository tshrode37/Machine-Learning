## Libraries ----------------------------------------------------------

library(DataExplorer)
library("vcd") # Kappa()
library(caret) #sampling, confusionMatrix(), sensitivity, specificity, pospredvalue (precision)
  #also loads ggplot2 and lattice
library(kernlab) #svm
library(gmodels) #CrossTable


## Load Dataset ----------------------------------------------------------

column_df <- read.csv(file.choose(), header = TRUE)
head(column_df)
str(column_df)
summary(column_df)
introduce(column_df)
plot_intro(column_df, theme_config = list(plot.title = element_text(size = rel(1)), plot.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"), axis.line = element_line(size = 1, colour = "grey80"), axis.ticks.length.y = unit(.25, "cm"), legend.position = "none"))

## Prepare Data: SVM ---------------------------------------

str(column_df$class)
column_df$class <- as.factor(column_df$class) #convert categorical variable to factor


column_scale <- column_df[1:6] %>% mutate_if(is.numeric, scale) #scale numeric values
head(column_scale)

class <- column_df$class #get response variable

column_final <- cbind(column_scale, class) #combine response variable and scaled variables
head(column_final)
str(column_final)


set.seed(789) #SVM: don't need to normalize for SVM
index <- createDataPartition(column_final$class, p =0.7, list = FALSE)
columnTrain <- column_final[index,] #index for training set
train_labels <- column_final[7][index,]
dim(columnTrain)
columnTest <- column_final[-index,] #not index for test set
test_labels <- column_final[-index,7]
dim(columnTest)


svm_linear_model <- ksvm(class ~ ., data = columnTrain, kernel = "vanilladot") #SVM Linear Model
svm_linear_model 


## Part 1 ----------------------------------------------------------

  # Cohen's kappa coefficient is an alternative measurement for accuracy. 
  # Research on Kappa(). What is the common interpretation of a kappa value? 
  # What are weighted and un-weighted kappa statistics, and when to use them? 
  # Give some examples in R and explanation.

#Lantz Chapter 10
linear_pred <- predict(svm_linear_model, columnTest)
CrossTable(linear_pred, test_labels)

pr_a <- 0.538 + 0.312 #proportion of actual agreement (observed agreement)
  #same as accuracy
pr_e <- (0.548*0.677)+(0.452*0.323) #expected agreement between classifier and the true values
  #P(actual type is Abnormal)*P(predicted type is Abnormal) = Probability of both choosing Abnormal: 0.538*0.677
  #P(actual type is Normal)*P(predicted type is Normal) = Probability of both choosing Normal: 0.452 (row total)*0.323(column total)
  #pr_e = sum of probs above
k <- (pr_a-pr_e)/(1-pr_e) #calculate Kappa statistic
round(k, 4)

pred_table <- table(linear_pred, test_labels)
Kappa(pred_table)



## Part 2 ----------------------------------------------------------

  # Precision and recall are mainly used in an information retrieval context 
  # (e.g., returned results from a search engine). Show R functions that compute these two 
  # measures (and specify the libraries used). Explain the meaning of precision and recall. 
  # Do you prefer low or high values? Discuss.

#TN FP
#FN TP

#Precision (Positive Predictive Value): TP/TP+FP
ppv <- 29/(29+1)
ppv
posPredValue(linear_pred, test_labels, positive = "Abnormal")
#Recall (Also Sensitivity): TP/TP+FN
  #Sensitivity (True Positive Rate) = TP/TP+FN

tpr <- 29/(29+13)
tpr
sensitivity(linear_pred, test_labels, positive = "Abnormal")

## Part 3 ----------------------------------------------------------

  # Include 2 more performance measures of your choice. For each performance measure,
  # state the objective, show an example, and interpret the result.


#Specificity (True Negative Rate) = TN/TN+FP
tnr <- 50/(50+1)
tnr
specificity(linear_pred, test_labels, positive = "Abnormal")


#F-Measure = 2*P*R/R+P = 2 * TP/2* TP + FP + FN
f_measure = 2*(ppv*tpr)/(ppv+tpr)
f_measure


conf_mat <- confusionMatrix(linear_pred, test_labels, mode = 'everything', positive = "Abnormal")
