## Libraries ----------------------------------------------------------------

library(DataExplorer)
library(e1071)
library("kernlab")
library(neuralnet)
library(caret)

## Load Dataset ----------------------------------------------------------------

mushroom_df <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data", sep = ',', header = FALSE, col.names = c("classes", "cap-shape", "cap-surface", "cap-color", "bruises", "odor", "gill-attachment", "gill-spacing", "gill-size", "gill-color", "stalk-shape", "stalk-root", "stalk-surface-above-ring", "stalk-surface-below-ring", "stalk-color-above-ring", "stalk-color-below-ring", "veil-type", "veil-color", "ring-number", "ring-type", "spore-print-color", "population", "habitat"))
head(mushroom_df)
for (x in colnames(mushroom_df)){
  print(unique(mushroom_df[x]))
}

## Convert "classes" to Factor ----------------------------------------------------------------------------------

mushroom_df$classes <- factor(mushroom_df$classes, levels = c("e","p"), labels=c("edible","poisonous")) 
unique(mushroom_df$classes)

## Remove Unnecessary Columns, Handle Missing Values, and Data Exploration ----------------------------------------------------------------

  #mushroom_df$veil.type has only one level
mushroom_df$veil.type <- NULL #remove veil.type column

  #missing values = "?"

mushroom_df[mushroom_df == "?"] <- NA 
str(mushroom_df)
introduce(mushroom_df)
plot_intro(mushroom_df)
sum(is.na(mushroom_df))
plot_missing(mushroom_df)

  #replace missing values with new level: "m" = missing
  #since missing value is in stalk.root column and no other values = "m", we can use "m" to represent missing values
mushroom_df$stalk.root[is.na(mushroom_df$stalk.root)] <- "m" #https://stackoverflow.com/questions/8161836/how-do-i-replace-na-values-with-zeros-in-an-r-dataframe
sum(is.na(mushroom_df))
plot_missing(mushroom_df)

plot_bar(mushroom_df)
## SVM ------------------------------------------------------------------------------------------------------
## Create Training/Testing Dataset and Labels----------------------------------------------------------------

set.seed(789)
index <- createDataPartition(mushroom_df$classes, p =0.7, list = FALSE)
mushroomTrain <- mushroom_df[index,] #index for training set
train_labels <- mushroom_df[1][index,]
dim(mushroomTrain)
mushroomTest <- mushroom_df[-index,] #not index for test set
test_labels <- mushroom_df[-index,1]
dim(mushroomTest)

  
  # Linear SVM - e1071
svm_linear_model = svm(classes~., data = mushroomTrain, kernel = "linear", scale = FALSE) #http://uc-r.github.io/svm
summary(svm_linear_model)
#plot(svm_linear_model, mushroomTrain, Petal.Width ~ Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))
linear_pred <- predict(svm_linear_model, mushroomTest)
confusionMatrix(linear_pred, test_labels)
plot(svm_linear_model, mushroomTrain)

  # Linear SVM - Kernlab
svm_linear_model_2  <- ksvm(classes ~ ., data = mushroomTrain, kernel = "vanilladot")
svm_linear_model_2
linear_pred_2 <- predict(svm_linear_model_2, mushroomTest)
table(linear_pred_2, test_labels)
confusionMatrix(linear_pred_2, test_labels)

  # Gaussian RBF (Radial) SVM - Kernlab
svm_rbf_model <- ksvm(classes ~ ., data = mushroomTrain, kernel = "rbfdot")
svm_rbf_model
rbf_pred <- predict(svm_rbf_model, mushroomTest)
table(rbf_pred, test_labels)
confusionMatrix(rbf_pred, test_labels)


  # Polynomial SVM - Kernlab

svm_poly_model <- ksvm(classes ~ ., data = mushroomTrain, kernel = "polydot")
svm_poly_model
poly_pred <- predict(svm_poly_model, mushroomTest)
table(poly_pred, test_labels)
confusionMatrix(poly_pred, test_labels)

  # Sigmoid SVM - Kernlab

svm_sigmoid_model <- ksvm(classes ~ ., data = mushroomTrain, kernel = "tanhdot")
svm_sigmoid_model
sigmoid_pred <- predict(svm_sigmoid_model, mushroomTest)
table(sigmoid_pred, test_labels)
confusionMatrix(sigmoid_pred, test_labels)

## ---------------------------------------------------------------------
## ANN ----------------------------------------------------------------

  #convert categorical variables (except the predicted class/label)
mushroom_ann_df <- subset(mushroom_df[2:22])
class <- subset(mushroom_df[1])
dmy <- dummyVars(~.,mushroom_ann_df, fullRank=T)
trsf <- data.frame(predict(dmy, newdata = mushroom_ann_df))

  #combine class and dummy variables
final_mushroom_ann <- cbind(trsf, class)
dim(final_mushroom_ann)

  #create train and test data sets
ind = sample(2, nrow(final_mushroom_ann),replace=TRUE,prob=c(0.7,0.3))
trainset = final_mushroom_ann[ind == 1,]
dim(trainset)
testset = final_mushroom_ann[ind==2,]
dim(testset)
    
  #add classes label to trainset
#unique(final_mushroom_ann$classes)
#trainset$poisonous = trainset$classes == "poisonous"
#trainset$edible = trainset$classes == "edible"
#dim(trainset)

  # Train model
train_columns <- colnames(trainset[-96])
nn_form <- as.formula(paste("classes~", paste(train_columns, collapse = "+")))
nn_model <- neuralnet(nn_form, data = trainset, hidden = 3)
nn_model$result.matrix
plot(nn_model, rep="best"))

  #visualize the generalized weights plot (Yu-Wei)
#par(mfrow=c(3,2))
#gwplot(nn_model, selected.covariate = "cap.shapec")
#gwplot(nn_model, selected.covariate = "cap.shapef")
#gwplot(nn_model, selected.covariate = "cap.shapek")
#gwplot(nn_model, selected.covariate = "cap.shapes")
#gwplot(nn_model, selected.covariate = "cap.shapex")

#par(mfrow=c(1,1)) #reset

  #predictions
nn_pred = compute(nn_model, testset[-96])$net.result #remove "classes" column
nn_predicition = c("edible","poisonous")[apply(nn_pred, 1, which.max)] #obtain other possible labels by finding the column with the greatest probability
pred_table = table(testset$classes, nn_predicition)#generate classification table
pred_table
classAgreement(pred_table)
confusionMatrix(pred_table)

  #ANN Model #2

nn_model_2 <- neuralnet(nn_form , data = trainset , startweights = NULL, hidden=3, err.fct="sse", act.fct="logistic", linear.output = FALSE)
plot(nn_model_2, rep="best")

nn_pred_2 = compute(nn_model_2, testset[-96])$net.result #remove "classes" column
nn_predicition_2 = c("edible","poisonous")[apply(nn_pred_2, 1, which.max)] #obtain other possible labels by finding the column with the greatest probability:
pred_table_2 = table(testset$classes, nn_predicition_2)#generate classification table
pred_table_2
classAgreement(pred_table)
confusionMatrix(pred_table)


##----------------------------------------------------------------