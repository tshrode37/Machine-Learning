## Libraries ----------------------------------------------------

library(mlbench)      #dataset
library(DataExplorer) #data exploration
library(ggplot2)      #extra plot options
library(dplyr)        #for general data wrangling needs
library("mice")       #impute missing
library("VIM")        #plot missings
library(ipred)        #bagged decision trees
library(caret)        #split data, general model fitting + bagging
library(pROC)         # ROC and AUC


## Load Data Set ----------------------------------------------------

data("PimaIndiansDiabetes2")
diabetes <- PimaIndiansDiabetes2

## Exploratory Data Analysis----------------------------------------------------

summary(diabetes)
str(diabetes)

introduce(diabetes)
plot_intro(diabetes)

colSums(is.na(diabetes)) #49% are incomplete rows
md.pattern(diabetes, rotate.names = TRUE) #tabular form of missing value present in each variable in a data set
mice_plot <- aggr(diabetes, col=c('purple','orange'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(diabetes), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern")) #plot missing with VIM

tempData <- mice(diabetes,m=5,maxit=50,meth='pmm',seed=500) #impute NA's with mean
summary(tempData)

xyplot(tempData,insulin ~ pregnant + glucose + pressure + triceps + mass + pedigree + age + diabetes,pch=18,cex=1) #compare the distributions of original and imputed data
densityplot(tempData)

head(tempData$imp$insulin) #check imputed data for "insulin"
completeData <- complete(tempData,1) #get complete data set from the first of five data set
dim(completeData)
head(completeData)
summary(completeData)
str(completeData)



plot_bar(completeData,ggtheme = theme_light(base_size = 10), theme_config = list("text" = element_text(color = "darkred")))
plot_histogram(completeData,ggtheme = theme_light(base_size = 10), theme_config = list("text" = element_text(color = "navyblue")))



## Partition Data ----------------------------------------------------

set.seed(789) #make results reproducible
index <- createDataPartition(completeData$diabetes, p =0.75, list = FALSE)
diabetesTrain <- completeData[index,] #index for training set
trainLabels <- completeData[9][index,]
dim(diabetesTrain)


diabetesTest <- completeData[-index,] #not index for test set
testLabels <- completeData[-index,9]
dim(diabetesTest)



## Bagging with ipred package ----------------------------------------------------


set.seed(789) 
bagged_ipred <- bagging(
  formula = diabetes ~ .,
  data = diabetesTrain, 
  coob = TRUE, 
  nbagg = 30
  )

bagged_ipred

bagged_pred <- predict(bagged_ipred, diabetesTest)
confusionMatrix(bagged_pred, testLabels)



bag_ipred <- predict(bagged_ipred, diabetesTest, type = "prob")
head(bag_ipred)
rocCurve_ipred <- roc(testLabels, bag_ipred[,"neg"])
plot(rocCurve_ipred, col= c(6))


pROC::auc(rocCurve_ipred)

# tune ipred model ---------------------

nbagg <- 10:50

# create empty vector to store OOB RMSE values
rmse <- vector(mode = "numeric", length = length(nbagg))


for (i in seq_along(nbagg)) {
  # reproducibility
  set.seed(789)
  
  # perform bagged model
  tune_model <- bagging(
    formula = diabetes ~ .,
    data = diabetesTrain, 
    coob = TRUE, 
    nbagg = nbagg[i]
  )

  # get OOB error
  rmse[i] <- tune_model$err
}


plot(nbagg, rmse, type = 'l', lwd = 2)
abline(v = 25, col = "red", lty = "dashed") #40




# tuned ipred model ---------------------

tuned_model <- bagging(
  formula = diabetes ~ .,
  data = diabetesTrain, 
  coob = TRUE, 
  nbagg = 45
)


confusionMatrix(predict(tuned_model, diabetesTest), testLabels)



tuned_bag_ipred <- predict(tuned_model, diabetesTest, type = "prob")
head(tuned_)
rocCurve_tuned_ipred <- roc(testLabels, tuned_bag_ipred[,"neg"])
plot(rocCurve_tuned_ipred, col= c(1))

pROC::auc(rocCurve_tuned_ipred)

## Bagging with Caret ----------------------------------------

#caret bagging
set.seed(789) #make results reproducible
ctrl <- trainControl(method = "cv",
                     number = 10,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary) #10-fold cross validation


bagged_cv <- train(
  diabetes ~ ., 
  data = diabetesTrain,
  method = "treebag",    #'treebag': bagging method for classification problem in R
  trControl = ctrl, 
  metric = "ROC",
  nbagg = 30
)



bagged_cv

bagged_cv_pred <- predict(bagged_cv, diabetesTest)
confusionMatrix(bagged_cv_pred, testLabels)


bagged_probs <- predict(bagged_cv, diabetesTest, type = "prob")
head(bagged_probs)
rocCurve_bag <- roc(testLabels, bagged_probs[,"neg"])
plot(rocCurve_bag, col= c(7))


## Boosting ----------------------------------------------------

#diabetesTrain$diabetes <- as.factor(ifelse(diabetesTrain$diabetes == 1, "pos", "neg")) #Chiu, 2015

fitControl = trainControl(method = "cv",
                          number = 10,
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)

set.seed(789) #make results reproducible
gbm_fit1 <- train(diabetes ~ ., 
                data = diabetesTrain, 
                method = "gbm", 
                trControl = fitControl,
                metric = "ROC",
                verbose = FALSE)

gbm_fit1
summary(gbm_fit1)

gbm1_pred <- predict(gbm_fit1, diabetesTest)
confusionMatrix(gbm1_pred, testLabels)

trellis.par.set(caretTheme())
plot(gbm_fit1)

boosted_probs <- predict(gbm_fit1, diabetesTest, type = "prob")
head(boosted_probs)
rocCurve_boost1 <- roc(testLabels, boosted_probs[,"neg"])
plot(rocCurve_boost1, col= c(4))


pROC::auc(rocCurve_boost1)



# tune gbm model --------------
gbm_grid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = c(.01, .1, .3),
                        n.minobsinnode = c(5, 10, 15))

nrow(gbm_grid)

set.seed(789) #make results reproducible

train_time <- system.time({
gbm_fit2 <- train(diabetes ~ ., 
                  data = diabetesTrain, 
                  method = "gbm", 
                  trControl = fitControl,
                  metric = "ROC",
                  tuneGrid = gbm_grid,
                  verbose = FALSE)        #indicate whether or not to print out progress and performance indicators
})


train_time


gbm_fit2
summary(gbm_fit2)


gbm2_pred <- predict(gbm_fit2, diabetesTest)
confusionMatrix(gbm2_pred, testLabels)

#ggplot(gbm_fit2)

# final model -----------------

gbm_grid <-  expand.grid(interaction.depth = 9, 
                         n.trees = 100, 
                         shrinkage = .01,
                         n.minobsinnode = 5)

train_time <- system.time({
  gbm_fit3 <- train(diabetes ~ ., 
                    data = diabetesTrain, 
                    method = "gbm", 
                    trControl = fitControl,
                    metric = "ROC",
                    tuneGrid = gbm_grid,
                    verbose = FALSE)       
})


gbm_fit3


summary(gbm_fit3)

gbm3_pred <- predict(gbm_fit3, diabetesTest)
confusionMatrix(gbm3_pred, testLabels)


boosted_probs3 <- predict(gbm_fit3, diabetesTest, type = "prob")
head(boosted_probs3)
rocCurve_boost3 <- roc(testLabels, boosted_probs3[,"neg"])
plot(rocCurve_boost3, col= c(10))

pROC::auc(rocCurve_boost3)



## Model Comparisons -----------------------------------

plot(rocCurve_bag, col= c(7)) #bagged cv tree, color = yellow
plot(rocCurve_boost1 ,add=TRUE,col= c(4)) #default gbm, color = blue
plot(rocCurve_boost3,add=TRUE, col= c(10)) #tuned gbm, color = red









##################################################################################
## CODE NOT USED ----------------------------------------------------

# simple model --------------------------------------------------

#diabetesTrain$diabetes <- ifelse(diabetesTrain$diabetes == "pos", 1, 0) #Chiu, 2015


gbm.fit <- gbm(
  formula = diabetes ~ .,     #model to be fit
  distribution = "bernoulli", #specify distribution; default will guess; gaussian = squared error
  data = diabetesTrain,       #data frame containing variables in model
  n.trees = 10000,            #number of trees to fit
  interaction.depth = 1,      #depth of each tree (1 means ensembling a bunch of stumps)
  shrinkage = 0.001,          #learning rate
  cv.folds = 5,               #number of folds for cross validation
  n.cores = NULL,             #will use all cores by default (number of CPU cores to use)
  verbose = FALSE             #indicate whether or not to print out progress and performance indicators
)  

print(gbm.fit)

gbm.perf(gbm.fit, method = "cv") # plot loss function as a result of n trees added to the ensemble

summary(gbm.fit, gbm.perf(gbm.fit, method = "cv"))

best <- which.min(gbm.fit$cv.error) #find index for number trees with minimum CV error
sqrt(min(gbm.fit$cv.error[best])) # get MSE and compute RMSE


#https://allstate-university-hackathons.github.io/PredictionChallenge2016/GBM
gbm.predict = predict(gbm.fit, diabetesTrain, n.trees = gbm.perf(gbm.fit, method = "cv"), type = "response")
head(data.frame("Actual" = trainLabels, 
                "PredictedProbability" = gbm.predict))

# tune gbm model -------------------------------------------

hyper_grid <- expand.grid(           #create hyperparameter grid
  shrinkage = c(.01, .1, .3),        #learning rate
  interaction.depth = c(1, 3, 5),  #depth of each tree (1 means ensembling a bunch of stumps)
  n.minobsinnode = c(5, 10, 15),     #number of observations in terminal nodes of trees
  bag.fraction = c(.65, .8, 1),      #fraction of the training set observations selected to propose the next tree in expansion
  optimal_trees = NA,                #a place to dump results
  min_RMSE = NA                      #a place to dump results
)

nrow(hyper_grid)
hyper_grid[1]
seq_len(nrow(hyper_grid))

for (i in seq_len(nrow(hyper_grid))){
  # fit gbm
  set.seed(789)  # for reproducibility
  train_time <- system.time({
      tune.gbm <- gbm(
      formula = diabetes ~ .,
      data = diabetesTrain,
      distribution = "bernoulli",
      n.trees = 10000, 
      shrinkage = hyper_grid$shrinkage[i],
      interaction.depth = hyper_grid$interaction.depth[i],
      n.minobsinnode = hyper_grid$n.minobsinnode[i],
      bag.fraction = hyper_grid$bag.fraction[i],
      cv.folds = 5,
      n.cores = NULL,             #will use all cores by default (number of CPU cores to use)
      verbose = FALSE             #indicate whether or not to print out progress and performance indicators
    )
  })
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(tune.gbm$cv.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(tune.gbm$cv.error))
}



hyper_grid %>% dplyr::arrange(min_RMSE) %>% head(10)

train_time


# tuned gbm --------------------------------------------------------

tuned.gbm <- gbm(
  formula = diabetes ~ .,
  data = diabetesTrain,
  distribution = "bernoulli",
  n.trees = 462, 
  shrinkage = 0.01,
  interaction.depth = 3,
  n.minobsinnode = 5,
  cv.folds = 5,
  n.cores = NULL,             #will use all cores by default (number of CPU cores to use)
  verbose = FALSE             #indicate whether or not to print out progress and performance indicators
)


gbm.perf(tuned.gbm, method = "cv") # plot loss function as a result of n trees added to the ensemble

print(tuned.gbm)
summary(tuned.gbm, gbm.perf(tuned.gbm, method = "cv"))


sqrt(min(tuned.gbm$cv.error))


as.factor(ifelse(tuned.gbm.prd>0.5, 0,1))
tuned.gbm.pred <- predict(tuned.gbm, diabetesTest, n.trees = tuned.gbm$n.trees)

head(tuned.gbm.pred)
str(tuned.gbm.pred)

RMSE(gbm.pred, testLabels)


