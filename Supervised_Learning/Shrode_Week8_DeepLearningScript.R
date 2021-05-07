## Libraries ---------------------------------------------------------

library(R.utils)
library(h2o)
library(caret)


## Load Dataset ---------------------------------------------------------

download.file("http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz", "train-images-idx3-ubyte.gz")
download.file("http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz", "train-labels-idx1-ubyte.gz")
download.file("http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz", "t10k-images-idx3-ubyte.gz")
download.file("http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz", "t10k-labels-idx1-ubyte.gz")

gunzip("train-images-idx3-ubyte.gz")
gunzip("train-labels-idx1-ubyte.gz")
gunzip("t10k-images-idx3-ubyte.gz")
gunzip("t10k-labels-idx1-ubyte.gz")


#load images
load_image_file <- function(filename) {
  ret = list()
  f = file(filename,'rb')
  readBin(f,'integer',n=1,size=4,endian='big') # Read Magic Number(A constant numerical or text value used to identify a file format)
  ret$n = readBin(f,'integer',n=1,size=4,endian='big') #read number of images
  nrow = readBin(f,'integer',n=1,size=4,endian='big') #read number of rows
  ncol = readBin(f,'integer',n=1,size=4,endian='big') #read number of columns
  x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F) # Read pixels of every image, each image has nrow x ncol pixels
  ret$x = matrix(x, ncol=nrow*ncol, byrow=T) # Store them in a matrix form for easy visulization
  close(f)
  ret
}


trainset <- load_image_file("train-images-idx3-ubyte")
testset <- load_image_file("t10k-images-idx3-ubyte")


#load labels
load_label_file <- function(filename) {
  f = file(filename,'rb')
  readBin(f,'integer',n=1,size=4,endian='big') # Read Magic Number
  n = readBin(f,'integer',n=1,size=4,endian='big') # Read Number of Labels
  y = readBin(f,'integer',n=n,size=1,signed=F) # Read All the Labels
  close(f)
  y
}

trainset_labels <- as.factor(load_label_file("train-labels-idx1-ubyte"))
testset_labels <- as.factor(load_label_file("t10k-labels-idx1-ubyte"))

trainset$labels = trainset_labels
testset$labels  = testset_labels

lengths(trainset)
class(trainset)
head(trainset$labels, 25)


lengths(testset)
class(testset)


## Display Images ---------------------------------------------------------

show_digit <- function(arr784, col=gray(12:1/12), ...) {
  image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}

show_digit(trainset$x[5,]) #show fifth digit
trainset$labels[5]

#display 25 images
par(mfrow=c(5,5))
for(i in 1:25){show_digit(trainset$x[i,])}

#display 25 labels
label_matrix <- t(matrix(trainset$labels[1:25], 5, 5))
label_matrix


par(mfrow=c(1,1)) #reset mfrow


## Deep Learning Model -----------------------------------

h2o.init(nthreads=-1, enable_assertions  = FALSE)
h2o.no_progress() 
set.seed(789)

h2o_train <- as.h2o(trainset)
h2o_test <- as.h2o(testset)

y <- "labels"
x <- setdiff(names(h2o_train),y)

h2o_train[,y] <- as.factor(h2o_train[,y])
h2o_test[,y] <- as.factor(h2o_test[,y])

model <- h2o.deeplearning(x = x,
                          y = y,
                          training_frame  = h2o_train,
                          validation_frame = h2o_test,
                          distribution = "multinomial",
                          activation = "RectifierWithDropout",
                          hidden = c(50,50,50),
                          epochs = 10,
                          input_dropout_ratio = 0.2,
                          l1 = 1e-5,
                          variable_importances = TRUE)


### View Results of "model" ---------------------------------------------------------

model@parameters # View specified parameters of the deep learning model

# Examine the performance of the trained model
model # display all performance metrics

h2o.performance(model)  # training metrics
h2o.performance(model, valid = TRUE) # validation metrics

# Get MSE only
h2o.mse(model, valid = TRUE)

h2o.varimp(model) #retrieve the variable importance

## View Predictions ---------------------------------------------------------

# Classify the test set (predict class labels)
# This also returns the probability for each class
pred <- h2o.predict(model, newdata = h2o_test)
head(pred)

pred_results <- as.data.frame(pred[,1])

confusionMatrix(unlist(pred_results), testset$labels)$overall


## Cartesian Hyper-Parameter Grid Search---------------------------------------------------------

hidden_opt <- list(c(50,50), c(100,100), c(200,200))
l1_opt <- c(1e-4, 1e-5)
hyper_params <- list(hidden = hidden_opt, l1 = l1_opt)
grid_search <- h2o.grid("deeplearning",
                        grid_id = "mygrid",
                        x = x,
                        y = y,
                        hyper_params = hyper_params,
                        training_frame  = h2o_train,
                        validation_frame = h2o_test,
                        distribution = "multinomial",
                        score_interval = 2,
                        epochs = 10,
                        stopping_rounds = 3,
                        stopping_tolerance = 0.05,
                        stopping_metric = "misclassification")





grid_search
for (model_id in grid_search@model_ids) {
  mse <- h2o.mse(h2o.getModel(model_id), valid = TRUE)
  print(sprintf("Test set MSE: %f", mse))
  print(sprintf("Model ID: %s", model_id))
  }


best_model <- grid_search@model_ids[[1]]
h2o.mse(h2o.getModel(best_model))

pred_search <- h2o.predict(h2o.getModel(best_model), newdata = h2o_test)
head(pred_search)

pred_search_results <- as.data.frame(pred_search[,1])

confusionMatrix(unlist(pred_search_results), testset$labels)$overall

## Exploratory Data Analysis (NOT INCLUDED IN ASSIGNMENT) ---------------------------------------------------------


library(DataExplorer)
df_train <- as.data.frame(trainset) 
dim(df_train)
introduce(df_train)
plot_intro(df_train)
summary(df_train$n)
df_train$n <- NULL


df_test <- as.data.frame(testset)
dim(df_test)
introduce(df_test)
plot_intro(df_test)

summary(df_test$n)
df_test$n <- NULL

