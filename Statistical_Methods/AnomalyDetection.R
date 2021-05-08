## Libraries ----------------------------------------------------------

library(DataExplorer)
library(ggplot2)
library(dplyr)
library(outliers) #grubbs test + chi-squared
library(car)
library(h2o)
library(AnomalyDetection)


## Load Data ----------------------------------------------------------

file_path <- "C:/Users/07hoc/OneDrive/Documents/Data Science Documents/wk7_eq.csv"
earthquake <- read.csv(file_path)
head(earthquake)


## Exploratory Data Analysis ----------------------------------------------------------

str(earthquake)
introduce(earthquake)
plot_intro(earthquake)
plot_histogram(earthquake, ggtheme = theme_light(base_size = 10), theme_config = list("text" = element_text(color = "darkblue"))) #plot continuous features



#boxplot to detect potential outliers

ggplot(earthquake, aes(y=year)) +
geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=1, notch=TRUE) #boxplot for year


ggplot(earthquake, aes(y=earthquakes)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=1, notch=TRUE) #boxplot for earthquakes


boxplot.stats(earthquake$earthquakes)$out


rows <- which(earthquake$earthquakes %in% c(boxplot.stats(earthquake$earthquakes)$out))
rows


#ggplot(earthquake, aes(x=year, y=earthquakes, group = earthquakes)) + geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=1, notch=FALSE) #boxplot for year x earthquakes

#line/scatter plot
#ggplot(earthquake, aes(x=year, y=earthquakes, color=earthquakes)) + geom_point()

#percentiles to detect potential outliers 

lower_bound_year <- quantile(earthquake$year, 0.025)
lower_bound_year #anything below 1902.45 is considered outlier

upper_bound_year <- quantile(earthquake$year, 0.975) 
upper_bound_year #anything above 1995.55 is considered outlier


outlier_year <- which(earthquake$year < lower_bound_year | earthquake$year  > upper_bound_year) #gives row numbers 
outlier_year

earthquake[outlier_year, "year"]

outlier_earthquakes <- which(earthquake$earthquakes < quantile(earthquake$earthquakes, 0.025) | earthquake$earthquakes  > quantile(earthquake$earthquakes, 0.975)) #gives row numbers 
earthquake[outlier_earthquakes, ]

##-------Outlier Tests----------##

## Grubb's Test --------------------------------------------------------
  #https://www.statology.org/grubbs-test-r/

qqPlot(earthquake$year)
shapiro.test(earthquake$year)

qqPlot(earthquake$earthquakes)
shapiro.test(earthquake$earthquakes)


grubbs.test(earthquake$earthquakes, type = 10) #test on the highest value
 # p-value = 0.1594, we do not reject that the highest value is not an outlier

grubbs.test(earthquake$earthquakes, opposite = TRUE, type = 10) #test on the lowest value
# p-value = 1, we do not reject that the highest value is not an outlier

grubbs.test(earthquake$earthquakes, type = 11) #test on the both max andd min values



 grubbs.test(earthquake$year, type = 11) #test on the highest value
 # p-value = 1, we do not reject that the highest value is not an outlier

grubbs.test(earthquake$year, opposite = TRUE, type = 11) #test on the lowest value
# p-value = 1, we do not reject that the highest value is not an outlier

grubbs.test(earthquake$year, type = 11) #test on the both max and min values



sample.quake <- earthquake[sample(nrow(earthquake), 30),] 
grubbs.test(sample.quake$year, type = 20)


grubbs.test(sample.quake$earthquakes, type = 20)
#found outliers


## Chi-Squared Test for Outliers --------------------------------------------------------
  #https://uribo.github.io/rpkg_showcase/statistics/outliers.html

chisq.out.test(earthquake$year)
chisq.out.test(earthquake$year, opposite = TRUE)

chisq.out.test(earthquake$earthquakes)
chisq.out.test(earthquake$earthquakes, opposite = TRUE)

##-------Outlier Techniques----------##

## AnomalyDetection ----------------------------------------------------------


earthquake$year <- as.POSIXct(as.character(earthquake$year), format = '%Y')
anomaly_1 <- AnomalyDetectionTs(earthquake, max_anoms=0.02, alpha = 0.05, direction='both', plot=TRUE)
anomaly_1$plot


anomaly_2 <- AnomalyDetectionTs(earthquake, max_anoms=0.1, alpha = 0.25, direction='both', plot=TRUE)
anomaly_2$plot

anomaly_vec <- AnomalyDetectionVec(earthquake$earthquakes, max_anoms=0.02, period=40, direction='both', only_last=FALSE, plot=TRUE)
anomaly_vec$anoms
anomaly_vec$plot

## Anomaly Detection with Deep Autoencoders H2O----------------------------------------------------------------


h2o.init()
earthquake_h2o <- as.h2o(earthquake)
earthquake_h2o$year <- as.factor(earthquake_h2o$year)
splits <- h2o.splitFrame(earthquake_h2o, ratios = 0.75, seed = 789) #partition data into 70% (training), ~30% (testing)


train <- splits[[1]]
dim(train)

test <- splits[[2]]
dim(test)


anomaly_model <- h2o.deeplearning(x = "year",
                                  training_frame = train,
                                  autoencoder = TRUE,
                                  reproducible = TRUE,
                                  seed = 789,
                                  ignore_const_cols = FALSE,
                                  model_id = "Test01",
                                  activation ="Tanh",
                                  hidden = c(200, 200),
                                  epochs = 10
)
anomaly_model

anomaly_score <- as.data.frame(h2o.anomaly(anomaly_model,
                                           train,
                                           per_feature = FALSE
)) #Detect anomalies in an H2O data set using an H2O deep learning model with auto-encoding trained previously.
anomaly_score$y = 0
head(anomaly_score)

threshold = quantile(anomaly_score$Reconstruction.MSE, probs = 0.975) # Calculate the threshold value for train anomaly scores
#Various methods can be used such as calculating the quantiles, max, median, min etc. It all depends on the use case. Here we will use quantile with probability of 97.5%.

#Now, we have anomaly score for train and its thresholds, we can predict the new anomaly scores for test data and plot it to see how it differs from train data.
test_score <- as.data.frame(h2o.anomaly(anomaly_model,
                                        test,
                                        per_feature = FALSE
))

test_score$y = 1


results <- data.frame(rbind(anomaly_score,test_score), threshold)
head(results)

plot(results$Reconstruction.MSE, type = 'n', xlab='observations', ylab='Reconstruction.MSE', main = "Anomaly Detection Results")
points(results$Reconstruction.MSE, pch=19, col=ifelse(results$Reconstruction.MSE < threshold, "green", "red"))
abline(h=threshold, col='red', lwd=2)




##----------------------------------------------------------
