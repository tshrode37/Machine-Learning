###########
##PROBLEM #1: Use the Sign Test to test the median rating whether it is at least 3. 
  #Given 95% confidence interval.

ratings <- c(5,3,2,1,4,3,5,1,5,2,3,4,2,1,3) #(1=terrible, 5=excellent)
particpants <- c(1:15) #Number of people asked to rate restaurant
souperb_ratings <- data.frame(particpants, ratings)
View(souperb_ratings)
median(souperb_ratings$ratings)

##Visualize data
#Histogram: view distribution of data
hist(souperb_ratings$ratings, xlab = "Ratings", main = "Histogram of Souperb Ratings")
#QQplot: view outliers
library(car)
qqPlot(souperb_ratings$ratings)
#OR
qqnorm(souperb_ratings$ratings, pch = 1, frame = TRUE)
qqline(souperb_ratings$ratings, col = "violet", lwd = 2)

##Hypotheses
#H0: The median of the ratings is at least 3 (greater than or equal to 3). 
#HA: The median of the ratings is less than 3. 

##Binomial Test: https://cran.r-project.org/web/packages/distributions3/vignettes/one-sample-sign-tests.html
sort(ratings) 
  #B=5: sum(ratings>3)
  #N=11: sum(ratings !=3)
binom.test(5, n = 11, p = 0.5, alternative = 'less', conf.level = 0.95) #for H0 is greater than or equal to 3

##Sign Test
install.packages('BSDA')
library(BSDA)
SIGN.test(souperb_ratings$ratings, md = 3, conf.level = 0.95, alternative = "less")

############
##PROBLEM #2: Two different operating systems (M, W) are rated using 1-10 scale (the higher the better rate). 
  #Do the operating systems have the same distribution? What statistical test will you use? 
  #Given the significance level = 0.05. The data are assumed to be paired.

M <- c(9,8,5,3,6,10,4,2,8,7)
W <- c(7,6,8,2,9,5,1,4,7,10)
operating_ratings <- data.frame(operating_system = rep(c("M", "W"), each = 10), rating = c(M, W))
#OR: operating_ratings <- data.frame(M,W)

View(operating_ratings) #or print(operating_ratings)
##Summary
library(dplyr)
group_by(operating_ratings, operating_system) %>% summarise(count = n(), median = median(rating, na.rm = TRUE), IQR = IQR(rating, na.rm = TRUE))

##Visualize Data
#Boxplot
library(ggpubr)
ggboxplot(operating_ratings, x = "operating_system", y = "rating", color = "operating_system", palette = c("orange", "violet"), ylab = "Rating", xlab = "Operating System", main = "Boxplots for Operating System Ratings")

library(ggplot2)
#Histogram: http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
ggplot(operating_ratings, aes(x=rating, color=operating_system)) + geom_histogram(fill="white", bins = 20, position = 'dodge') + scale_color_brewer(palette="Set1")

##Wilcoxon Signed Rank Test
#H0: The distribution of ratings for M and W are the same (median differnce = 0).
#HA: The distribution of ratings for M and W are not the same (median differnce != 0).

#Notes from Professor:
  #H0: these two samples come from the same distribution
  #H1: these two samples do not come from the same distribution
install.packages('exactRankTests')
library(exactRankTests)
wilcox.exact(M, W, alternative = 'two.sided', paired  = TRUE, conf.level = 0.95)
