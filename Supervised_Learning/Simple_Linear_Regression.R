#To use 'Carseats' dataset from ISLR package
install.packages("ISLR")
library(ISLR)
data("Carseats")
View(Carseats)
summary(Carseats)

#plot sales vs Price

plot(Carseats$Sales, Carseats$Price, main = 'Sales vs Price', xlab = 'Sales', ylab = 'Price')

#create a color scheme for matrix scatterplot

my_cols <- c('green', 'darkorchid', 'firebrick1')

#create matrix scatterplot of Sales, Age, Income, and price columns and remove lower half of plot

pairs(~Sales+Age+Income+Price, data = Carseats, lower.panel = NULL, col = my_cols[Carseats$ShelveLoc], pch = 20, cex = .5)

#Change non-numeric columns to numbers

Carseats$Urban <- as.numeric(Carseats$Urban)
Carseats$ShelveLoc <- as.numeric(Carseats$ShelveLoc)
Carseats$US <- as.numeric(Carseats$US)

#Compute correlation matrix, data must be numeric. Reason for commands above 

cor(Carseats)

#save correlation in variable called 'res'

res <- cor(Carseats)

#round correlations to 3 decimals 

round(res, 3)

#can 'select' specific columns to use in cor(), but need library(dplyr) for select()

library(dplyr)
res2 <- cor(select(Carseats,Sales,Age,Income,Price))
round(res2, 3)

#Can also specify by columnn number: cor(select(Carseats,1:6,8:9))

#install.packages('Hmisc')
#from http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
#library(Hmisc)
#The output of the function rcorr() is a list containing the following elements : 
#r : the correlation matrix - n : the matrix of the number of observations used in analyzing each pair of variables 
#P : the p-values corresponding to the significance levels of correlations.
#res3 <- rcorr(as.matrix(Carseats))
#res3

install.packages('PerformanceAnalytics')
library('PerformanceAnalytics')

#from http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
#can be used to display a chart of a correlation matrix.
#command below creates a correlation matrix of entire data set

chart.Correlation(Carseats, histogram=TRUE,pch=19)

#correlation matrix below is included in the HW
data <- Carseats[,c(1,6,8,4)]
chart.Correlation(data, histogram=TRUE,pch=19)

#fit linear model
lm(Sales~Price, Carseats)

#save linear model outputs in variable 
lm_model <- lm(Sales~Price, Carseats)

#regression line added on top of the previous plot
abline(lm(Sales~Price, Carseats), col = 'red')

#multiple plots with 2 by 2
par(mfrow=c(2,2))

#fit information - displays 4 charts: 
#residuals vs fitted, normal-QQ, Scale-location, residuals vs leverage
plot(lm_model)

#shows the model formula, residual quartiles, coefficient estimate with std error, and a significance test,
#multiple and adjusted R-square, and F-test for model fit
summary(lm_model)
