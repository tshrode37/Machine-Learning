library(ISLR)
data('Carseats')
View(Carseats)

summary(Carseats)
str(Carseats)

#convert data using as.numeric
Carseats$ShelveLoc <- as.numeric(Carseats$ShelveLoc)
Carseats$Urban <- as.numeric(Carseats$Urban)
Carseats$US <- as.numeric(Carseats$US)
###Check for linearity

#Scatteplots
plot(Carseats, lower.panel = NULL, pch = 19, cex = 0.5)
myvars = c('Sales', 'Advertising', 'Price', 'Age')
Carseats2 = Carseats[myvars]
Carseats2
plot(Carseats2, lower.panel = NULL)

###fit linear model

sales.model <- lm(Sales ~ Advertising + Price + Age, Carseats)
sales.model
#multiple plots with 2 by 2
par(mfrow=c(2,2))
plot(sales.model)
summary(sales.model)

##detecting multicollinearity: create correlation matrix, calculate VIF

#calculate correlation coefficients
res <- cor(Carseats)
round(res,3)

install.packages('corrplot')
library(corrplot)
#use corrplot() to create correlogram 
#http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram

corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

#detecting multicollinearity: http://www.sthda.com/english/articles/39-regression-model-diagnostics/160-multicollinearity-essentials-and-vif-in-r/
#vif() function from car package

library('car')
vif(sales.model)

###test significance of each explanatory variable
#Sales~Advertising
sales.ad.model <- lm(Sales~Advertising, Carseats)
sales.ad.model
summary(sales.ad.model)

#Sales~Price
sales.price.model <- lm(Sales~Price, Carseats)
sales.price.model
summary(sales.price.model)

#Sales~Age
sales.age.model <- lm(Sales~Age, Carseats)
sales.age.model
summary(sales.age.model)

###New MLR model: Add 'Income'
new.sales.model <- lm(Sales~Advertising+Price+Age+Income, Carseats)
new.sales.model
summary(new.sales.model)
par(mfrow=c(2,2))
plot(new.sales.model)
vif(new.sales.model)

