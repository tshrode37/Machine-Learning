########################### Load data: https://www.kaggle.com/gagandeep16/car-sales
car_sales <- read.csv(file.choose(), header = TRUE)

##Check for missing data: TRUE = complete data, FALSE = missing data
complete.cases(car_sales)

##Remove rows with missing values
complete_car_sales <- car_sales[complete.cases(car_sales), ]
complete.cases(complete_car_sales)
View(complete_car_sales)
summary(complete_car_sales)
str(complete_car_sales)

#create full scatterplot: may need to convert to use as.numeric(); create copy of data
car.sales <- complete_car_sales
#convert data using as.numeric
car.sales$Vehicle_type <- as.numeric(car.sales$Vehicle_type) #2 = passengers, 1 = car
car.sales$Vehicle_type <- as.numeric(car.sales$Vehicle_type)
cor_data <- car.sales[,c(1, 3:14)] 
plot(cor_data, lower.panel = NULL, pch = 19, cex = 0.5)

#calculate correlation coefficients
res <- cor(cor_data)
round(res,3)

library(corrplot)
#use corrplot() to create correlogram: http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

###########################SLR: Predict Price based Horsepower 

cor(car.sales$Price_in_thousands, car.sales$Horsepower)
slr_model <- lm(Price_in_thousands~Horsepower, car.sales)
par(mfrow=c(2,2))
plot(slr_model)
summary(slr_model)

par(mfrow=c(1,1)) #reset
plot(car.sales$Horsepower, car.sales$Price_in_thousands, main = 'Horsepower v.s. Price of Vehicle', xlab = 'Horsepower', ylab = 'Price')abline(slr_model, col = 'red')
abline(slr_model, col = 'red')

#make a prediction
new_hp <- data.frame(Horsepower = 330) #make prediction when hp = 330
predict(slr_model, new_hp) #predict price

#check for normality
slr_residuals <- slr_model$residuals
shapiro.test(slr_residuals)
hist(slr_residuals, main="Histogram of standardised residuals",xlab="Standardised residuals")


########################### MLR
myvars <- car.sales[,c(6, 7, 8, 12, 14)]
plot(myvars, lower.panel = NULL)
mlr_model <- lm(Price_in_thousands ~ Fuel_efficiency+Engine_size+Horsepower+Curb_weight, car.sales)

#backwards elimination; removed Fuel_efficiency
mlr2_model <- lm(Price_in_thousands ~ Engine_size+Horsepower+Curb_weight, car.sales)
summary(mlr2_model)
vif(mlr2_model)
#qf(1-alpha, df1, df2): can compare the F-statistic with F-critical value. df = degree of freedom
qf(0.95, 3, 113)

#add Width and Length
mlr3_model <- lm(Price_in_thousands ~ Engine_size+Horsepower+Curb_weight+Width+Length, car.sales)
summary(mlr3_model)

#remove Width
mlr4_model <- lm(Price_in_thousands ~ Engine_size+Horsepower+Curb_weight+Length, car.sales)
summary(mlr4_model)
qf(0.95, 4, 112)
AIC(mlr4_model) #https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/AIC
library(car)
vif(mlr4_model)
par(mfrow=c(2,2))
plot(mlr4_model)

par(mfrow=c(1,1))

mlr_residuals <- mlr4_model$residuals
shapiro.test(mlr_residuals)
hist(mlr_residuals, main="Histogram of standardised residuals",xlab="Standardised residuals")

#compare models: anova(model1,model2)
  #If the p-value is less than 0.05, then a larger model is significantly 
  #better than a smaller model. In other words, additional variables contribute 
  #significantly to the response.

anova(mlr2_model,mlr4_model) #larger model is significantly better than smaller model
########################### One-way ANOVA: mean of sales of Passenger vs car 
library(dplyr)
vehicle_ANOVA <- data.frame(select(complete_car_sales, Vehicle_type, Sales_in_thousands))

#Histogram
library(ggplot2)

#Boxplot: Sales in Thousands by Vehicle Type
ggplot(complete_car_sales, aes(x=Vehicle_type, y=Sales_in_thousands, fill=Vehicle_type)) + geom_boxplot() + labs(x = 'Vehicle Type', y = 'Sales in Thousands', title = 'Boxplots for Sales in Thousands by Vehicle Type')

#Histogram: Sales in Thousands by Vehicle Type
ggplot(vehicle_ANOVA, aes(x=Sales_in_thousands, color=Vehicle_type)) + geom_histogram(fill="white", bins = 20, position = 'dodge') + scale_color_brewer(palette="Set2")

#mean by group
#Group by Vehicle_type; stats for Sales_in_thousands
library(dplyr)
group_by(complete_car_sales, Vehicle_type) %>% summarise(count = n(), mean_Sales_in_thousands = mean(Sales_in_thousands, na.rm = TRUE))


#calculate F critical value
qf(0.95,1,115) #Reject H0 if (calculated)F > F-critical value

vehicle_aov_model<- aov(Sales_in_thousands~Vehicle_type, vehicle_ANOVA)
summary(vehicle_aov_model)
par(mfrow=c(2,2)) # multiple plots with 2 by 2 using mfrow
plot(vehicle_aov_model)

TukeyHSD(vehicle_aov_model) #pairwise comparison
plot(TukeyHSD(vehicle_aov_model))

#check for normality
residuals<-vehicle_aov_model$residuals
shapiro.test(residuals)
hist(residuals, main="Histogram of standardised residuals",xlab="Standardised residuals")

#check for equal variances
library(car)
leveneTest(Sales_in_thousands~Vehicle_type, vehicle_ANOVA) #Levene's test for equality of variances
#As p - value (0.006199) < 0.05, equal variances can not be assumed

########################### Two-way Anova: Manufacturer and Vehicle type vs sales in thousands
library(dplyr)
#Group by Manufacturer; stats for Sales_in_thousands
group_by(complete_car_sales, Manufacturer) %>% summarise(count = n(), mean_Sales_in_thousands = mean(Sales_in_thousands, na.rm = TRUE))

table(complete_car_sales$Manufacturer, complete_car_sales$Vehicle_type)

#Boxplots
  #add labels: https://ggplot2.tidyverse.org/reference/labs.html
  #format axis: http://www.sthda.com/english/wiki/ggplot2-axis-scales-and-transformations
library(ggplot2)
#Sales in Thousands by Manufacturer
ggplot(complete_car_sales, aes(x=Manufacturer, y=Sales_in_thousands, fill=Manufacturer)) + geom_boxplot() + labs(y = 'Sales in Thousands', title = 'Boxplots for Sales in Thousands by Manufacturer') + theme(axis.text.x = element_text(angle=60))
#Sales in Thousands by Vehicle Type
ggplot(complete_car_sales, aes(x=Vehicle_type, y=Sales_in_thousands, fill=Vehicle_type)) + geom_boxplot() + labs(y = 'Sales in thousands', title = 'Boxplots for Sales in Thousands by Vehicle Type')
#Sales in Thousands by Manufacturer*Vehicle Type: https://www.r-bloggers.com/box-plot-with-r-tutorial/

ManufacturerCount <- length(unique(complete_car_sales$Manufacturer))
2*ManufacturerCount #how many colors needed for boxplot
boxplot(Sales_in_thousands~Manufacturer*Vehicle_type, data = complete_car_sales, las = 2, par(mar = c(12, 5, 4, 2)+ 0.1), xlab='Manufacturer*Vehicle Type', ylab = "Sales in Thousands", main="Manufacturer*Vehicle Type v.s. Sales", col = colors(52))

#Interaction Plots
#Arguments: https://rdrr.io/r/stats/interaction.plot.html

#Manufacturer vs Sales in Thousands
interaction.plot(x.factor = complete_car_sales$Vehicle_type, trace.factor = complete_car_sales$Manufacturer, response = complete_car_sales$Sales_in_thousands, fun = mean, type = 'l', xlab = "Vehicle Type", ylab = "Mean of Sales", lwd = 2, trace.label = "Manufacturer")
#Vehicle Type vs Sales in Thousands
interaction.plot(x.factor = complete_car_sales$Manufacturer, trace.factor = complete_car_sales$Vehicle_type, response = complete_car_sales$Sales_in_thousands, fun = mean, type = 'l', xlab = "Manufacturer", ylab = "Mean of Sales", lwd = 2, trace.label = "Vehicle Type")

#check interaction effect
interaction_aov <- aov(Sales_in_thousands ~ Vehicle_type + Manufacturer + Vehicle_type:Manufacturer, complete_car_sales)
summary(interaction_aov)

#check main effects
main_aov <- aov(Sales_in_thousands ~ Vehicle_type+Manufacturer, complete_car_sales)
summary(main_aov)
#qf(0.95,1,115) #Reject H0 if (calculated)F > F-critical value

#pairwise comparison
tukey_interaction <-TukeyHSD(interaction_aov, conf.level = 0.95)
tukey_interaction
plot(tukey_interaction)

#check assumptions
  #normality
residual_aov <- residuals(interaction_aov)
hist(residual_aov, main="Histogram of standardised residuals",xlab="Standardised residuals")
shapiro.test(residual_aov)
plot(interaction_aov,2) #Normal QQ 

  #Homogeneity of variances: Bartlett Test and fitted vs residuals plot
#bartlett.test(Sales_in_thousands~interaction(Manufacturer,Vehicle_type), data = complete_car_sales) #check homogeneity of variances for interaction - data is normal or nearly normal
#bartlett.test(Sales_in_thousands~Manufacturer, data = complete_car_sales) #check variances on Manufacturer
#bartlett.test(Sales_in_thousands~Vehicle_type, data = complete_car_sales) # check variances on Vehicle Type

library(car)
leveneTest(Sales_in_thousands~Vehicle_type*Manufacturer, complete_car_sales)

plot(interaction_aov,1) #residuals vs fitted  

########################### Logistic Regression: Use sales to predict vehicle type 
#'select' specific columns; need library(dplyr) for select()
library(dplyr)
vehicle_log <- data.frame(select(complete_car_sales, Manufacturer, Sales_in_thousands, Vehicle_type))
View(vehicle_log)
summary(vehicle_log)

vehicle_log$Vehicle_type <- as.numeric(vehicle_log$Vehicle_type) #2 = passengers (presence), 1 = car (absence)
vehicle_log$Vehicle_type <- as.factor(vehicle_log$Vehicle_type) 
summary(vehicle_log)

table(vehicle_log$Vehicle_type)

#Visuals: Scatterplot and boxplot
plot(vehicle_log$Sales_in_thousands, vehicle_log$Vehicle_type, xlab = 'Sales in Thousands', ylab = 'Vehicle Type', main = 'Sales v.s. Vehicle Type')
library(ggpubr)
ggboxplot(vehicle_log, x = "Vehicle_type", y = "Sales_in_thousands", color = "Vehicle_type", palette = c("darkcyan", "darkorchid3"), ylab = 'Sales in Thousands', xlab = "Vehicle Type", main = "Boxplots for Sales by Vehicle Type")

#run logit model
vehicle_glm <- glm(Vehicle_type~Sales_in_thousands, vehicle_log, family = binomial)
summary(vehicle_glm)
  #For unit change in Sales, the log odds of the vehicle being a passenger decrease by 0.008182 OR
  #For one unit change in sales, the odds of the vehicle being a car go up by exp(-0.008182) = 0.9918154. In other
    # words, the odds of the vehicle being a passenger vehicle are about 0.9918512 times for each additional increase in sales
exp(-0.008182)
exp(coef(vehicle_glm))
exp(confint.default(vehicle_glm)) #95% confidence interval (0.9857528, 0.9979874)

par(mfrow=c(2,2))
plot(vehicle_glm)

#test overall fit of model: Likelihood test
anova(vehicle_glm,test='Chisq') #where vehicle_glm is model fitted above
#same as 1-pchisq(131.03-122.81, 116-115)

#reset mfrow
par(mfrow=c(1,1))

#glm plotting: https://www.theanalysisfactor.com/r-glm-plotting/
  #cannot plot because Vehicle type is not 0/1
range(vehicle_log$Sales_in_thousands) #0.11 - 540.561
xsales <- seq(0, 600, 10)
ysales <- predict(vehicle_glm, list(Sales_in_thousands = xsales),type="response")
plot(vehicle_log$Sales_in_thousands,vehicle_log$Vehicle_type, pch = 16, xlab = "Sales", ylab = "Vehicle Type", main = 'Sales v.s. Vehicle Type')
lines(xsales, ysales)

#Predict vehicle type 
newsales <- data.frame(Sales_in_thousands = 100)
predict(vehicle_glm, newsales, type = 'response') # = 0.6954908
  #same as logit(p(x)) = intercept + sales-coeff * sales
          # logit(p(x)) = 1.644131 - 0.008182*sales,
          #thus, the log odds for sales = 100 is 0.825931. So, odds is exp(0.825931) = 2.284006
          # prob = odds/1+odds = 0.6954939

newsales <- data.frame(Sales_in_thousands = 0.2)
predict(vehicle_glm, newsales, type = 'response')

newsales <- data.frame(Sales_in_thousands = c(0.2, 10, 100, 200, 350))
predict(vehicle_glm, newsales, type = 'response')

#determine goodness of fit
library(ResourceSelection)
hoslem.test(vehicle_log$Vehicle_type, fitted(vehicle_glm))

#wald test
wald_stat <- (-0.008182/0.003147)^2 #6.75967
wald_stat
###########################
