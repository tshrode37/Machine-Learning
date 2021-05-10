####reading data into R with URL: http://www.sthda.com/english/wiki/reading-data-from-txt-csv-files-r-base-functions
my_data <- read.csv("https://raw.githubusercontent.com/ywchiu/rcookbook/master/chapter5/engineer.csv")
View(my_data)
summary(my_data)
str(my_data)

table(my_data$Region, my_data$Profession) #Generate frequency table to check for balanced design

####plot boxplots
  #color palettes: https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
boxplot(Salary~Profession, data = my_data, xlab='Profession', ylab = "Salary", main="Profession v.s. Salary", col = rainbow(3))
boxplot(Salary~Region, data = my_data, xlab='Region', ylab = "Salary", main="Region v.s. Salary", col = cm.colors(3))
boxplot(Salary~Region*Profession, data = my_data, xlab='Region*Profession', ylab = "Salary", main="Region*Profession v.s. Salary", col = heat.colors(3))

#####find medians
library(data.table)
data_dt <- as.data.table(my_data)
#median of salary by profession
data_dt[, median(Salary), by = Profession]
#median of salary by profession
data_dt[, median(Salary), by = Region]

####check assumptions of two-way ANOVA: normality, independence, homogeneity
  #Normality: Histogram of residuals, Shapiro-wilks test, and Normal Q-Q plot 
#Histogram
model <- lm(Salary ~ Region + Profession + Region:Profession, my_data)
residuals<-model$residuals #or residuals <- residuals(interaction_aov)
hist(residuals, main="Histogram of standardised residuals",xlab="Standardised residuals")
#Shapiro-Wilks test
shapiro.test(residuals)

  #Homogeneity of variances:Levene's Test and fitted vs residuals plot
#Levene's Test 
library(car)
leveneTest(interaction_aov)

####interaction effects plot interaction.plot
#Arguments: https://rdrr.io/r/stats/interaction.plot.html

  #Region vs Salary
interaction.plot(x.factor = my_data$Region, trace.factor = my_data$Profession, response = my_data$Salary, fun = mean, type = 'l', col = c("springgreen3", "blue2", "darkred"), xlab = "Region", ylab = "Mean of Salary", lwd = 2, trace.label = "Profession")
  #profession vs Salary
interaction.plot(x.factor = my_data$Profession, trace.factor = my_data$Region, response = my_data$Salary, fun = mean, type = 'l', col = c("springgreen3", "blue2", "darkred"), xlab = "Profession", ylab = "Mean of Salary", lwd = 2, trace.label = "Region")

####two-way ANOVA with interaction effect
interaction_aov <- aov(Salary ~ Region + Profession + Region:Profession, my_data)
summary(interaction_aov)
plot(interaction_aov)
plot(interaction_aov,2)
plot(interaction_aov,1)

#post hoc test for interaction effect

tukey_interaction <- TukeyHSD(interaction_aov, conf.level = 0.95)
tukey_interaction
plot(tukey_interaction)

####main effects test: If no interaction effect, test main effects
#region
region_aov <- aov(Salary ~ Region, my_data)
summary(region_aov)

#profession
prof_aov <- aov(Salary ~ Profession, my_data)
summary(prof_aov)

#post hoc tests for main effects: Compare the means of the three levels separately for the two factors using Tukey's method
par(mfrow=c(1,1)) #reset mfrow
tukey_region <- TukeyHSD(interaction_aov, conf.level = 0.95, which = 'Region')
tukey_region
plot(tukey_region)

tukey_profession <- TukeyHSD(interaction_aov, conf.level = 0.95, which = 'Profession')
tukey_profession
plot(tukey_profession)

