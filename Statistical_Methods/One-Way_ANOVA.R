#Load treatmeant data
treatment = c(rep("A",4), rep("B", 4), rep("C",4))  # or rep(LETTERS[1:3],each=4)
  #Treatment type = A, B, C
  #Each have 4 participants (thus, 4 rows of data per treatment)
time_to_relief = c(14,24,12,25,20,14,17,18,22,29,36,20)
  #recorded time (in minutes) when the pain eases off
pain_data <- data.frame(treatment, time_to_relief)
  #combine data
View(pain_data)
str(pain_data)
summary(pain_data)


#visualize time_to_relief with boxplot
boxplot(pain_data$time_to_relief, horizontal = TRUE, main="Relief Time Distribution across all Treatments", col = "orchid")
#visualize data by group
boxplot(pain_data$time_to_relief~pain_data$treatment, main="Boxplot comparing Time to Relief of Three Treatment Types", xlab = "Treatment", ylab = "Time to Relief", col= heat.colors(3))

#ANOVA
pain_aov <- aov(time_to_relief~treatment, pain_data) #create model
names(pain_aov) #command gives a sense of all the information pain_aov
summary(pain_aov) #summary of model
par(mfrow=c(2,2)) # multiple plots with 2 by 2 using mfrow
plot(pain_aov)

#plot residuals in a histogram: https://www.sheffield.ac.uk/polopoly_fs/1.536445!/file/MASH_ANOVA_in_R.pdf
residuals<-pain_aov$residuals
hist(residuals, main="Histogram of standardised residuals",xlab="Standardised residuals")
#check for Homogeneity
library(car)
leveneTest(time_to_relief~treatment) #Levene's test for equality of variances
#As p - value (0.04323) < 0.05, equal variances can not be assumed

#Pairwise Comparison
tukey_test <- TukeyHSD(pain_aov, conf.level=0.95)
tukey_test
par(mfrow=c(1,1)) #reset the mfrow parameter
plot(tukey_test)
