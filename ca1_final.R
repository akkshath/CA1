#removing all the previously stored variables
rm(list=ls())

heart_data <- read.csv("heart.csv")
heart_data
----------------------------------------------------------------------------------------------------------------------------------
#exploration of the data
library(Hmisc)#importing library
describe(heart_data) #Hmisc command

# Display the first six entries from the DF
head(heart_data)

# Structure of DF
str(heart_data)

# Verifying that it is a DF
class(heart_data)

----------------------------------------------------------------------------------------------------------------------------------
  
# To check if any NA data present 
any(is.na(heart_data))
#clearing out NA values
----------------------------------------------------------------------------------------------------------------------------------
  
#changing column names

names(heart_data)[1] <- "Age"
names(heart_data)[2] <- "Sex"
names(heart_data)[3] <- "Chest_Pain"
names(heart_data)[4] <- "Blood_pressure"
names(heart_data)[5] <- "Cholestoral"
names(heart_data)[6] <- "Fasting_blood_sugar"
names(heart_data)[7] <- "Resting_ECG"
names(heart_data)[8] <- "MAX_Heartrate"
names(heart_data)[9] <- "Angina_excercise"
names(heart_data)[10] <- "oldpeak"
names(heart_data)[11] <- "slp"
names(heart_data)[12] <- "Number_Majorvessel"
names(heart_data)[13] <- "thall"
names(heart_data)[14] <- "Result"
str(heart_data)

----------------------------------------------------------------------------------------------------------------------------------
  
#taking required columns
install.packages("dplyr")
library(dplyr)
heart_data
new_heartdata = subset(heart_data, select = c(1:9,12,14))
new_heartdata  

----------------------------------------------------------------------------------------------------------------------------------
#question number one
#does high levels of cholesterol has an increased chance of getting heart attack?

#H0 the levels of cholesterol has no effect on heart attack
#H1 the levels of cholesterol has an effect on heart attack
  
#the variables used are "Cholesterol" and "Result"
#Factorizing Result into two cases less chance and more chance
new_heartdata$`Result` <- factor(new_heartdata$`Result`, labels = c("Less Chance", "More Chance"))
str(new_heartdata)

attach(new_heartdata)

# We can split the dichotomous variable into 2 & then examine the data
library("lattice")

histogram(~Cholestoral | Result,
          data = new_heartdata,
          main = "Distribution of levels of cholestrol with 
          occurance of Heart Attack",
          xlab = "cholestrol levels",
          ylab = "occurance of Heart Attack")

# Visual analysis seems to indicate that the data is not normally distributed

#Summarizing the medians to confirm that it is not normally distributed

tapply(Cholestoral, Result, median)
# 0   1 
#249 234 

# Let's confirm the distribution as the point for less chance is one and more chance is zero 

# Is Cholesterol normally distributed?

qqnorm(Cholestoral)
qqline(Cholestoral, col = "blue")

# Cholesterol appears not to be normally distributed

# Is Result normally distributed? 
with(new_heartdata, qqplot(Cholestoral[Result == "More of a Chance"],
                        Cholestoral[Result == "Less of a Chance"],
                        main = "Comparing Two Samples of Heart Attack Data",
                        xlab = "Less of a Chance",
                        ylab = "More of a Chance"))

# We can add normality line to the plot
# to help evalute normality 
with(new_heartdata, {
  qqnorm(Cholestoral[Result == "Less of a Chance"],
         main = "Less of a Chance")
  qqline(Cholestoral[Result == "Less of a Chance"])})
# This reveals that the 'Less of a Chance' side of the 'Result' 
# variable, is normally distributed


with(new_heartdata, {
  qqnorm(Cholestoral[Result == "More of a Chance"],
         main = "More of a Chance")
  qqline(Cholestoral[Result == "More of a Chance"])})
# This reveals that the 'More of a Chance' side of the 'Result' 
# variable, is not normally distributed

# Shapiro-Wilks test

normality_test <- shapiro.test(new_heartdata$Cholestoral)
normality_test$p.value
# p-value = 5.364848e-09 (0.000000005364848)
# This is less than 0.05, so it is not normally distributed

# This test does not work on dicotomous variable
with(new_heartdata, tapply(Cholestoral, Result, shapiro.test))
# p-value for 'Less of a Chance' is 0.3792 
#This is greater than 0.05, so it is normally distributed

# p-value for 'More of a Chance' is 3.079e-09 (0.000000003079)
# This is less than 0.05, so it is not normally distributed

# Therefore Result show that both variables, 'Cholestoral'
# and 'Result', are normally distributed.

#less of a chance is true

wilcox.test(Cholestoral~Result)
# cut-off = 0.05
# p-value = 0.03572
# p-value < 0.05 so this indicates the Null (H0) hypothesis is accepted. 
# Therefore, the number of Cholestoral levels has less chance of having a heart attack.(p-value = 0.03572)

----------------------------------------------------------------------------------------------------------------------------------
#question number two

#Does fasting sugar levels has an increased chance of causing a heart attack?
  
  #H0 the levels of fasting sugar levels has no effect on heart attack
  #H1 the levels of fasting sugar levels has an effect on heart attack
  
  #the variables used are "Fasting_blood_sugar" and "Result"
  
#Factorizing Result into two cases less chance and more chance
new_heartdata$`Result` <- factor(new_heartdata$`Fasting_blood_sugar`, labels = c("no effect", "has effect"))
str(new_heartdata)

attach(new_heartdata)

#We can split the dichotomous variable into 2 & then examine the data
library("lattice")

histogram(~Fasting_blood_sugar | Result,
          data = new_heartdata,
          main = "Distribution of levels of Fasting_blood_sugar with 
          occurance of Heart Attack",
          xlab = "Fasting_blood_sugar levels",
          ylab = "occurance of Heart Attack")
# Visual analysis seems to indicate that the data is normally distributed

# #Summarizing the medians to confirm that it is normally distributed
tapply(Fasting_blood_sugar, Result, median)
#0 1 
#0 0 
# Let's confirm the distribution

# Is fasting blood sugar normally distributed?
qqnorm(Fasting_blood_sugar)

# Add line that represents normal distribution
qqline(Fasting_blood_sugar, col = "red")
# fasting blood sugar appears not to be normally distributed


# Is Result normally distributed? 
with(new_heartdata, qqplot(Fasting_blood_sugar[Result == "has effect"],
                           Fasting_blood_sugar[Result == "no effect"],
                           main = "Fasting blood sugars has any impact on heart attack",
                           xlab = "no effect",
                           ylab = "has effect"))

# We can add normality line to the plot
# to help evalute normality 
with(new_heartdata, {
  qqnorm(Fasting_blood_sugar[Result == "no effect"],
         main = "no effect")
  qqline(Fasting_blood_sugar[Result == "no effect"])})
# This reveals that the 'no effect' side of the 'Result' 
# variable, is normally distributed


with(new_heartdata, {
  qqnorm(Fasting_blood_sugar[Result == "has effect"],
         main = "has effect")
  qqline(Fasting_blood_sugar[Result == "has effect"])})
# This reveals that the 'has effect' side of the 'Result' 
# variable, is not normally distributed

# Formal test of normality
# Shapiro-Wilks test
# P value tells us the chances that the sample comes from a normal distribution
# If p > 0.05 then, normally dist.
normality_test <- shapiro.test(new_heartdata$Fasting_blood_sugar)
normality_test$p.value
# p-value = 5.43089e-30 (0.00000000543089)
# This is less than 0.05, so it is normally distributed

# This test does not work on dicotomous variable
with(new_heartdata, tapply(Fasting_blood_sugar, Result, shapiro.test))
# p-value for 'Less of a Chance' is 2.2e-16
#This is greater than 0.05, so it is not normally distributed

# p-value for 'More of a Chance' is 2.2e-16
# This is less than 0.05, so it is not normally distributed

# Therefore Result show that both variables, 
# and 'Result', are not normally distributed.

#less of a chance is true

kruskal.test(Fasting_blood_sugar ~ Result, data = new_heartdata)
pairwise.wilcox.test(new_heartdata$Fasting_blood_sugar, new_heartdata$Result, p.adjust.method = "BH")

# cut-off = 0.05
# p-value = 0.626 
# p-value > 0.05 then we, accept the H0

# p-value > 0.05 so this indicates that the
# Null (H0) hypothesis is accepted


----------------------------------------------------------------------------------------------------------------------------------
#question number three
  
  # Are males more likely to experience heart
  # attacks than females?
  
  # H0: Likeliness of a heart attack does not differ by gender
  # H1: Likeliness of a heart attack does differ by gender
  
  # The variables that will be used for analysis are 'Sex' and 'Result'.
  

#Factorizing sex into two cases male or female

new_heartdata$`Result` <- factor(new_heartdata$`Sex`, labels = c("no effect", "has effect"))

str(new_heartdata)


attach(new_heartdata)
plot(Sex, 
     Result, 
     pch = "19", 
     col = "red", 
     main = "Comparison of Sex with occurance of Heart Attack",
     xlab = "Sex", 
     ylab = "occurance of Heart Attack")

# This does not tell much because the two variables are both categorical
# dichotomous so they aren't going to show a trend of linearity

# Instead, we can split the dichotomous variable into 2 & then examine the data
library("lattice")

histogram(~Sex| Result,
          data = new_heartdata,
          main = "Comparison of Sex with 
          occurance of Heart Attack",
          xlab = "Sex",
          ylab = "occurance of Heart Attack")
# Visual analysis seems to indicate that the data is skewed
# Summarise the medians of the data to confirm that it is not normally distributed.
tapply(Sex, Result, median)
#has least Chance  has more Chance 
#       55               57 
# They seem to be off, the center point for both sides of the variable are different.
# The center point for 'Less of a Chance' is 1 
# Where the center point for 'More of a Chance' is 0
# Let's confirm the distribution

# Is sex normally distributed?
qqnorm(Sex)
# Add line that represents normal distribution
qqline(Sex, col = "red")
# sex appears not to be normally distributed


# Is Result normally distributed? 
with(new_heartdata, qqplot(age[Result == "has more Chance"],
                           age[Result == "has least Chance"],
                           main = "Comparing Two Samples of Heart Attack Data",
                           xlab = "has least Chance",
                           ylab = "has more Chance"))

# We can add normality line to the plot
# to help evalute normality 
with(new_heartdata, {
  qqnorm(Sex[Result == "has least Chance"],
         main = "has least Chancee")
  qqline(Sex[Result == "has least Chance"])})
# This reveals that the 'Less of a Chance' side of the 'Result' 
# variable, is normally distributed


with(new_heartdata, {
  qqnorm(Sex[Result == "has more Chance"],
         main = "has more Chance")
  qqline(Sex[Result == "has more Chance"])})
# This reveals that the 'has more Chance' side of the 'Result' 
# variable, is not normally distributed

# Formal test of normality
# Shapiro-Wilks test
# P value tells us the chances that the sample comes from a normal distribution
# If p > 0.05 then, normally dist.
normality_test <- shapiro.test(new_heartdata$Sex)
normality_test$p.value
# p-value =  2.750712e-26 which is 
# This is less than 0.05, so it is not normally distributed

# This test does not work on dicotomous variable
with(new_heartdata, tapply(Sex, Result, shapiro.test))
# p-value for 'Less of a Chance' is 2.2e-16
#This is greater than 0.05, so it is not normally distributed
# p-value for 'More of a Chance' is 2.2e-16
# This is less than 0.05, so it is not normally distributed

# Therefore Result show that both variables, 'sex'
# and 'Result', are normally distributed.

#less of a chance is true
wilcox.test(Sex~Result)
# cut-off = 0.05
# p-value = 0.000001054
# p-value > 0.05 so this indicates the Null (H0) hypothesis is accepted. 
#Therefore, heart attack is unbised of sex and has chances of having a heart attack is more.(p-value = 0.000001054).
detach(new_heartdata)
----------------------------------------------------------------------------------------------------------------------------------
#question number four
#Is age a factor in causing heart attack?
  
  #H0 age of a person has no effect on heart attack
  #H1 age of a person has an effect on heart attack
  
  #the variables used are "age" and "Result"
  
#factorizing results with has least chance and more chance cases
new_heartdata$`Result` <- factor(new_heartdata$`Result`, labels = c("has least Chance", "has more Chance"))
str(new_heartdata)


attach(new_heartdata)
plot(age, 
     Result, 
     pch = "19", 
     col = "red", 
     main = "Comparison of Number of Major Vessels with occurance of Heart Attack",
     xlab = "Number of Major Vessels", 
     ylab = "occurance of Heart Attack")
# This does not tell much because the two variables are different types


# Instead, we can split the dichotomous variable into 2 & then examine the data
library("lattice")

histogram(~age | Result,
          data = new_heartdata,
          main = "Age with 
          occurance of Heart Attack",
          xlab = "age",
          ylab = "occurance of Heart Attack")
# Visual analysis seems to indicate that the data is skewed to the right
# and not normally distributed

# Summarise the medians of the data to confirm that it is not normally distributed
tapply(age, Result, median)
#has least Chance  has more Chance 
#58               52
# They seem to be off, the center point for both sides of the variable are different.
# The center point for 'Less of a Chance' is 1 
# Where the center point for 'More of a Chance' is 0
# Let's confirm the distribution

# Is Cholestoral normally distributed?
qqnorm(age)
# Add line that represents normal distribution
qqline(age, col = "red")
# Cholestoral appears not to be normally distributed


# Is Result normally distributed? 
with(new_heartdata, qqplot(age[Result == "has more Chance"],
                           age[Result == "has least Chance"],
                           main = "Comparing Two Samples of Heart Attack Data",
                           xlab = "has least Chance",
                           ylab = "has more Chance"))

# We can add normality line to the plot
# to help evalute normality 
with(new_heartdata, {
  qqnorm(age[Result == "has least Chance"],
         main = "has least Chancee")
  qqline(age[Result == "has least Chance"])})
# This reveals that the 'Less of a Chance' side of the 'Result' 
# variable, is normally distributed


with(new_heartdata, {
  qqnorm(age[Result == "has more Chance"],
         main = "has more Chance")
  qqline(age[Result == "has more Chance"])})
# This reveals that the 'has more Chance' side of the 'Result' 
# variable, is not normally distributed

# Formal test of normality
# Shapiro-Wilks test
# P value tells us the chances that the sample comes from a normal distribution
# If p > 0.05 then, normally dist.
normality_test <- shapiro.test(new_heartdata$age)
normality_test$p.value
# p-value =  0.005798359
# This is less than 0.05, so it is not normally distributed

# This test does not work on dicotomous variable
with(new_heartdata, tapply(age, Result, shapiro.test))
# p-value for 'Less of a Chance' is 0.002868
#This is greater than 0.05, so it is normally distributed
# p-value for 'More of a Chance' is 0.1211
# This is less than 0.05, so it is not normally distributed

# Therefore Result show that both variables, 'age'
# and 'Result', are normally distributed.

#less of a chance is true
wilcox.test(age~Result)
# cut-off = 0.05
# p-value = 0.04772
# p-value > 0.05 so this indicates the Null (H0) hypothesis is
# accepted. Therefore, the age number has less chance of
# having a heart attack.(p-value = 3.439e-05)

----------------------------------------------------------------------------------------------------------------------------------
#question number five

#Do blood pressure have an affect on heart rate on a person
#H0: Does blood pressure affects heart rate
#H1: Does blood pressure not affects heart rate
  
# Plot the graph to analyze the specified attributes
plot(Blood_pressure, MAX_Heartrate, pch = 19, col ="lightblue", 
       main = "Comaprison of blood pressure with heart rate", 
       xlab = "Blood Pressure (mm Hg)", ylab = "Heart Rate")


# Visualizing the variables separately using hist function
# for blood pressure
hist(Blood_pressure, col = "red", main = "Distributation of blood pressure", 
     xlab = "Blood Pressure (mm Hg)")
# for heart rate
hist(MAX_Heartrate, col = "red", main = "Distribution of heart rate", 
     xlab = "Heart Rate")

# Visual analysis of the data
histogram(~Blood_pressure | MAX_Heartrate,
          data = cars,
          main = "Distributation of blood pressure v/s heart rate",
          xlab = "Blood Pressure (mm Hg)" ,ylab = "Heart Rate")


# Testing the linearity of the variables
# We can run the formal test of normality provided through 
# the widely used shapiro-wilks test
normality_test <- shapiro.test(new_heartdata$Blood_pressure)
normality_test$p.value
# For this variable p-value = 1.458097e-06

normality_test <- shapiro.test(new_heartdata$MAX_Heartrate)
normality_test$p.value
# For this variable p-value = 6.620819e-05

# p-values tells us the chance that the sample comes from ND
# If p-value < 0.05 then the variable is not ND
# If p-value < 0.05 then the variable is ND
shapiro.test(new_heartdata$Blood_pressure)
# 0.00014 < 0.05
# Then resting_bs is not ND
shapiro.test(new_heartdata$MAX_Heartrate)
# 0.00066 < 0.05
# Then Max_Heart_Rate is not ND

# If both the variables are not ND 
# will use "spearman" correlation method test
# dependent var = Heart Rate
# independent var = Resting Blood Pressure

wilcox.test(Blood_pressure~MAX_Heartrate)
# cut-off = 0.05
# p-value = 0.04772
# p-value > 0.05 so this indicates the Null (H0) hypothesis is
# accepted. Therefore, the age number has less chance of
# having a heart attack.(p-value = 3.439e-05)
----------------------------------------------------------------------------------------------------------------------------------
  
write.csv(new_heartdata, file = "new_heartdata.csv")
