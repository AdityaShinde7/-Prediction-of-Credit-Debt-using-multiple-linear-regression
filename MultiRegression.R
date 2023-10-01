install.packages(c("ggplot2", "ggthemes", "scales", "dplyr", "mice", "randomForest"))
setwd("C:/Users/adity/Downloads/StatsCA")
creditData <-read.csv("Credit_v2.csv", header=T, na.strings=c(""), stringsAsFactors = T)
#--------Initial remove na and rename
sapply(creditData,function(x)sum(is.na(x)))


#rename i...age as age
colnames(creditData)[1] <- "age"
str(creditData)
summary(creditData)

boxplot(credit)
plot(creditData)
pairs(creditData, panel =panel.smooth)
#Histogram and normal curve

#Histogram and normal curve

hist(creditData$creddebt, prob = TRUE, main = "Histogram with normal curve")
x <- seq(min(creditData$creddebt), max(creditData$creddebt), length = 40)
f <- dnorm(x, mean = mean(creditData$creddebt), sd = sd(creditData$creddebt))
lines(x, f, col = "red", lwd = 2)

#Hist after Log
hist(log(creditData$creddebt), prob = TRUE, main = "Histogram with normal curve")
x <- seq(min(log(creditData$creddebt)), max(log(creditData$creddebt)), length = 40)
f <- dnorm(x, mean = mean(log(creditData$creddebt)), sd = sd(log(creditData$creddebt)))
lines(x, f, col = "red", lwd = 2)

library(correlation)

correlation::correlation(creditData,
                         include_factors = TRUE, method = "auto"
)

cor.test(creditData$creddebt,creditData$age)
cor.test(creditData$creddebt,creditData$ed)
cor.test(creditData$creddebt,creditData$employ)
cor.test(creditData$creddebt,creditData$address)
cor.test(creditData$creddebt,creditData$income)
cor.test(creditData$creddebt,creditData$debtinc)
cor.test(creditData$creddebt,creditData$othdebt)
cor.test(creditData$creddebt,creditData$default)

cordata <-creditData[ , c("creddebt", "income", "debtinc", "othdebt")]   
pairs(cordata, panel =panel.smooth)
 

####################################Outlier Removal
corinc <- creditData
library(correlation)

correlation::correlation(creditData,
                         include_factors = TRUE, method = "auto"
)

outliers <- function(x) {
  
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  
  x > upper_limit | x < lower_limit
}

remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers(df[[col]]),]
  }
  df
}
remove_outliers(cordata, c("income","debtinc","creddebt","othdebt"))

pairs(cordata, panel =panel.smooth)

model <- lm(creddebt~.,data = cordata)
summary(model)
par(mfrow=c(2,2))
plot(model)


model1<-lm(log(creddebt)~.,data = cordata)
summary(model1)
par(mfrow=c(2,2))
plot(model1)


library(dplyr)
cooksD <- cooks.distance(model1)
influential <- cooksD[(cooksD > (1 * mean(cooksD, na.rm = TRUE)))]
influential
names_of_influential <- names(influential)
outliers <- cordata[names_of_influential,]
cred_without_outliers <- cordata %>% anti_join(outliers)
cred_without_outliers

model2 <-lm(log(creddebt)~.,data = cred_without_outliers)
summary(model2)
par(mfrow=c(2,2))
plot(model2)


model3 <-lm(log(creddebt)~I(sqrt(income)) +debtinc + othdebt,data = cred_without_outliers)
summary(model3)
par(mfrow=c(2,2))
plot(model3)

model4 <-lm(log(creddebt)~I(sqrt(income)) + I(sqrt(debtinc)) + othdebt,data = cred_without_outliers)
summary(model4)
par(mfrow=c(2,2))
plot(model4)
########################DONE Run All till model 4 below#######################################################################
library(performance)
library(see)
library(patchwork)
check_model(model4)
 
library(car)
ncvTest(model4)
durbinWatsonTest(model4)
vif(model4)
#*******************************DONE*******************************************************************#
cordata <-creditData[ , c("creddebt", "income", "debtinc", "othdebt","employ")]
library(correlation)

correlation::correlation(creditData,
                         include_factors = TRUE, method = "auto"
)

outliers <- function(x) {
  
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  
  x > upper_limit | x < lower_limit
}

remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers(df[[col]]),]
  }
  df
}
remove_outliers(cordata, c("income","debtinc","creddebt","othdebt","employ"))

model1 <- lm(creddebt~.,data = cordata)
summary(model1)
par(mfrow=c(2,2))
plot(model1)

model2<-lm(log(creddebt)~.,data = cordata)
summary(model2)
par(mfrow=c(2,2))
plot(model2)

library(dplyr)
cooksD <- cooks.distance(model2)
influential <- cooksD[(cooksD > (1 * mean(cooksD, na.rm = TRUE)))]
influential
names_of_influential <- names(influential)
outliers <- cordata[names_of_influential,]
cred_without_outliers <- cordata %>% anti_join(outliers)
cred_without_outliers

model3 <-lm(log(creddebt)~.,data = cred_without_outliers)
summary(model3)
par(mfrow=c(2,2))
plot(model3)

model31 <-lm(log(creddebt)~I(sqrt(income)) + debtinc + othdebt +employ,data = cred_without_outliers)
summary(model31)
par(mfrow=c(2,2))
plot(model31)

model32 <-lm(log(creddebt)~I(sqrt(income)) + I(sqrt(debtinc)) + othdebt,data = cred_without_outliers)
summary(model32)
par(mfrow=c(2,2))
plot(model32)

model5 <-lm(log(creddebt)~I(sqrt(income)) + I(sqrt(debtinc)) + othdebt,data = cred_without_outliers)
summary(model5)
par(mfrow=c(2,2))
plot(model5)
########################DONE#######################################################################
library(performance)
library(see)
library(patchwork)
check_model(model5)

library(car)
ncvTest(model5)
durbinWatsonTest(model5)
vif(model5)

anova(model12,model4)

######################################################################
model5 <-lm(log(creddebt)~I(sqrt(income)) + I(sqrt(debtinc)) + I(sqrt(othdebt)),data = cred_without_outliers)
summary(model5)
par(mfrow=c(2,2))
plot(model5)

library(car)
#ncv for homoscedasticity
ncvTest(model5)
durbinWatsonTest(model5)
vif(model5)

anova(model4,model5)







model5 <-lm(log(creddebt)~I(sqrt(income)) +debtinc +employ+othdebt+ employ ,data = cred_without_outliers)
summary(model5)
par(mfrow=c(2,2))
plot(model5)
library(performance)
library(see)
library(patchwork)
check_model(model5)

library(car)
ncvTest(model5)


anova(model3,model4,model5)
