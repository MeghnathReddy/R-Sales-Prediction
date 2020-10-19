#Required libraries
require(mlbench) #package to get glass dataset
library(tidyverse) #To install and load multiple tidyverse packages
require(rgl) #3D visualization device package
require(ggplot2)
require(MASS) #TO load LDA
require(Rtsne) #FOR TSNE PACKAGE
require(umap) #FOR UMAP PACKAGE
require(factoextra) #To draw bar plot of variable contribution
library(psych)
require(mice)
require(ggpubr) #for normality
require(funModeling)
require(pastecs)
require(VIM)
library(outliers)           #load "outliers" package to 'test' for outliers
require(dplyr)
require(lubridate)
require(forcats)
library(EnvStats)
library(tidyverse)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Part-----a)EDA
sales<-read.csv("Train.csv")
sales<-data.frame(sales)
summary(sales)
describe(sales)
numeric_sales<-sales[c(6,7,32,35)] #Numeric data
status(numeric_sales)
freq(sales$browser)
plot_num(numeric_sales)

#mutate sessionId and custId so that they are character instead of numeric
dfTrain <- sales %>% mutate_at(vars(ends_with("Id")), as.character) 
glimpse(dfTrain)

#sample the data to reduce number of observations to deal with at first...
dfSample <- dfTrain %>% sample_frac(0.1)
glimpse(dfSample)

#change some options on formatting numbers
options(scipen=100)
options(digits=2)

dfSample %>% select_if(is.numeric) %>% 
  stat.desc()

dfSample %>% select_if(is.numeric) %>%gather() %>% head(10)
dfSample %>% select_if(is.numeric) %>% 
  gather() %>%
  ggplot(aes(value)) + 
  facet_wrap(~ key, scales = "free") + 
  geom_histogram()  

#Different plots to explore data
ggplot(dfSample, aes(x=channelGrouping, y=revenue)) + 
  geom_point(size=2, shape=20)+labs(title="Referral seems to generate more revenue")

ggplot(dfSample, aes(x=browser, y=revenue)) + 
  geom_point(size=2, shape=20)+labs(title="Basic scatter plot Browser type vs Revenue")

ggplot(dfSample, aes(x = pageviews, y = deviceCategory, color =revenue )) + geom_point()+
  labs(title="Revenue as Continuous color variable") #color = revenue which is continuous

ggplot(dfSample, aes(x = pageviews, y = revenue, color =isMobile )) + geom_point()+
  labs(title="Mobile category as categorical variable") #color = isMobile which is categorical

ggplot(data = dfSample) + 
  geom_point(mapping = aes(x = pageviews, y = revenue)) + 
  facet_wrap(~ continent, nrow = 2)+labs(title="Facet grid") #Facet grid

pairs(numeric_sales[,1:4])
barchart(dfSample$continent,main="Customers by continent",xlab="Customers by region",ylab="Frequency")
barchart(dfSample$browser,main="Customers user platform",xlab="Platform used",ylab="Frequency")
hist(dfSample$pageviews,main="Frequency of Page views",xlab="Number of views")

#density plot
ggplot(dfSample, aes(x=deviceCategory,fill=continent)) + 
  geom_density(alpha=0.3)+labs(title="Advanced density plot")

#stat_bin
ggplot(dfSample,aes(x=pageviews))+stat_bin()+
  scale_x_continuous(breaks=c(100,200,300,400,500)) + labs(title="To give count based on Quality")

#stat_summary
ggplot(dfSample,aes(x=pageviews,y=revenue))+stat_summary()+
  scale_x_continuous(breaks=c(100,200,300,400,500))+
  theme_dark(base_family = "serif")+labs(title="Summary and theme")

#scale_shape_manual
ggplot(dfSample,aes(x=pageviews,y=continent,shape=deviceCategory))+
  geom_point()+scale_shape_manual(values=c(5,3,8,2))+
  scale_color_brewer(type="seq")+scale_x_continuous(breaks=c(100,200,300,400,500))+
  labs(title="Views vs Continent based on device category")

#Partb-------------Data preparation
#MISSING VALUE Exploration
missing_values<-sales[,c(31,27,34,33)]
md.pattern(missing_values)

vim_patter<-aggr(sales)
vim_patter<-aggr(missing_values)

marginplot(sales[c("adwordsClickInfo.isVideoAd","adwordsClickInfo.page")], 
           col = c("blue", "red", "orange"))

# can also look at all of the plots with Missing Information
scattmatrixMiss(missing_values)

#Outlier exploration
plot(sales$revenue)
identify(sales$revenue)

plot(sales$visitNumber)
identify(sales$visitNumber)

grubbs.test(sales$revenue)  #univariate test for 'Revenue' outliers
grubbs.test(sales$visitNumber)   #univariate test for 'VisitNumber' outliers

outlier(sales$revenue)      #what is the most extreme value for Revenue?
outlier(sales$visitNumber)       #what is the most extreme value for VisitNumber?

sales[sales$revenue==outlier(sales$revenue),]   #which records identified?
sales[sales$visitNumber==outlier(sales$visitNumber),]


outler_removed<-sales[sales$revenue!=outlier(sales$revenue),]  

#pick the four furthest points from the line..
plot(dfSample$revenue)
plot(dfSample$pageviews)
v<-identify(dfSample$revenue,dfSample$pageviews)   
dfSample[v,]

dfSample<-dfSample[-v,]                  # and delete if you want...                        

plot(sales$revenue)                                      # plot what ever is left, and 
abline(lm(dfSample$revenue ~ dfSample$pageviews))           #the new model looks like this

#now please identify the most extreme bivariate data point in the resulting plot
v<-identify(dfSample$revenue,dfSample$pageviews) 
dfSample[v,]

#dimesnion reduction using PCA
unique_rows<-numeric_sales
unique_rows<-unique_rows[!duplicated(sales),] #find and remove the duplicate row
my_num_data <- unique_rows[, sapply(unique_rows, is.numeric)] #remove non-numeric
corMat<-cor(my_num_data,use = "complete.obs") #correlational matrix
corMat
#Eigen vectors and eigen values
eigen_vector<-eigen(corMat)
eigen_vector
eigen_values<-eigen_vector$values
eigen_values

Sample.scaled.2 <- data.frame(t(na.omit(t(numeric_sales))))
pca.Sample.2 <- prcomp(Sample.scaled.2, retx=TRUE)
summary(pca.Sample.2)

plot((pca.Sample.2),type='l')
biplot(pca.Sample.2,group_by=numeric_sales$Type,color=numeric_sales$Type,scale = 0)

#find out the missing value of newVisit,pageviews and transform
train <- read.csv("Train.csv", stringsAsFactors = TRUE)
summary(train$newVisits)
train[is.na(train$newVisits),"newVisits"] <- 0
summary(train$isTrueDirect)
train[is.na(train$isTrueDirect),"isTrueDirect"] <- 0
summary(train$pageviews)
train[is.na(train$pageviews),"pageviews"] <- 0
summary(train$bounces)
train[is.na(train$bounces),"bounces"] <- 0

#test data for MARS
testCust<-read.csv("Test.csv",stringsAsFactors = TRUE)

#find out the missing value of newVisit,pageviews and transform
summary(testCust$newVisits)
testCust[is.na(testCust$newVisits),"newVisits"] <- 0
summary(testCust$isTrueDirect)
testCust[is.na(testCust$isTrueDirect),"isTrueDirect"] <- 0
summary(testCust$pageviews)
testCust[is.na(testCust$pageviews),"pageviews"] <- 0
summary(testCust$bounces)
testCust[is.na(testCust$bounces),"bounces"] <- 0

#Partd D--------------Modeling
trainmd <- train %>% group_by(custId) %>%
  summarise(sumRevenue = sum(revenue),
            sumViews = sum(pageviews),
            newVisits = max(newVisits),
            sumDirect = sum(isTrueDirect),
            sumBounce = sum(bounces),
            groupContinent = continent)

testmd <- testCust %>% group_by(custId) %>%
  summarise(
    sumViews = sum(pageviews),
    newVisits = max(newVisits),
    sumDirect = sum(isTrueDirect),
    sumBounce = sum(bounces),
    groupContinent = continent)
testmd

#Linear Model
modeltrain <- lm(data = trainmd, log(sumRevenue+1) ~ sumViews 
                 + newVisits*sumDirect*sumBounce + groupContinent)
summary(modeltrain)

#MARS Model
library(earth)
marstrain <- earth(log(sumRevenue+1)~sumViews+newVisits*sumDirect*sumBounce+
                     groupContinent,data=trainmd,degree=5)

summary(marstrain)

#Predict Results and store the results in csv file
pred<-predict(marstrain,testmd)
submissionDf<-data.frame(testmd$custId, pred_revenue=pred)
unique(submissionDf$testmd.custId)
write.csv(submissionDf, "submission1.csv", row.names=FALSE)
#duplicate values are removed directly in Excel. Go to Data tab>Data tools>
#Remove Duplicates and select column custId

# Function for Root Mean Squared Error
RMSE <- function(error) { sqrt(mean(error^2)) }
RMSE(lmmodeltrain$residuals)

#Linear model with no intercept
lmmodel_nointercept <- lm(data = trainmd, log(sumRevenue+1) ~ -1+sumViews
                          + newVisits*sumDirect*sumBounce + groupContinent)
summary(lmmodel_nointercept)
RMSE(lmmodel_nointercept$residuals)



#PLS MODEL
library(pls)
library(caret)
set.seed(123)
model <- train(
  sumRevenue~., data = trainmd, method = "pls",
  scale = TRUE,
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)

# minimize the cross-validation error, RMSE
model$bestTune
# Summarize the final model
summary(model$finalModel)
model.r.sqaured

# Make predictions
predictions <- model %>% predict(testmd)

# Function for Root Mean Squared Error
RMSE <- function(error) { sqrt(mean(error^2)) }
RMSE(predictions$residuals)





