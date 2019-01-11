setwd("~/2018 Fall WPI/DS502/Final Project/0ksprojects")
library(dplyr)
library(ggplot2)
library(lubridate)
library(googledrive)
library(caTools)
library(class)
library("glmnet", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("caret", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")

df5=read.csv("TrainingKickstarter.csv")
train.size=floor(nrow(df5)*.1)
train.size
#[1] 18417
set.seed(1)
train_percent=sample(seq_len(nrow(df5)),size=train.size)
ks2=df5[train_percent,]
dim(ks2)
#[1] 18417    22

KS_percent= ks2[, c("currency",
                    "state",
                    "country",
                    "usd_goal_real",
                    "deadline.day",
                    "deadline.dow",
                    "deadline.mth",
                    "launched.min",
                    "launched.day",
                    "launched.dow",
                    "launched.mth",
                    "ks.duration",
                    "namesize")]

summary(KS_percent)
#currency            state          country      usd_goal_real      deadline.day    deadline.dow  
#USD    :14410   others    :11877   US     :14256   Min.   :0.0e+00   Min.   : 1.00   Min.   :1.000  
#GBP    : 1631   successful: 6540   GB     : 1607   1st Qu.:2.0e+03   1st Qu.: 7.00   1st Qu.:2.000  
#EUR    :  862                      CA     :  713   Median :5.2e+03   Median :15.00   Median :4.000  
#CAD    :  721                      AU     :  363   Mean   :3.9e+04   Mean   :15.26   Mean   :4.176  
#AUD    :  373                      N,0"   :  214   3rd Qu.:1.5e+04   3rd Qu.:23.00   3rd Qu.:6.000  
#SEK    :   87                      DE     :  185   Max.   :5.8e+07   Max.   :31.00   Max.   :7.000  
#(Other):  333                      (Other): 1079                                                    
#deadline.mth     launched.min    launched.day    launched.dow    launched.mth     ks.duration      
#Min.   : 1.000   Min.   : 0.00   Min.   : 1.00   Min.   :1.000   Min.   : 1.000   Min.   :    1.00  
#1st Qu.: 4.000   1st Qu.:13.00   1st Qu.: 8.00   1st Qu.:3.000   1st Qu.: 4.000   1st Qu.:   30.00  
#Median : 7.000   Median :29.00   Median :15.00   Median :4.000   Median : 6.000   Median :   30.00  
#Mean   : 6.787   Mean   :28.66   Mean   :15.25   Mean   :4.056   Mean   : 6.429   Mean   :   35.05  
#3rd Qu.:10.000   3rd Qu.:44.00   3rd Qu.:23.00   3rd Qu.:5.000   3rd Qu.: 9.000   3rd Qu.:   38.00  
#Max.   :12.000   Max.   :59.00   Max.   :31.00   Max.   :7.000   Max.   :12.000   Max.   :14749.00  

#namesize    
#Min.   : 0.00  
#1st Qu.:21.00  
#Median :34.00  
#Mean   :34.93  
#3rd Qu.:49.00  
#Max.   :96.00  

glimpse(KS_percent)
#Observations: 18,417
#Variables: 13
#$ currency      <fct> USD, USD, USD, USD, USD, CAD, USD, GBP, GBP, USD, USD, USD, USD, USD, GBP, USD, AUD, U...
#$ state         <fct> successful, successful, others, others, others, successful, others, others, successful...
#$ country       <fct> US, US, US, US, US, CA, US, GB, GB, US, US, US, US, US, GB, US, AU, US, US, US, US, GB...
#$ usd_goal_real <dbl> 5000.00, 6000.00, 8000.00, 18000.00, 75000.00, 11658.64, 4000.00, 7836.25, 756.91, 560...
#$ deadline.day  <int> 21, 29, 7, 17, 2, 15, 1, 21, 31, 25, 27, 11, 11, 13, 18, 5, 19, 18, 26, 28, 18, 30, 14...
#$ deadline.dow  <int> 5, 5, 7, 7, 6, 6, 6, 6, 7, 4, 2, 2, 2, 7, 5, 1, 6, 2, 6, 2, 2, 5, 6, 7, 6, 6, 4, 5, 3,...
#$ deadline.mth  <int> 6, 1, 4, 9, 10, 7, 8, 8, 10, 3, 2, 12, 1, 4, 9, 6, 12, 8, 9, 7, 8, 11, 11, 3, 7, 9, 7,...
#$ launched.min  <int> 58, 40, 41, 12, 59, 20, 13, 11, 51, 32, 59, 1, 51, 20, 31, 29, 13, 4, 41, 0, 31, 1, 20...
#$ launched.day  <int> 2, 15, 8, 18, 2, 15, 1, 30, 19, 23, 6, 11, 12, 14, 1, 6, 19, 21, 27, 13, 17, 1, 15, 3,...
#$ launched.dow  <int> 4, 2, 5, 5, 4, 4, 3, 5, 2, 2, 2, 7, 5, 5, 2, 6, 4, 2, 4, 6, 5, 4, 4, 5, 4, 2, 2, 3, 4,...
#$ launched.mth  <int> 5, 12, 3, 8, 9, 6, 7, 7, 10, 2, 2, 11, 11, 3, 9, 5, 11, 7, 8, 6, 7, 11, 10, 2, 6, 7, 6...
#$ ks.duration   <int> 50, 45, 29, 30, 30, 30, 31, 22, 12, 29, 21, 30, 60, 30, 17, 30, 30, 28, 30, 45, 32, 29...
#$ namesize      <int> 57, 9, 5, 24, 59, 52, 16, 18, 34, 29, 34, 19, 34, 17, 47, 28, 35, 44, 17, 14, 31, 17, ...

#Create a table to store methods and results
n=0
method.results = data.frame(method=character(n), 
predictors=character(n), 
threshold=numeric(n), 
correct.success = numeric(n))

#KNN with K chosen by cross-validation
trControl=trainControl(method="cv",number=10)
fit=train(state~.,method="knn",tuneGrid=expand.grid(k=1:10),trControl=trControl,metric="Accuracy",data=KS_percent)
fit
#k-Nearest Neighbors 

#18417 samples
#12 predictor
#2 classes: 'others', 'successful' 

#No pre-processing
#Resampling: Cross-Validated (10 fold) 
#Summary of sample sizes: 16575, 16575, 16575, 16575, 16576, 16575, ... 
#Resampling results across tuning parameters:

#k   Accuracy   Kappa     
#1  0.5824507  0.08509509
#2  0.5831021  0.08850617
#3  0.6015097  0.10112206
#4  0.6005857  0.09792894
#5  0.6139432  0.11187972
#6  0.6107941  0.10620537
#7  0.6174722  0.10613469
#8  0.6208934  0.11451139
#9  0.6229565  0.11156355
#10  0.6242059  0.11466446

#Accuracy was used to select the optimal model using the largest value.
#The final value used for the model was k = 10.
