setwd("~/DS502/Project")

# Load and preprocess data
raw_train_data = read.csv("TrainingKickstarter.csv")
raw_val_data = read.csv("validationKickstarter.csv")
dim(raw_train_data)
dim(raw_val_data)
# x = sample(nrow(raw_train_data), 5000)
# raw_data = raw_train_data[x, ]
# dim(raw_data)
# names(raw_data)
# summary(raw_data)
# state = as.factor(raw_data$cat)
# raw_data1 = raw_data[, -c(1, 2, 5, 22)]
# d = data.frame(raw_data[, -c(1, 2, 10, 22)], state)
da_train = raw_train_data[, c(21, 8, 9, 5, 3, 20, 14, 13, 11, 19, 18, 16, 15)]
da_val = raw_val_data[, c(21, 8, 9, 5, 3, 20, 14, 13, 11, 19, 18, 16, 15)]
names(da_train) == names(da_val)
names(da_train)
# # split some data as test dataset
# set.seed(0)
# train = sample(nrow(da), 0.7*nrow(da))
# test = (-train)
# t = da[test, ]
# d = da[train, ]





# Tree  
# There is no big difference between pruned and unpruned tree
library(tree)
# try with all the attributes
tree_fit = tree(state~., data=da_train)
# plot unpruned tree
plot(tree_fit)
text(tree_fit, pretty=0)
summary(tree_fit)
pred_tree_unpruned = predict(tree_fit, newdata=da_val, type="class")
table(pred_tree_unpruned, da_val$state)
1 - mean(pred_tree_unpruned == da_val$state)
# use cross-validatin to choose how to prune tree
set.seed(0)
cv_tree = cv.tree(tree_fit, FUN=prune.misclass)
cv_tree
# According to cross validation, best tree is unpruned tree
table(pred_tree_unpruned, da_val$state)
1 - mean(pred_tree_unpruned == da_val$state)

# # plot size and dev relation
# plot(cv_tree$size, cv_tree$dev, type="b", xlab="Tree Size", ylab="Deviance")
# best_tree = prune.misclass(tree_fit, best=10)
# summary(best_tree)
# # plot best tree
# plot(best_tree)
# text(best_tree, pretty=0)

# # test
# pred_tree = predict(tree_fit, newdata=da_val, type="class")
# table(pred_tree, da_val$state)
# 1 - mean(pred_tree == da_val$state)


# Bagging
library(randomForest)
set.seed(0)
bag_fit = randomForest(state~., data=da_train, mtry=12, importance=TRUE, ntree=20)
bag_fit
# importance of each variable
importance(bag_fit)
# plot importance
varImpPlot(bag_fit)
# test
pred_bag = predict(bag_fit, newdata=da_val)
table(pred_bag, da_val$state)
1 - mean(pred_bag == da_val$state)



# Random Forest
set.seed(0)
rf_fit = randomForest(state~., data=da_train, mtry=3, importance=TRUE, ntree=20)
rf_fit
# importance of each variable
importance(rf_fit)
# plot importance
varImpPlot(rf_fit)
pred_rf = predict(rf_fit, newdata=da_val)
table(pred_rf, da_val$state)
1- mean(pred_rf == da_val$state)



# # Boosting does not work so far
# # Try it later
# library(gbm)
# set.seed(0)
# boost_fit = gbm(state~., data=da_train, distribution="bernoulli", n.trees=20, interaction.depth=5,
#                 shrinkage=0.05, verbose=FALSE)
# summary(boost_fit)
# boost_pred = predict(boost_fit, newdata=da_val, n.trees=20)
# table(boost_pred, da_val$state)



# k(10)-fold validation code example
# folds = cut(seq(1:nrow(d)), breaks=10, labels=FALSE)
# for(i in 1:10){
#        testIndexes = which(folds==i, arr.ind=TRUE)
#        testData = d[testIndexes, ]
#        trainData = d[-testIndexes, ]
#        ...
# }



# For logistic regression LDA, QDA, KNN, SVM, encode "main_category".


# One-hot encode "main_category"
da_oh_train = da_train
da_oh_val = da_val
for(cat in unique(da_oh_train$main_category)){
  da_oh_train[paste("cat", cat, sep=".")] = ifelse(da_oh_train$main_category == cat, 1, 0)
  da_oh_val[paste("cat", cat, sep=".")] = ifelse(da_oh_val$main_category == cat, 1, 0)
}
da_oh_train = da_oh_train[, -5]
da_oh_val = da_oh_val[, -5]
names(da_oh_train)


# 10-fold cross validaiton
folds = cut(seq(1:nrow(da_train)), breaks=10, labels=FALSE)



# Logistic Regression
# cor(da_oh_train[, -5])
# fold = sample(1:10, nrow(da_oh), replace=TRUE)
contrasts(da_oh_train$state)
# Use 10-fold cross validation to select threshold
# Three values in log_cv[i, j] are accuracy, accuracy_successful, accuracy_other
log_cv = array(NA, c(10, 9, 3))
for(i in 1:10){
  testIndexes = which(folds==i, arr.ind=TRUE)
  testData = da_oh_train[testIndexes, ]
  trainData = da_oh_train[-testIndexes, ]
  log_fit = glm(state~., family=binomial, data=trainData)
  prob_log = predict(log_fit, newdata=testData, type="response")
  for(j in 1:9){
    pred_log = ifelse(prob_log>=j/10, "successful", "other")
    tab = table(pred_log, testData$state)
    if(j==2){
      print(tab)
    }
    log_cv[i, j, 1] = mean(pred_log == testData$state)
    log_cv[i, j, 2] = tab[2, 2]/(tab[2, 2] + tab[2, 1])
    log_cv[i, j, 3] = tab[1, 1]/(tab[1, 1] + tab[1, 2])
  }
  # pred_log = ifelse(prob_log>=0.5, "successful", "other")
  # tab = table(pred_log, testData[, 5])
  # log_cv[[i]] = tab
}
log_cv_1 = matrix(0, nrow=9, ncol=3, dimnames=list(seq(0.1, 0.9, 0.1), c("Accuracy", "Acc_success", "Acc_other")))
for(i in 1:10){
  for(j in 1:9){
    log_cv_1[j, ] = log_cv_1[j, ] + log_cv[i, j, ]
  }
}
log_cv_1 = log_cv_1/10
log_cv_1
# How to decide threshold according these three plots???
par(mfrow=c(1, 3))
plot(log_cv_1[, 1], type="b", xlab="threshold * 10", ylab="Accuracy")
plot(log_cv_1[, 2], type="b", xlab="threshold * 10", ylab="Accuracy of Success")
plot(log_cv_1[, 3], type="b", xlab="threshold * 10", ylab="Accuracy of Other")


# Use selected threshold to train and test model
# according to the accuracy of success, I select threshold=0.7
log_best_fit = glm(state~., family=binomial, data=da_oh_train)

# predict on test data
prob_log = predict(log_best_fit, newdata=da_oh_test, type="response")
pred_log = ifelse(prob_log>=0.7, "successful", "others")
con_matrix = table(pred_log, da_oh_test$state)
con_matrix

#pred_log       others    successful
#others          72185      39714
#successful         256           604

# accuracy of success
print(con_matrix[2, 2]/(con_matrix[2, 2] + con_matrix[2, 1]))

#[1] 0.7023256



# PCA
pca_train = prcomp(da_oh_train[, -4], center=TRUE, scale=TRUE)
summary(pca_train)
# plot PVE curve
par(mfrow=c(2, 1))
plot(summary(pca_train)$importance[2, ], type="o", ylab="PVE", xlab="Principal Component", col =" blue ")
plot(summary(pca_train)$importance[3, ], type="o", ylab=" Cumulative PVE", xlab="Principal Component", col =" blue ")
# plot data after PCA
par(mfrow=c(1, 1))
plot(pca_train$x[, 1:2], col=as.numeric(da_oh_train$state)+1)

# According PVE plot, select 5 components
da_pca_train = data.frame(pca_train$x[, 1:5], da_oh_train$state)
names(da_pca_train)[6] = "state"
# v = predict(pca_train, da_oh_val[, -5])
# summary(v)
da_pca_val = data.frame(predict(pca_train, da_oh_val[, -4])[, 1:5], da_oh_val$state)
names(da_pca_val)[6] = "state"



# LDA and QDA 
# Try to use PCA to solve collinear problem
library(MASS)
lda_fit = lda(state~., data=da_pca_train)
pred_lda = predict(lda_fit, da_pca_val)
table(pred_lda$class, da_pca_val$state)

qda_fit = qda(state~., data=da_pca_train)
pred_qda = predict(qda_fit, da_pca_val)
table(pred_qda$class, da_pca_val$state)

#others    successful
#others          47245      24033
#successful    3756       3897

mean(pred_qda$class == da_pca_val$state)

#[1] 0.647933

3897/(3897 + 3756)

#[1] 0.5092121



# # scale data
# da_oh_scaled_train = scale(da_oh_train[, -5], center=TRUE, scale=TRUE)
# da_oh_scaled_train = data.frame(da_oh_scaled_train, da_oh_train$state)
# names(da_oh_scaled_train)[28] = "state"


# scale data by hand
my_scale = function(col){
  col = (col - mean(col))/sd(col)
}
da_oh_scaled_train = apply(da_oh_train[, -4], 2, my_scale)
da_oh_scaled_train = data.frame(da_oh_scaled_train, da_oh_train$state)
names(da_oh_scaled_train)[27] = "state"
da_oh_scaled_val = da_oh_val[, -4]
for(i in 1:ncol(da_oh_scaled_val)){
  train_data = da_oh_train[, -4]
  da_oh_scaled_val[, i] = (da_oh_scaled_val[, i] - mean(train_data[, i]))/sd(train_data[, i])
}
da_oh_scaled_val = data.frame(da_oh_scaled_val, da_oh_val$state)
names(da_oh_scaled_val)[27] = "state"


# KNN with cross-validation ran too long so we used 10% of the training data to run KNN with cross-validation

da_KNN_train=read.csv("TrainingKickstarter.csv")
train.size=floor(nrow(da_KNN_train)*.1)
train.size
#[1] 18417
set.seed(1)
da_KNN_train_percent=sample(seq_len(nrow(da_KNN_train)),size=train.size)
da_KNN_train_percent=da_KNN_train[da_KNN_train_percent,]
dim(da_KNN_train_percent)
#[1] 18417    22

da_KNN_train_percent= da_KNN_train_percent[, c("currency",
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

summary(da_KNN_train_percent)
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

glimpse(da_KNN_train_percent)
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
KNN_fit=train(state~.,method="knn",tuneGrid=expand.grid(k=1:10),trControl=trControl,metric="Accuracy",data=da_KNN_train_percent)
pred_KNN=predict(KNN_fit,da_KNN_train_percent)
table(pred_KNN,da_KNN_train_percent$state)

#pred_KNN     others successful
#others      10197       3861
#successful   1680       2679

mean(pred_KNN==da_KNN_train_percent$state)
#[1] 0.699136

2679/(2679+1680)
#[1] 0.6145905

#SVM

SVMtune = tune(svm, state~., data = train_data , kernel="radial", ranges = list(cost = c(0.1,0.5,1,5,10,50,100), gamma = c(0.1,0.25,0.5,0.75,1,5)))
#best parameters:
#  cost gamma
#1      0.1
SVMrad = svm(state~., data=KStrain, kernel="radial", gamma = 0.1, cost=1)
conf_mat = table(predict = predict(SVMrad, KStest), truth = KStest$state)
#others    successful
#others         51000     27930
#successful       1              0

#“Test Error”
#[1] 0.353866

#“NPV”
#[1] 0

SVMrad = svm(state~., data=KStrain, kernel="radial", gamma = 0.1, cost=1, class.weights=c("others"=0.4,"successful"=0.6))
conf_mat = table(predict = predict(SVMrad, KStest), truth = KStest$state)
#others    successful
#others         38677      16661
#successful    12324       11269

#“Test Error”
#[1] 0.367219

#“NPV”
#[1] 0.477641
