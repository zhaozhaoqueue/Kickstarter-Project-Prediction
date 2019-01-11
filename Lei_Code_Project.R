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
da_train = raw_train_data[, c(21, 6, 8, 9, 5, 3, 20, 14, 13, 11, 19, 18, 16, 15)]
da_val = raw_val_data[, c(21, 6, 8, 9, 5, 3, 20, 14, 13, 11, 19, 18, 16, 15)]
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
table(pred_tree_unpruned, da_val[, 5])
1 - mean(pred_tree_unpruned == da_val[, 5])
# use cross-validatin to choose how to prune tree
set.seed(0)
cv_tree = cv.tree(tree_fit, FUN=prune.misclass)
cv_tree
# plot size and dev relation
plot(cv_tree$size, cv_tree$dev, type="b", xlab="Tree Size", ylab="Deviance")
best_tree = prune.misclass(tree_fit, best=10)
summary(best_tree)
# plot best tree
plot(best_tree)
text(best_tree, pretty=0)
# test
pred_tree = predict(best_tree, newdata=da_val, type="class")
table(pred_tree, da_val[, 5])
1 - mean(pred_tree == da_val[, 5])




# Bagging
library(randomForest)
set.seed(0)
bag_fit = randomForest(state~., data=da_train, mtry=13, importance=TRUE, ntree=20)
bag_fit
# importance of each variable
importance(bag_fit)
# plot importance
varImpPlot(bag_fit)
# test
pred_bag = predict(bag_fit, newdata=da_val)
table(pred_bag, da_val[, 5])
1 - mean(pred_bag == da_val[, 5])





# Random Forest
set.seed(0)
rf_fit = randomForest(state~., data=da_train, mtry=3, importance=TRUE, ntree=20)
rf_fit
# importance of each variable
importance(rf_fit)
# plot importance
varImpPlot(rf_fit)
pred_rf = predict(rf_fit, newdata=da_val)
table(pred_rf, da_val[, 5])
1- mean(pred_rf == da_val[, 5])



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
da_oh_train = da_oh_train[, -6]
da_oh_val = da_oh_val[, -6]
names(da_oh_train)




# 10-fold cross validaiton
folds = cut(seq(1:nrow(da_train)), breaks=10, labels=FALSE)





# Logistic Regression
# cor(da_oh_train[, -5])
# fold = sample(1:10, nrow(da_oh), replace=TRUE)
contrasts(da_oh_train$state)
# Use 10-fold cross validation to select threshold
# three value in log_cv[i, j] are accuracy, accuracy_successful, accuracy_other
log_cv = array(NA, c(10, 9, 3))
for(i in 1:10){
        testIndexes = which(folds==i, arr.ind=TRUE)
        testData = da_oh_train[testIndexes, ]
        trainData = da_oh_train[-testIndexes, ]
        log_fit = glm(state~., family=binomial, data=trainData)
        prob_log = predict(log_fit, newdata=testData, type="response")
        for(j in 1:9){
                pred_log = ifelse(prob_log>=j/10, "successful", "other")
                tab = table(pred_log, testData[, 5])
                log_cv[i, j, 1] = mean(pred_log == testData[, 5])
                log_cv[i, j, 2] = tab[2, 2]/(tab[2, 2] + tab[1, 2])
                log_cv[i, j, 3] = tab[1, 1]/(tab[1, 1] + tab[2, 1])
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
par(mfrow=c(1,1))

# Use selected threshold to train and test model
# For example, threshold=0.4
log_fit_best = glm(state~., family=binomial, data=da_oh_train)
summary(log_fit)
prob_log = predict(log_fit, newdata=da_oh_val, type="response")
pred_log = ifelse(prob_log>=0.4, "successful", "others")
table(pred_log, da_oh_val[, 5])
1 - mean(pred_log == da_oh_val[, 5])




# PCA
pca_train = prcomp(da_oh_train[, -5], center=TRUE, scale=TRUE)
summary(pca_train)
# plot PVE curve
par(mfrow=c(2, 1))
plot(summary(pca_train)$importance[2, ], type="o", ylab="PVE", xlab="Principal Component", col =" blue ")
plot(summary(pca_train)$importance[3, ], type="o", ylab=" Cumulative PVE", xlab="Principal Component", col =" blue ")
# plot data after PCA
par(mfrow=c(1, 1))
plot(pca_train$x[, 1:2], col=as.numeric(da_oh_train[, 5])+1)

# According PVE plot, select 5 components
da_pca_train = data.frame(pca_train$x[, 1:5], da_oh_train[, 5])
names(da_pca_train)[6] = "state"
# v = predict(pca_train, da_oh_val[, -5])
# summary(v)
da_pca_val = data.frame(predict(pca_train, da_oh_val[, -5])[, 1:5], da_oh_val[, 5])
names(da_pca_val)[6] = "state"








# LDA and QDA ???How to deal with main_category??? How to encode it???One-hot encoding produces collinear problem.
# Try to use PCA to solve collinear problem
library(MASS)
lda_fit = lda(state~., data=da_pca_train)
pred_lda = predict(lda_fit, da_pca_val)
table(pred_lda$class, da_pca_val[, 6])

qda_fit = qda(state~., data=da_pca_train)
pred_qda = predict(qda_fit, da_pca_val)
table(pred_qda$class, da_pca_val[, 6])










# # scale data
# da_oh_scaled_train = scale(da_oh_train[, -5], center=TRUE, scale=TRUE)
# da_oh_scaled_train = data.frame(da_oh_scaled_train, da_oh_train$state)
# names(da_oh_scaled_train)[28] = "state"


# scale data by hand
my_scale = function(col){
        col = (col - mean(col))/sd(col)
}
da_oh_scaled_train = apply(da_oh_train[, -5], 2, my_scale)
da_oh_scaled_train = data.frame(da_oh_scaled_train, da_oh_train$state)
names(da_oh_scaled_train)[28] = "state"
da_oh_scaled_val = da_oh_val[, -5]
for(i in 1:ncol(da_oh_scaled_val)){
        train_data = da_oh_train[, -5]
        da_oh_scaled_val[, i] = (da_oh_scaled_val[, i] - mean(train_data[, i]))/sd(train_data[, i])
}
da_oh_scaled_val = data.frame(da_oh_scaled_val, da_oh_val$state)
names(da_oh_scaled_val)[28] = "state"
















# KNN and SVM run too long time, haven't finished one running.






# KNN
library(class)
# test if knn works
# This process takes 5 minutes.
pred_test_knn = knn(da_oh_scaled_train[, -28], da_oh_scaled_val[, -28], da_oh_scaled_train$state, k=2)
1 - mean(pred_test_knn == da_oh_scaled_val$state)
# k=1 results in 37.21% accuracy.
# k=2 results in 37.67% accuracy.

# KNN with 10-fold cross validation
# this takes a longer time, haven't finished one running.
# 10 columns: k=1:10.
# 10 rows: 10-fold cross validation
knn_cv = matrix(0, 10, 10)
for(j in 1:10){
        for(i in 1:10){
                testIndexes = which(folds==i, arr.ind=TRUE)
                testData = da_oh_scaled_train[testIndexes, -28]
                trainData = da_oh_scaled_train[-testIndexes, -28]
                pred_knn = knn(trainData, testData, da_oh_scaled_train$state[-testIndexes], k=j)
                knn_cv[i, j] = 1 - mean(pred_knn == da_oh_scaled_train$state[testIndexes])
        }
}
knn_cv = apply(knn_cv, 2, mean)
knn_cv









# SVM and tune with radial kernel using scaled data
# SVM and tune are too slow, havn't finished one running.
library(e1071)
set.seed(0)
tune_out = tune(svm, state~., data=da_oh_scaled_train, kernel="radial")
                #,ranges=list(cost=c(0.001, 0.01, 0.1), gamma=c(0.5,1,2)))
summary(tune_out)
best_svm = tune_out$best.model
pred_svm = predict(best_svm, da_oh_scaled_val)
table(pred_svm, da_oh_scaled_val$state)