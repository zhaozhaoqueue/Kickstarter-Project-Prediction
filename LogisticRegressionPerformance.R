# load and check data
d_tr = read.csv("TrainingKickstarter.csv")
d_val = read.csv("validationKickstarter.csv")
d_te = read.csv("VaultedKickstarter.csv")
# dim(d_tr)
# dim(d_val)
# dim(d_te)
# names(d_te)


# combine training data and validation data
raw_train_data = rbind(d_tr, d_val)
da_train = raw_train_data[, c(21, 9, 5, 3, 20, 14, 13, 11, 19, 18, 16, 15)]

# preprocess test data
da_test = d_te[, c(20, 8, 4, 2, 19, 13, 12, 10, 18, 17, 15, 14)]

# One-hot encode "main_category"
da_oh_train = da_train
da_oh_test = da_test
for(cat in unique(da_oh_train$main_category)){
        da_oh_train[paste("cat", cat, sep=".")] = ifelse(da_oh_train$main_category == cat, 1, 0)
        da_oh_test[paste("cat", cat, sep=".")] = ifelse(da_oh_test$main_category == cat, 1, 0)
}
da_oh_train = da_oh_train[, -4]
da_oh_test = da_oh_test[, -4]


# 10-fold cross validaiton
folds = cut(seq(1:nrow(da_train)), breaks=10, labels=FALSE)


# check state encoding
contrasts(da_oh_train$state)


# Use 10-fold cross validation to select threshold
# Three values in log_cv[i, j] are overall accuracy, accuracy of success, accuracy of others
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
                log_cv[i, j, 1] = mean(pred_log == testData$state)
                if(dim(tab)[1] != 1){
                        log_cv[i, j, 2] = tab[2, 2]/(tab[2, 2] + tab[2, 1])        
                }
                log_cv[i, j, 3] = tab[1, 1]/(tab[1, 1] + tab[1, 2])
        }
}
log_cv_acc = matrix(0, nrow=9, ncol=3, dimnames=list(seq(0.1, 0.9, 0.1), 
                                                     c("Accuracy", "Acc_success", "Acc_other")))
for(i in 1:10){
        for(j in 1:9){
                log_cv_acc[j, ] = log_cv_acc[j, ] + log_cv[i, j, ]
        }
}
log_cv_acc = log_cv_acc/10
log_cv_acc


# plot overall accuracy, accuracy of success, accuracy of others
par(mfrow=c(1, 3))
plot(log_cv_acc[, 1], type="b", xlab="threshold * 10", ylab="Accuracy")
plot(log_cv_acc[, 2], type="b", xlab="threshold * 10", ylab="Accuracy of Success")
plot(log_cv_acc[, 3], type="b", xlab="threshold * 10", ylab="Accuracy of Other")
par(mfrow=c(1,1))

# according to the accuracy of success, I select threshold=0.7
log_best_fit = glm(state~., family=binomial, data=da_oh_train)
summary(log_best_fit)
prob_log = predict(log_best_fit, newdata=da_oh_test, type="response")
pred_log = ifelse(prob_log>=0.7, "successful", "others")
con_matrix = table(pred_log, da_oh_test$state)
print("Confusion Matrix")
print(con_matrix)
print("Overall Accuracy")
print(mean(pred_log == da_oh_test$state))
print("Accuracy of Success")
print(con_matrix[2, 2]/(con_matrix[2, 2] + con_matrix[2, 1]))
