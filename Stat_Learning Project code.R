##################################################### PACKAGES
# General Packages
library(caret)     
library(stats)
library(tidyverse)
library(MASS)
# kNN Packages
library(class)     
# Discriminant Analysis Packages
library(mda)
library(klaR)
# Neural Network Packages
library(nnet)    
# K-means Packages
library(clue)    
# Heirchechal Packages
library(dendextend)    
library(dplyr)
#Decision Tree Packages
library(tree)
# Bagging Packages
library(ipred)       
library(rpart)
#Random Forest Packages
library(randomForest)
#XGboost Packages
library(xgboost)
library(e1071)

##################################################### PRE-PROCESSING
set.seed(1)

#load data
df = data.frame(read.csv(file = "F:\\old USB\\PhD Math Concordia\\Winter 2020\\MAST 881\\Project\\NEW_Skyserver DR16_SQL_data.csv", header = TRUE))

y = df[,10] #responses
df = df[,-c(1,9,10)] #predictors

#selecting train and test set
samp = sample(1:100000, size = 2500, replace = FALSE) #select a computationally reasonable subset

tr_n = 2000
ts_n = 500

tr = sample(samp, size = tr_n, replace = FALSE)
ts = sample(samp[!(samp %in% tr)], size = ts_n, replace = FALSE)

train = df[tr,] 
test = df[ts,] 

#scale inputs
scale_train = scale(train, center = TRUE, scale = TRUE)
scale_test = scale(test, center = TRUE, scale = TRUE)

y_train = y[tr]
y_test = y[ts]

#Some function calls require a certain type of input
train_df = data.frame(cbind(y_train, train))
colnames(train_df)[1] = "class"
test_df = data.frame(cbind(y_test,test))
colnames(test_df)[1] = "class"

dummy = function(x){
  ret = vector()
  for(i in 1:length(x)){
    if(x[i] == "GALAXY"){
      ret[i] = 1
    }else if(x[i] == "QSO"){
      ret[i] = 2
    }else if(x[i] == "STAR"){
      ret[i] = 3
    }
  }
  return(ret)
}

##################################################### DATA INTEGRITY CHECKS

nrow(df)
#100000
ncol(df)
#8

#checking for outliers
boxplot(df)
which(df$u < -8000)
boxplot(df[-c(57902,91562),])

#checking for erroneous values
sum(is.na(df))
#0
sum(is.infinite(as.matrix(abs(df))))
#0
which(df$ra < 0)
#integer(0)
which(df$ra > 360)
#integer(0)
which(df$dec < -90)
#integer(0)
which(df$dec > 90)
#integer(0)

##################################################### DESCRIPTIVE STATISTICS

plot(df[sample(1:100000,100),])

summary(df)

galaxy = df[which(y == "GALAXY"),]
apply(galaxy, 2, mean) #mean vector for GALAXY
cov(galaxy) #covariance matrix for GALAXY
quasar = df[which(y == "QSO"),]
apply(quasar, 2, mean) #mean vector for QSO
cov(quasar) #covariance matrix for QSO
star = df[which(y == "STAR"),]
apply(star, 2, mean) #mean vector for STAR
cov(star) #covariance matrix for STAR


##################################################### K-NEAREST NEIGHBORS
if(FALSE){ #special code block for 10-fold cross validation - not to be run
  scale_train_df = data.frame(cbind(y_train, scale_train))
  colnames(scale_train_df)[1] = "class"
  scale_test_df = data.frame(cbind(y_test,scale_test))
  colnames(test_df)[1] = "class"
  
  fit = train(factor(class) ~ .,
              method     = "knn",
              tuneGrid   = expand.grid(k = 1:10),
              trControl  = trainControl(method  = "cv", number  = 10),
              metric     = "Accuracy",
              data       = scale_train_df)
  
  #k_opt = 3 in every situation
}

#Leave one out cross validation to select optimal k
error = rep(0,10)
for(k in 1:10){
  print(k) # output to monitor progress of loop
  for(j in 1:nrow(scale_train)){
    if(j %% 1000 == 0) print(j)
    pred = knn(scale_train[-j,], scale_train[j,], y_train[-j], k)
    if(pred != y_train[j]){
      error[k] = error[k] + 1
    }
  }
  error[k] = error[k]/nrow(scale_train)
}

#choosing optimal k
plot(error, type="b", main = "Validation Error Rate - kNN", xlab = "k", ylab = "Error Rate") 
k_opt = which.min(error)

#calculating test error rate
pred = knn(scale_train, scale_test, y_train, k_opt)
err_rate = mean(pred != y_test)
#Error Rate = 0.098

#Confusion Matrix
confusionMatrix(pred,y_test)
#Prediction GALAXY QSO STAR
#GALAXY        272   6   22
#QSO             1  38    0
#STAR           16   3  142

### We try PCA to reduce the dimensionality (see below)

####################################################  K-means Clustering
#k has to be 3
km = kmeans(x = train, centers = 3, nstart = 25)
res = dummy(y_test)

pred = cl_predict(km, newdata = test, type = "class_ids")
err_rate = mean(pred != res)
#Error rate = 0.5-0.8

#Confusion Matrix
confusionMatrix(factor(pred),factor(res))
#Prediction   GALAXY   QSO   STAR
#GALAXY          156    16     73
#QSO               1    24      0
#STAR            132     7     91

### We try PCA to reduce the dimensionality (see below)

####################################################################################### Hierchechal Clustering

dist_mat = dist(test, method = 'euclidean')
res = dummy(y_test)

hclust_avg = hclust(dist_mat, method = 'average')
avg_dend = color_branches(as.dendrogram(hclust_avg), k = 3) 
plot(avg_dend, leaflab = "none", yaxt = 'n')                                                       
cut_avg = cutree(hclust_avg, k = 3)
err_rate = mean(cut_avg != res)
#Error Rate = 0.418

#confusionMatrix(factor(cut_avg),factor(res))

hclust_ward.D = hclust(dist_mat, method = 'ward.D')
ward.D_dend = color_branches(as.dendrogram(hclust_ward.D), k = 3)
plot(ward.D_dend, leaflab = "none", yaxt = 'n')
cut_ward.D = cutree(hclust_ward.D, k = 3)
err_rate = mean(cut_ward.D != res)
#Error Rate = 0.634

#confusionMatrix(factor(cut_ward.D),factor(res))

hclust_ward.D2 = hclust(dist_mat, method = 'ward.D2')
ward.D2_dend = color_branches(as.dendrogram(hclust_ward.D2), k = 3)
plot(ward.D2_dend)
cut_ward.D2 = cutree(hclust_ward.D2, k = 3)
err_rate = mean(cut_ward.D2 != res)
#Error Rate = 0.63

#confusionMatrix(factor(cut_ward.D2),factor(res))

hclust_single = hclust(dist_mat, method = 'single')
single_dend = color_branches(as.dendrogram(hclust_single), k = 3)
plot(single_dend)
cut_single = cutree(hclust_single, k = 3)
err_rate = mean(cut_single != res)
#Error Rate = 0.418

#confusionMatrix(factor(cut_single),factor(res))

hclust_complete = hclust(dist_mat, method = 'complete')
complete_dend = color_branches(as.dendrogram(hclust_complete), k = 3)
plot(complete_dend)
cut_complete = cutree(hclust_complete, k = 3)
err_rate = mean(cut_complete != res)
#Error Rate = 0.618

#confusionMatrix(factor(cut_complete),factor(res))

hclust_mcquitty = hclust(dist_mat, method = 'mcquitty')
mcquitty_dend = color_branches(as.dendrogram(hclust_mcquitty), k = 3)
plot(mcquitty_dend)
cut_mcquitty = cutree(hclust_mcquitty, k = 3)
err_rate = mean(cut_mcquitty != res)
#Error Rate = 0.418

#confusionMatrix(factor(cut_mcquitty),factor(res))

hclust_median = hclust(dist_mat, method = 'median')
median_dend = color_branches(as.dendrogram(hclust_median), k = 3)
plot(median_dend)
cut_median = cutree(hclust_median, k = 3)
err_rate = mean(cut_median != res)
#Error Rate = 0.418

#confusionMatrix(factor(cut_median),factor(res))

hclust_centroid = hclust(dist_mat, method = 'centroid')
centroid_dend = color_branches(as.dendrogram(hclust_centroid), k = 3)
plot(centroid_dend)
cut_centroid = cutree(hclust_centroid, k = 3)
err_rate = mean(cut_centroid != res)
#Error Rate = 0.418

#confusionMatrix(factor(cut_centroid),factor(res))

############################################################################################# Discriminant Analysis 

######################################## LDA

model_lda = lda(factor(train_df$class)~., data = train_df)
pred_lda = predict(model_lda, test_df)
err_rate = mean(pred_lda$class != test_df$class)
#Error rate 0.166

#Confusion Matrix
confusionMatrix(pred_lda$class, y_test)
#Prediction GALAXY QSO STAR
#GALAXY        266   9   39
#QSO             1  26    0
#STAR           22  12  125

######################################## QDA

model_qda = qda(factor(train_df$class)~., data = train_df)
pred_qda = predict(model_qda, test_df)
err_rate = mean(pred_qda$class != test_df$class)
#Error rate 0.03

#Confusion Matrix
confusionMatrix(pred_qda$class, y_test)
#Prediction GALAXY QSO STAR
#GALAXY        281   6    0
#QSO             5  41    1
#STAR            3   0  163


######################################## MDA

model_mda = mda(factor(train_df$class)~., data = train_df)
pred_mda = predict(model_mda, test_df)
err_rate = mean(pred_mda != test_df$class)
#Error rate 0.13

#Confusion Matrix
confusionMatrix(pred_mda, y_test)
#Prediction GALAXY QSO STAR
#GALAXY        272  11   34
#QSO             1  33    0
#STAR           16   3  130

######################################## FDA

model_fda = fda(factor(train_df$class)~., data = train_df)
pred_fda = predict(model_fda, test_df)
err_rate = mean(pred_fda != test_df$class)
#Error rate 0.166

#Confusion Matrix
confusionMatrix(pred_fda, y_test)
#Prediction GALAXY QSO STAR
#GALAXY        266   9   39
#QSO             1  26    0
#STAR           22  12  125

######################################## RDA

model_rda = rda(factor(train_df$class)~., data = train_df)
pred_rda = predict(model_rda, test_df)
err_rate = mean(pred_rda$class != test_df$class)
#Error rate 0.03

#Confusion Matrix
confusionMatrix(pred_rda$class, y_test)
#Prediction GALAXY QSO STAR
#GALAXY        281   6    0
#QSO             5  41    1
#STAR            3   0  163

################################################################################# Neural Network

#creates dummy variables for the neural network
dummy_vars = class.ind(y_train) 

#10-fold cross validation
err = matrix(nrow = 150, ncol = 10)
for(j in 1:150){
  print(j) # monitor progress of loop
  for(k in 1:10){
    NeuralNet = nnet(x = train[!((1:nrow(train)) %in% (1:(nrow(train)/10) + (k-1)*nrow(train)/10)),], 
                     y = dummy_vars[!((1:nrow(train)) %in% (1:(nrow(train)/10) + (k-1)*nrow(train)/10)),],
                     size = j, softmax = TRUE, maxit = 100, MaxNWts = 1000000, trace = FALSE)
    
    valid = train[(1:(nrow(train)/10) + (k-1)*nrow(train)/10),]
    y_valid = y_train[(1:(nrow(train)/10) + (k-1)*nrow(train)/10)]
    
    pred = predict(NeuralNet, newdata = valid)
    error = rep(0, nrow(valid))
    for(i in 1:nrow(valid)){
      if((which.max(pred[i,]) == 1 && y_valid[i] != "GALAXY") ||
         (which.max(pred[i,]) == 2 && y_valid[i] != "QSO") ||
         (which.max(pred[i,]) == 3 && y_valid[i] != "STAR") ){
        error[i] = 1
      }
    }
    
    err[j,k] = mean(error)
  }
}

err_rate = apply(err, 1, mean)
plot(err_rate, type="l", main = "Error Rate - Neural Network", xlim = c(0,150), xlab = "Hidden Nodes", ylab = "Error Rate")

NeuralNet = nnet(x = train, y = dummy_vars, size = 100, softmax = TRUE, maxit = 100000, MaxNWts = 1000000)

# out-of-sample test
pred = predict(NeuralNet, newdata = test)
error = rep(0, nrow(test))
for(i in 1:nrow(test)){
  if((which.max(pred[i,]) == 1 && y_test[i] != "GALAXY") ||
     (which.max(pred[i,]) == 2 && y_test[i] != "QSO") ||
     (which.max(pred[i,]) == 3 && y_test[i] != "STAR") ){
    error[i] = 1
  }
}
err_rate = mean(error)
#Error Rate = 0.028

#Confusion Matrix
forCM = vector()
for(i in 1:nrow(test)){
  if(which.max(pred[i,]) == 1){
    forCM[i] = "GALAXY"
  }else if(which.max(pred[i,]) == 2){
    forCM[i] = "QSO"
  }else if(which.max(pred[i,]) == 3){
    forCM[i] = "STAR"
  }
}

confusionMatrix(factor(forCM), y_test)
#Prediction GALAXY QSO STAR
#GALAXY        285  10    0
#QSO             1  37    0
#STAR            3   0  164

################################################################################# Multinomial Logisitic Regression
logistic_reg = multinom(factor(train_df$class)~., data = train_df, maxit = 10000)
summary(logistic_reg)

pred = predict(logistic_reg, newdata = test_df)
err_rate = mean(pred != y_test)
#Error rate = 0.028

#Confusion Matrix
confusionMatrix(pred, y_test)
#Prediction GALAXY QSO STAR
#GALAXY        285  10    0
#QSO             1  37    0
#STAR            3   0  164

################################################################### Decision Trees
tm = tree(factor(train_df$class)~.,data = train_df)
summary(tm)

plot(tm)
text(tm, pretty = 0)

err_rate = mean(predict(tm, newdata = test_df, type="class") != y_test) 
#Error rate = 0.036

#Leave-one-out cross validation
cv.tm = cv.tree(tm, FUN = prune.misclass, K = nrow(train_df))

par(mfrow = c(1,2))
plot(cv.tm$size ,cv.tm$dev ,type ="b")
plot(cv.tm$k ,cv.tm$dev ,type ="b")

#Pruning the tree
prune.tm = prune.misclass (tm, best = 3)
par(mfrow = c(1 ,1))
plot(prune.tm)
text(prune.tm, pretty = 0)
print(prune.tm)

pred = predict(prune.tm, test_df, type ="class")
err_rate = mean(pred != y_test)
#Error rate = 0.036

#Confusion Matrix
confusionMatrix(pred, y_test)
#Prediction GALAXY QSO STAR
#GALAXY        285  14    0
#QSO             1  33    0
#STAR            3   0  164

################################################################### Bagging
OOB_err = vector()
for(bags in seq(from = 10, to = 300, by = 10)){
  print(bags)
  bag_clus = bagging(formula = factor(train_df$class)~., 
                     data = train_df, 
                     nbagg = bags, 
                     OOB = TRUE,
                     coob = TRUE,
                     control = rpart.control(minsplit = 2, cp = 0))
  OOB_err[bags/10] = bag_clus$err
}
par(mfrow = c(1,1))
plot(OOB_err, type="b", ylab = "OOB Error Rate", xlab = "Number of Bags (x10)", main = "OOB Error Rate vs Number of Bags")

#remove noise by using a smoothing spline
sp = smooth.spline(na.omit(OOB_err), all.knots = TRUE, lambda = 0.01)
lines(sp$y, type="l", col="red")

#by inspection, take nbagg = 200
bag_clus = bagging(formula = factor(train_df$class) ~ ., data = train_df, nbagg = 200, coob = TRUE,
                   control = rpart.control(minsplit = 2, cp = 0))
pred = predict(bag_clus, newdata = test_df)
err_rate = mean(pred != y_test)
#Error rate 0.024

#Confusion Matrix
confusionMatrix(pred, y_test)
#Prediction GALAXY QSO STAR
#GALAXY        285   8    0
#QSO             1  39    0
#STAR            3   0  164

################################################################## Random Forest
#10-fold cross validation to select optimal predictor subset
err_rate0 = vector()
for(i in 1:8){
  error = rep(0, 10)
  for(j in 1:10){
    print(c(i,j)) #output to monitor progress of loop
    rf = randomForest(factor(train_df$class[!((1:nrow(train)) %in% (1:(nrow(train)/10) + (j-1)*nrow(train)/10))])~., 
                      data = train_df[!((1:nrow(train)) %in% (1:(nrow(train)/10) + (j-1)*nrow(train)/10)),], 
                      mtry = i, control = rpart.control(minsplit = 2, cp = 0))
    error[j] = mean(predict(rf, newdata = train_df[((1:nrow(train)) %in% (1:(nrow(train)/10) + (j-1)*nrow(train)/10)),]) != 
       train_df$class[((1:nrow(train)) %in% (1:(nrow(train)/10) + (j-1)*nrow(train)/10))])
  }
  
  err_rate0[i] = mean(error)
}
plot(err_rate0, type="b", ylab = "Error Rate", xlab = "Number of Variables Selected", main = "Optimal Subset Selection of Variables")

optSub = which.min(err_rate0)
#Optimal number of subsets is 3

#Optimal number of trees
err_rate1 = vector()
for(trees in seq(from = 10, to = 1000, by = 10)){
  print(trees) #output to monitor progress of loop
  rf = randomForest(factor(class)~., data = train_df, ntree = trees,
                    mtry = optSub,
                    control = rpart.control(minsplit = 2, cp = 0))
  err_rate1[trees/10] = rf$err.rate[trees,1]
}
plot(err_rate1, type="b", ylab = "OOB Error Rate", xlab = "Number of Trees (x10)", main = "OOB Error Rate vs Number of Trees")

#remove noise by using a smoothing spline
sp = smooth.spline(na.omit(err_rate1), all.knots = TRUE, lambda = 0.01)
lines(sp$y, type="l", col="red")

#10-fold cross validation to find optimal number of trees
error = matrix(nrow = 100, ncol = 10)
for(trees in seq(from = 10, to = 1000, by = 10)){
  print(trees)
  for(j in 1:10){
    rf = randomForest(factor(train_df$class[!((1:nrow(train)) %in% (1:(nrow(train)/10) + (j-1)*nrow(train)/10))])~., 
                      data = train_df[!((1:nrow(train)) %in% (1:(nrow(train)/10) + (j-1)*nrow(train)/10)),], 
                      ntree = trees,
                      mtry = optSub,
                      control = rpart.control(minsplit = 2, cp = 0))
    error[trees/10,j] = mean(predict(rf, newdata = train_df[((1:nrow(train)) %in% (1:(nrow(train)/10) + (j-1)*nrow(train)/10)),]) != 
       factor(train_df$class[((1:nrow(train)) %in% (1:(nrow(train)/10) + (j-1)*nrow(train)/10))]))
  }
}
err_rate2 = apply(error,1,mean)
optTrees = which.min(err_rate2)
plot(err_rate2, type="b", ylab = "Error Rate", xlab = "Number of Trees (x10)", main = "10-Fold Cross Validation for Optimal Number of Trees")

#by inspection, take ntree = 300
rf = randomForest(factor(train_df$class)~., data = train_df, ntree = optTrees,
                  mtry = optSub, control = rpart.control(minsplit = 2, cp = 0))
pred = predict(rf, newdata = test_df)
err_rate = mean(pred != y_test)
#0.022

#Confusion Matrix
confusionMatrix(pred,y_test)
#Prediction GALAXY QSO STAR
#GALAXY        285   7    0
#QSO             1  40    0
#STAR            3   0  164


################################################################# XGBoost
#10-fold cross validation
boost = train(factor(class)~.,data=train_df, method = "xgbTree", trControl = trainControl("cv", number = 10))

### Getting the predictions on the test data using the fitted XGboost model
pred = predict(boost, newdata = test_df)
err_rate = mean(pred != y_test)
#Error rate = 0.026

#Confusion Matrix
confusionMatrix(pred, y_test)
#Prediction GALAXY QSO STAR
#GALAXY        285   9    0
#QSO             1  38    0
#STAR            3   0  164

################################################################# PCA
pc = prcomp(train, center = TRUE, scale. = TRUE)
summary(pc)
#first 5 principle components cover 98.6% 

pc1 = pc$x[,1]
pc2 = pc$x[,2]
pc3 = pc$x[,3]
pc4 = pc$x[,4]
pc5 = pc$x[,5]

pc_df = cbind(pc1,pc2,pc3,pc4,pc5)

pc_new = predict(pc, newdata = test)
pc_new_df = cbind(pc_new[,1],pc_new[,2],pc_new[,3],pc_new[,4],pc_new[,5])


#KNN

#Leave one out cross validation to select optimal k
error = rep(0,10)
for(k in 1:10){
  print(k) #output to monitor progress of loop
  for(j in 1:nrow(pc_df)){
    pred = knn(pc_df[-j,], pc_df[j,], y_train[-j], k)
    if(pred != y_train[j]){
      error[k] = error[k] + 1
    }
  }
  error[k] = error[k]/nrow(pc_df)
}

#choosing optimal k
plot(error, type="l", main = "Validation Error Rate - PCA/kNN", xlab = "k", ylab = "Error Rate")
k_opt = which.min(error)

#calculating test error rate
pred = knn(pc_df, pc_new_df, y_train, k_opt)
err_rate = mean(pred != y_test)
#Error Rate = 0.102

#Kmeans

#k has to be 3
km = kmeans(x = pc_df, centers = 3, nstart = 25)
res = dummy(y_test)

pred = cl_predict(km, newdata = pc_new, type = "class_ids")
err_rate = mean(pred != res)
#Error Rate = 0.5-0.8
