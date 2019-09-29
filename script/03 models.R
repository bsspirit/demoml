library(tidyverse)
library(lubridate)
library(caret)

library(ROCR)
# functions
f1_score <- function(pred, label) {
  prec <- sum(label[pred==1]==1) / sum(pred==1)
  rec <- sum(pred[label==1]==1) / sum(label==1)
  f1 <- 2*prec*rec / (prec+rec)
  print(paste0('precision:',prec))
  print(paste0('recall:',rec))
  print(paste0('F1 score:',f1))
}
opt.cut <- function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x-0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
plot_roc <- function(pred, label) {
  pred <- prediction(pred, label)
  roc.perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  plot(roc.perf)
  abline(a=0,b=1)
  print(opt.cut(roc.perf,pred))
  # calculate auc
  auc.perf <- performance(pred,measure = "auc")
  print(paste0('AUC:',auc.perf@y.values[[1]]))
}

# 2 models --------------------------------------------
data("iris")
df <- iris
names(df) <- gsub('\\.','_',names(df))
df$Species <- ifelse(iris$Species == 'versicolor',1,0)

# logistic regression
library(gmodels)
model_logit <- glm(Species ~ .,
                   data = df,
                   family = 'binomial')
summary(model_logit)
plot_roc(model_logit$fitted.values, df$Species)

# decision tree
library(rpart)
library(rpart.plot)
model_dtree <- rpart(Species ~ ., data = df, method = 'class')
rpart.plot(model_dtree,branch=1,type=2, fallen.leaves=T,cex=0.8, sub="??֦ǰ")
model_dtree <- prune(model_dtree,
                     cp=model_dtree$cptable[which.min(model_dtree$cptable[,"xerror"]),"CP"])
rpart.plot(model_dtree,branch=1,type=4, fallen.leaves=T,cex=0.8, sub="??֦??")

pred <- predict(model_dtree, df[,1:4])[,2]
plot_roc(pred, df$Species)

# random forest
library(randomForest)
model_rf <- randomForest(as.factor(Species) ~ ., data = df,
                         ntree = 12, importance = TRUE)
model_rf$confusion
pred <- predict(model_rf, df[,1:4])
f1_score(pred, df$Species)


fitControl <- trainControl(method = "repeatedcv",
                           number = 5, repeats = 2, search = "random")
model_rf <- train(as.factor(Species) ~ ., data = df, method = "rf",
                  trControl = fitControl)

# xgboost
library(xgboost)
model_xgboost <- xgboost(data = as.matrix(df[,1:4]), label = df[,5],
                         max.depth = 1, eta = 0.2425924, nround = 284,
                         objective = "binary:logistic")

pred <- predict(model_xgboost, as.matrix(df[,1:4]))
plot_roc(pred, df$Species)


fitControl <- trainControl(method = "repeatedcv",
                           number = 5, repeats = 2, search = "random")
model_xgboost <- train(as.factor(Species) ~ ., data = df, method = "xgbTree",
                       trControl = fitControl)
print(model_xgboost)

# gbdt
library(gbm)
fitControl <- trainControl(method = "repeatedcv",
                           number = 5, repeats = 2, search = "random")
model_gbdt <- train(as.factor(Species) ~ .,
                    data = df, method = "gbm",
                    trControl = fitControl)
print(model_gbdt)
pred <- predict(model_gbdt, df[,1:4])
f1_score(pred, df$Species)







# other models
# library(nnet)
# library(bnclassify)