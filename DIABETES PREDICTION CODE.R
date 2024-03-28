rm(list = ls())

library(readr)
library(tidyverse)
library(randomForest)
library(pROC)
library(e1071)
library(readr)
Anthropometric_10_05 <- read.csv("C:/Users/ADMIN/Downloads/Anthropometric_10_05.csv")

Anthropometric_10_05 <- read_csv("C:/Users/ADMIN/Downloads/Anthropometric_10_05.csv", col_types = cols(Sex = col_factor(levels = c("M", "F"))))
View(Anthropometric_10_05)

Anthropometric_10_05$Sex=as.factor(Anthropometric_10_05$Sex)

#clubing of Somewhatexcersises with moderately excerises in Ls

data=Anthropometric_10_05%>%
  mutate(Ls=str_replace_all(Ls,'Somewhat exercises','Moderately exercises'))
data$Ls=as.factor(data$Ls)
data$Diabetes=as.factor(data$Diabetes)

#converting age_coded into factor

data$Age_coded=as.factor(data$Age_coded)
View(data)

data=data%>%mutate(wthr=Waist/Ht)
summary(data$Diabetes)
table(data$Age_coded,data$Sex)
summary(data)

#train-test split
set.seed(seed=1234)
train=sample(c(TRUE,FALSE),size=nrow(data),replace = TRUE,p=c(0.8,0.2))
train_data=data[train,]
test_data=data[!train,]
test_data_male=test_data%>%filter(Sex=="M")
test_data_female=test_data%>%filter(Sex=="F")

#model -logistic-male

data_model_2=train_data%>%filter(Sex=="M")%>%select(Ls,Age_coded,Diabetes,BMI)
model_2=glm(Diabetes~Ls+Age_coded+BMI,family = binomial,data=data_model_2)
summary(model_2)
predictions <- predict(model_2, type = "response")
roc_curve_1 <- roc(data_model_2$Diabetes, predictions)
auc <- auc(roc_curve_1)
print(auc)
pred_test=predict(model_2,newdata=test_data_male,type = "response")
pred_y=ifelse(pred_test>=0.5,1,0)
confusion_mat=table(pred_y,test_data_male$Diabetes)
TP <- confusion_mat[1, 1]
TN <- confusion_mat[2, 2]
FP <- confusion_mat[1, 2]
FN <- confusion_mat[2, 1]
accuracy <- (TP + TN) / (TP + TN + FP + FN)
recall <- TP / (TP + FN)
precision <- TP / (TP + FP)
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("Accuracy: ", accuracy, "\n")
cat("Recall: ", recall, "\n")
cat("Precision: ", precision, "\n")
cat("F1 Score: ", f1_score, "\n")
confusion_mat

plot(roc_curve_1, main = "ROC Curve")

#model -logistic-female

data_model_3=train_data%>%filter(Sex=="F")%>%select(Ls,Age_coded,Diabetes,BMI)
model_3=glm(Diabetes~Ls+Age_coded+BMI,family = binomial,data=data_model_3)
summary(model_3)
predictions <- predict(model_3, type = "response")
roc_curve_2 <- roc(data_model_3$Diabetes, predictions)
auc <- auc(roc_curve_2)
print(auc)
pred_test=predict(model_3,newdata=test_data_female,type = "response")
pred_y=ifelse(pred_test>=0.5,1,0)
confusion_mat=table(pred_y,test_data_female$Diabetes)
TP <- confusion_mat[1, 1]
TN <- confusion_mat[2, 2]
FP <- confusion_mat[1, 2]
FN <- confusion_mat[2, 1]
accuracy <- (TP + TN) / (TP + TN + FP + FN)
recall <- TP / (TP + FN)
precision <- TP / (TP + FP)
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("Accuracy: ", accuracy, "\n")
cat("Recall: ", recall, "\n")
cat("Precision: ", precision, "\n")
cat("F1 Score: ", f1_score, "\n")
confusion_mat
plot(roc_curve_2, main = "ROC Curve", print.auc = TRUE, auc.polygon = TRUE,add=TRUE)

#model -logistic-male

data_model_2=train_data%>%filter(Sex=="M")%>%select(Ls,Age_coded,Diabetes,wthr)
model_2=glm(Diabetes~Ls+Age_coded+wthr,family = binomial,data=data_model_2)
summary(model_2)
predictions <- predict(model_2, type = "response")
roc_curve_3 <- roc(data_model_2$Diabetes, predictions)
auc <- auc(roc_curve_3)
print(auc)
pred_test=predict(model_2,newdata=test_data_male,type = "response")
pred_y=ifelse(pred_test>=0.5,1,0)
confusion_mat=table(pred_y,test_data_male$Diabetes)
TP <- confusion_mat[1, 1]
TN <- confusion_mat[2, 2]
FP <- confusion_mat[1, 2]
FN <- confusion_mat[2, 1]
accuracy <- (TP + TN) / (TP + TN + FP + FN)
recall <- TP / (TP + FN)
precision <- TP / (TP + FP)
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("Accuracy: ", accuracy, "\n")
cat("Recall: ", recall, "\n")
cat("Precision: ", precision, "\n")
cat("F1 Score: ", f1_score, "\n")
confusion_mat
plot(roc_curve_3, main = "ROC Curve", print.auc = TRUE, auc.polygon = TRUE)


#model -logistic-female

data_model_3=train_data%>%filter(Sex=="F")%>%select(Ls,Age_coded,Diabetes,wthr)
model_3=glm(Diabetes~Ls+Age_coded+wthr,family = binomial,data=data_model_3)
summary(model_3)
predictions <- predict(model_3, type = "response")
roc_curve_4 <- roc(data_model_3$Diabetes, predictions)
auc <- auc(roc_curve_4)
print(auc)
pred_test=predict(model_3,newdata=test_data_female,type = "response")
pred_y=ifelse(pred_test>=0.5,1,0)
confusion_mat=table(pred_y,test_data_female$Diabetes)
TP <- confusion_mat[1, 1]
TN <- confusion_mat[2, 2]
FP <- confusion_mat[1, 2]
FN <- confusion_mat[2, 1]
accuracy <- (TP + TN) / (TP + TN + FP + FN)
recall <- TP / (TP + FN)
precision <- TP / (TP + FP)
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("Accuracy: ", accuracy, "\n")
cat("Recall: ", recall, "\n")
cat("Precision: ", precision, "\n")
cat("F1 Score: ", f1_score, "\n")
confusion_mat

plot(roc_curve_4, main = "ROC Curve", print.auc = TRUE, auc.polygon = TRUE)

#random forest -Female

data_model_rf=train_data%>%filter(Sex=="F")%>%select(Ls,Age_coded,Diabetes,wthr,BMI)
random_forest = randomForest(Diabetes~., data=data_model_rf, ntree=100, importance=T)
importance(random_forest)
varImpPlot(random_forest)

pred_y=predict(random_forest,newdata = test_data_female)
confusion_mat=table(pred_y,test_data_female$Diabetes)
TP <- confusion_mat[1, 1]
TN <- confusion_mat[2, 2]
FP <- confusion_mat[1, 2]
FN <- confusion_mat[2, 1]
accuracy <- (TP + TN) / (TP + TN + FP + FN)
recall <- TP / (TP + FN)
precision <- TP / (TP + FP)
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("Accuracy: ", accuracy, "\n")
cat("Recall: ", recall, "\n")
cat("Precision: ", precision, "\n")
cat("F1 Score: ", f1_score, "\n")
confusion_mat


#random forest -mmale


data_model_rf=train_data%>%filter(Sex=="M")%>%select(Ls,Age_coded,Diabetes,wthr,BMI)
random_forest = randomForest(Diabetes~., data=data_model_rf, ntree=100, importance=T)
importance(random_forest)
varImpPlot(random_forest)

pred_y=predict(random_forest,newdata = test_data_male)
confusion_mat=table(pred_y,test_data_male$Diabetes)
TP <- confusion_mat[1, 1]
TN <- confusion_mat[2, 2]
FP <- confusion_mat[1, 2]
FN <- confusion_mat[2, 1]
accuracy <- (TP + TN) / (TP + TN + FP + FN)
recall <- TP / (TP + FN)
precision <- TP / (TP + FP)
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("Accuracy: ", accuracy, "\n")
cat("Recall: ", recall, "\n")
cat("Precision: ", precision, "\n")
cat("F1 Score: ", f1_score, "\n")
confusion_mat


#roc curve

plot(roc_curve_1, main = "ROC Curve",col="blue",lty=2)
plot(roc_curve_2, main = "ROC Curve",col="lightblue",lty=3,add=TRUE)
plot(roc_curve_3, main = "ROC Curve",col="black",lty=1,add=TRUE)
plot(roc_curve_4, main = "ROC Curve",col="darkslategrey",lty=4,add=TRUE)
c1="diabetes on BMI,Age,Lifestyle on male"
c2="diabetes on BMI,Age,Lifestyle on female"
c3="diabetes on whtr,Age,Lifestyle on male"
c4="diabetes on whtr,Age,Lifestyle on female"
legend("bottomright",legend=c(c1,c2,c3,c4),col=c("darkblue","lightblue","black","darkslategrey"),
       lty=c(2,3,1,4))


#svm-1
data_model_svm1=train_data%>%filter(Sex=="F")%>%select(Ls,Age_coded,Diabetes,wthr,BMI)
data_model_svm2=train_data%>%filter(Sex=="F")%>%select(Ls,Age_coded,Diabetes,wthr)
data_model_svm3=train_data%>%filter(Sex=="F")%>%select(Ls,Age_coded,Diabetes,BMI)
tune.out <- tune(svm,Diabetes~., data = data_model_svm3, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

best_model=tune.out$best.model
summary(best_model)

pred_y=predict(best_model,test_data_female)
confusion_mat=table(pred_y,test_data_female$Diabetes)
TP <- confusion_mat[1, 1]
TN <- confusion_mat[2, 2]
FP <- confusion_mat[1, 2]
FN <- confusion_mat[2, 1]
accuracy <- (TP + TN) / (TP + TN + FP + FN)
recall <- TP / (TP + FN)
precision <- TP / (TP + FP)
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("Accuracy: ", accuracy, "\n")
cat("Recall: ", recall, "\n")
cat("Precision: ", precision, "\n")
cat("F1 Score: ", f1_score, "\n")
confusion_mat


#svm-2
#Males--BMI
data_model_svm2=train_data%>%filter(Sex=="M")%>%select(Ls,Age_coded,Diabetes,BMI)

tune.out <- tune(svm,Diabetes~., data = data_model_svm2, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

best_model=tune.out$best.model
summary(best_model)

pred_y=predict(best_model,test_data_male)
confusion_mat=table(pred_y,test_data_male$Diabetes)
TP <- confusion_mat[1, 1]
TN <- confusion_mat[2, 2]
FP <- confusion_mat[1, 2]
FN <- confusion_mat[2, 1]
accuracy <- (TP + TN) / (TP + TN + FP + FN)
recall <- TP / (TP + FN)
precision <- TP / (TP + FP)
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("Accuracy: ", accuracy, "\n")
cat("Recall: ", recall, "\n")
cat("Precision: ", precision, "\n")
cat("F1 Score: ", f1_score, "\n")
confusion_mat

#Males--Wthr
data_model_svm3=train_data%>%filter(Sex=="M")%>%select(Ls,Age_coded,Diabetes,wthr)

tune.out <- tune(svm,Diabetes~., data = data_model_svm3, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

best_model=tune.out$best.model
summary(best_model)

pred_y=predict(best_model,test_data_male)
confusion_mat=table(pred_y,test_data_male$Diabetes)
TP <- confusion_mat[1, 1]
TN <- confusion_mat[2, 2]
FP <- confusion_mat[1, 2]
FN <- confusion_mat[2, 1]
accuracy <- (TP + TN) / (TP + TN + FP + FN)
recall <- TP / (TP + FN)
precision <- TP / (TP + FP)
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("Accuracy: ", accuracy, "\n")
cat("Recall: ", recall, "\n")
cat("Precision: ", precision, "\n")
cat("F1 Score: ", f1_score, "\n")
confusion_mat


#Females--BMI
data_model_svm4=train_data%>%filter(Sex=="F")%>%select(Ls,Age_coded,Diabetes,BMI)

tune.out <- tune(svm,Diabetes~., data = data_model_svm4, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

best_model=tune.out$best.model
summary(best_model)

pred_y=predict(best_model,test_data_male)
confusion_mat=table(pred_y,test_data_male$Diabetes)
TP <- confusion_mat[1, 1]
TN <- confusion_mat[2, 2]
FP <- confusion_mat[1, 2]
FN <- confusion_mat[2, 1]
accuracy <- (TP + TN) / (TP + TN + FP + FN)
recall <- TP / (TP + FN)
precision <- TP / (TP + FP)
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("Accuracy: ", accuracy, "\n")
cat("Recall: ", recall, "\n")
cat("Precision: ", precision, "\n")
cat("F1 Score: ", f1_score, "\n")
confusion_mat

#Females--Whtr
data_model_svm5=train_data%>%filter(Sex=="F")%>%select(Ls,Age_coded,Diabetes,wthr)

tune.out <- tune(svm,Diabetes~., data = data_model_svm5, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

best_model=tune.out$best.model
summary(best_model)

pred_y=predict(best_model,test_data_male)
confusion_mat=table(pred_y,test_data_male$Diabetes)
TP <- confusion_mat[1, 1]
TN <- confusion_mat[2, 2]
FP <- confusion_mat[1, 2]
FN <- confusion_mat[2, 1]
accuracy <- (TP + TN) / (TP + TN + FP + FN)
recall <- TP / (TP + FN)
precision <- TP / (TP + FP)
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("Accuracy: ", accuracy, "\n")
cat("Recall: ", recall, "\n")
cat("Precision: ", precision, "\n")
cat("F1 Score: ", f1_score, "\n")
confusion_mat

