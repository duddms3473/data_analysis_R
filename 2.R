#1번
cb<-read.csv("CommonBank.csv")
cb<-cb[-c(1,5)]
cb$PersonalLoan<-factor(cb$PersonalLoan,levels=c(0,1),labels=c("Reject","Accept"))
normalize<-function(x){ return((x-min(x))/(max(x)-min(x)))}
cb_n<-as.data.frame(lapply(cb[-8],normalize))
cb_train<-cb_n[1:4000,]
cb_test<-cb_n[4001:5000,]
cb_train_labels<-cb[1:4000,8]
cb_test_labels<-cb[4001:5000,8]
table(cb_train_labels)
table(cb_test_labels)

#2번
library(class)
cb_test_pred<-knn(train=cb_train,test=cb_test,cl=cb_train_labels,k=7)
install.packages("gmodels")
library(gmodels)
CrossTable(x=cb_test_labels,y=cb_test_pred,prop.chisq=FALSE)


#3번

Accuracy<-vector()
Sensitivity<-vector()
Specificity<-vector()
for(k in seq(1,99,2)){
  cb_test_pred<-knn(train=cb_train, test=cb_test,cl=cb_train_labels,k=k)
  cm<-confusionMatrix(cb_test_pred,cb_test_labels,positive="Accept")
  Accuracy<-c(Accuracy,cm$overall[1])
  Sensitivity<-c(Sensitivity,cm$byClass[1])
  Specificity<-c(Specificity,cm$byClass[2])
}
plot_data<-data.frame(
  k=seq(1,99,2),
  value=c(Accuracy,Sensitivity,Specificity),
  type=c(rep("Accuracy",50),rep("Sensitivity",50),rep("Specificity",50))
)
ggplot(plot_data,aes(x=k,y=value,color=type))+geom_point()+geom_line()+theme_bw()+labs(title="k-nn (k=1~99, 홀수)")


#4번
library(caret)
cb_train<-cb[1:4000,]
cb_test<-cb[4001:5000,]
z_normalized<-c("center","scale")
cv<-trainControl(method="repeatedcv",number=5,repeats=5)
tune_grid<-expand.grid(k=seq(1,99,2))

knn_fit<-train(data=cb_train,PersonalLoan~.,method="knn",trControl=cv,preProcess=z_normalized,tuneGrid=tune_grid)
library(ggplot2)
ggplot(knn_fit)+theme_bw()
knn_pred<-extractPrediction(list(knn_fit),testX=cb_test[,-8],testY=cb_test_labels)
test_pred<-knn_pred$pred[knn_pred$dataType=="Test"]
test_obs<-knn_pred$obs[knn_pred$dataType=="Test"]
confusionMatrix(test_pred,test_obs,positive="Accept")
