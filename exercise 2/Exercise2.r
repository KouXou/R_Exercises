library('dplyr')
library('ROCR')

res<-matrix(c(1,TRUE,0.73,0.61,
            2,TRUE,0.69,0.03,
            3,FALSE,0.44,0.68,
            4,FALSE,0.55,0.31,
            5,TRUE,0.67,0.45,
            6,TRUE,0.47,0.09,
            7,FALSE,0.08,0.38,
            8,FALSE,0.15,0.05,
            9,TRUE,0.45,0.01,
            10,FALSE,0.35,0.04),ncol=4,byrow=TRUE)
colnames(res)<-c("Instance","True_Class","PM1","PM2")

resdf<-as.data.frame(res)
resdf<-mutate(resdf,PM11=1-PM1,PM22=1-PM2)
resdf

# i
eval1 <-prediction(resdf$PM1, resdf$True_Class)
eval2 <-prediction(resdf$PM2, resdf$True_Class)
plot(performance(eval1,"tpr","fpr"), col="green")
plot(performance(eval2,"tpr","fpr"),add=TRUE, col="blue")
print(paste('AUC1 : ',attributes(performance(eval1,'auc'))$y.values[[1]]))
print(paste('AUC2 : ',attributes(performance(eval2,'auc'))$y.values[[1]]))
# M1 > M2

# ii
df1<-resdf[order(-resdf$PM1),]
df1

confusion_matrix<-table(truth=resdf$True_Class,prediction=resdf$PM1>0.5)
print('Πίνακας Κόστους')
confusion_matrix

TN<-confusion_matrix[1,1] # TRUE NEGATIVE
TP<-confusion_matrix[2,2] # TRUE POSITIVE
FN<-confusion_matrix[2,1] # FALSE NEGATIVE
FP<-confusion_matrix[1,2] # FALSE POSITIVE

#ACCURACY
accuracy<-(TP+TN)/sum(confusion_matrix)

#PRECISION
precision<-TP/(TP+FP)

#RECALL
recall<-TP/(TP+FN)

# F1 Score
f1_score<-(2*precision*recall)/(precision+recall)

print(paste('Ακρίβεια : ',accuracy))
print(paste('Ανάκληση : ',recall))
print(paste('Μέτρο F : ',f1_score))

#iii
df2<-resdf[order(-resdf$PM2),]
df2

confusion_matrix2<-table(truth=resdf$True_Class,prediction=resdf$PM2>0.5)
confusion_matrix2

TN<-confusion_matrix2[1,1] # TRUE NEGATIVE
TP<-confusion_matrix2[2,2] # TRUE POSITIVE
FN<-confusion_matrix2[2,1] # FALSE NEGATIVE
FP<-confusion_matrix2[1,2] # FALSE POSITIVE

#ACCURACY
accuracy<-(TP+TN)/sum(confusion_matrix)

#PRECISION
precision<-TP/(TP+FP)

#RECALL
recall<-TP/(TP+FN)

# F1 Score
f1_score<-(2*precision*recall)/(precision+recall)

print(paste('Ακρίβεια : ',accuracy))
print(paste('Ανάκληση : ',recall))
print(paste('Μέτρο F : ',f1_score))

#iv
confusion_matrix3<-table(truth=resdf$True_Class,prediction=resdf$PM1>0.1)
confusion_matrix3

TN<-confusion_matrix3[1,1] # TRUE NEGATIVE
TP<-confusion_matrix3[2,2] # TRUE POSITIVE
FN<-confusion_matrix3[2,1] # FALSE NEGATIVE
FP<-confusion_matrix3[1,2] # FALSE POSITIVE
#ACCURACY
accuracy<-(TP+TN)/sum(confusion_matrix)

#PRECISION
precision<-TP/(TP+FP)

#RECALL
recall<-TP/(TP+FN)

# F1 Score
f1_score<-(2*precision*recall)/(precision+recall)

print(paste('Ακρίβεια : ',accuracy))
print(paste('Ανάκληση : ',recall))
print(paste('Μέτρο F : ',f1_score))
