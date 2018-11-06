#using UCI breast cancer data from https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Coimbra

#PCA
library("caTools")
dataset=read.csv("breastCancer_UCI.csv")


split=sample.split(dataset$Classification,SplitRatio = 0.8)

training=subset(dataset,split==TRUE)
test=subset(dataset,split==FALSE)

#feature scaling

training[-10]=scale(training[-10])
test[-10]=scale(test[-10])

#applying PCA

#install.packages("caret",dependencies = TRUE)
library(caret)
#install.packages("e1071",dependencies = TRUE)
library("e1071")

pca=preProcess(x=training[-10],method = "pca",pcaComp = 2)
training_pca=predict(pca,training)
training_pca=training_pca[c(2,3,1)]
test_pca=predict(pca,test)
test_pca=test_pca[c(2,3,1)]


#Fitting SVM to training set

classifier=svm(formula=Classification ~.,
              data=training_pca,
              type="C-classification",
              kernel="linear"
              
              
              
              )



#predicting test data using new model

y_pred=predict(classifier,newdata = test_pca[-3])

#Confusion matrix
cm=table(test_pca[,3],y_pred) #14 predicted to be in 1 but in actuality there are 10. 9 predicted to be 2 but in real it's 13

#Visualizing the training set results

#install.packages("ElemStatLearn")
library("ElemStatLearn")
set=training_pca
X1=seq(min(set[,1]-1),max(set[,1])+1,by=0.01)
X2=seq(min(set[,2])-1,max(set[,2])+1,by=0.01)
grid_set=expand.grid(X1,X2)
colnames(grid_set)=c("PC1","PC2")
y_grid=predict(classifier,newdata=grid_set)

plot(set[,-3],
     
     main="SVM(training set)",
     xlab = "PC1",ylab = "PC2",
     xlim=range(X1),ylim=range(X2),
     contour(X1,X2,matrix(as.numeric(y_grid),length(X1),length(X2)),add=TRUE),
     points(grid_set,pch=".",col=ifelse(y_grid==1,'springgreen3','tomato')),
     points(set,pch=21,bg=ifelse(set[,3]==1,'green4','red3')),
     log = ""
     )


#Visualizing test set

set=test_pca
X1=seq(min(set[,1]-1),max(set[,1])+1,by=0.01)
X2=seq(min(set[,2])-1,max(set[,2])+1,by=0.01)
grid_set=expand.grid(X1,X2)
colnames(grid_set)=c("PC1","PC2")
y_grid=predict(classifier,newdata=grid_set)

plot(set[,-3],
     
     main="SVM(training set)",
     xlab = "PC1",ylab = "PC2",
     xlim=range(X1),ylim=range(X2),
     contour(X1,X2,matrix(as.numeric(y_grid),length(X1),length(X2)),add=TRUE),
     points(grid_set,pch=".",col=ifelse(y_grid==1,'springgreen3','tomato')),
     points(set,pch=21,bg=ifelse(set[,3]==1,'green4','red3')),
     log = ""
)





