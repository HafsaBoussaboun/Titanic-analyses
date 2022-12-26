data_frame<-read.csv("titanic.csv",na.strings = "")
View(data_frame)
summary(data_frame)
dim(data_frame)
str(data_frame)
table(data_frame$Survived) ## nombre
prop.table(table(data_frame$Survived)) ## pourcentage
prop.table(table(data_frame$Sex,data_frame$Survived),margin = 1)
colSums(is.na(data_frame))
install.packages('psych')
install.packages('GGally')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('rpart')
install.packages('rpart.plot')
install.packages('Amelia')

#clean and prepare
library(Amelia)
##
library(dplyr)
data_frame=select(data_frame,Survived,Pclass,Parch,SibSp,Sex,Age)
View(data_frame)
missmap(data_frame,col = c("gray","black"))
data_frame=na.omit(data_frame)
missmap(data_frame,col = c("gray","black"))


library(GGally)
##data_frame$Survived=factor()
ggcorr(data_frame,nbreaks = 6,label = TRUE,label_size = 3,color="grey50")
data_frame$Survived=factor(data_frame$Survived)
data_frame$Pclass=factor(data_frame$Pclass,order=TRUE,levels = c(3,2,1))
str(data_frame)
ggcorr(data_frame,nbreaks = 6,label = TRUE,label_size = 3,color="grey50")
str(data_frame)
##create train/test set
create_train_test<-function(data,size=0.8,train=TRUE){
  n_row=nrow(data)
  total_row=size*n_row
  train_sample=1:total_row
  if(train==TRUE){
    return(data[train_sample,])
  }else{
    return(data[-train_sample,])
  }
}
View(data_frame)
train<-create_train_test(data_frame,0.8,train = TRUE)
test<-create_train_test(data_frame,0.8,train=FALSE)
View(train)
dim(train)

##survived count
library(ggplot2)
ggplot(train,aes(x=Survived))+
  geom_bar(width = 0.5,fill="coral")+
  geom_text(stat = "count",aes(label=stat(count)),vjust=0.5)+
  theme_classic()

##survived count by gender
ggplot(train,aes(x=Survived,fill=Sex))+
  geom_bar(position=position_dodge())+
  geom_text(stat = "count",
            aes(label=stat(count)),
            position = position_dodge(width = 1))+
            theme_classic()


##survived count by class

ggplot(train,aes(x=Survived,fill=Pclass))+
  geom_bar(position=position_dodge())+
  geom_text(stat = "count",
            aes(label=stat(count)),
            position = position_dodge(width = 1))+
            theme_classic()

#Age density
ggplot(train,aes(x=Age)
       +geom_density(fill="coral"))

##
view(train)
train$Discretize.age=cut(train$Age,c(0,20,30,40,50,60,70,80,90,100))
train$Discretize.age

#plot Discretize.age
##on ajoute une autre colonne pour les classes d'age
ggplot(train,aes(x=Discretize.age,fill=Survived))+
  geom_bar(position=position_dodge())+
  geom_text(stat = "count",
            aes(label=stat(count)),
            vjust=-0.5,
            position = position_dodge(width = 1))+
  theme_classic()

  train$Discretize.age=NULL #on suprime la colonne qu'on a ajouter
  
##partie de machine learning dicision tree
  library(rpart)
  my_dt<-rpart(Survived~.,
               data=train,
               method="class")
  install.packages("rattle")
  install.packages("RColorBrewer")
  install.packages("rpart.plot")
  
  library(rattle)
  library(rpart.plot)
  library(RColorBrewer)
  
  fancyRpartPlot(my_dt)
  round(prop.table(table(train$Survived)),2)
  round(prop.table(table(train$Sex,train$Survived),margin = 1),2)
  
  predicted=predict(my_dt,test,type = "class")
  table_mat=table(test$Survived,predicted)
  dt_accuracy=sum(diag(table_mat))/sum(table_mat)
  paste("the occuaracy is",dt_accuracy)
  print(table_mat)
  
  perfermance_tune<-function(fit){
    predict<-predict(fit,test,type="class")
    table_mat<-table(test$Survived,predict)
    accuracy_test<-sum(diag(table_mat))/sum(table_mat)
    print(accuracy_test)
  }
  control<-rpart.control(minsplit = 5,
                         minbucket = round(7/3),
                         maxdepth = 4,
                         cp=0)
  tune_dt<-rpart(Survived~.,data=train,method = "class",control = control)
  perfermance_tune(tune_dt)
  view(train)
  
  new_test<-read.csv("test.csv")
  new_test=na.omit(new_test)
  missmap(new_test,col = c('black',"grey"))
  new_test$Pclass=factor(new_test$Pclass,order=TRUE,levels = c(3,2,1))
  str(new_test)
  new_predition<-predict(tune_dt,new_test,typr="class")
  submit<-data.frame(PassengerId=new_test$PassengerId,Survived=new_predition$Survuved)
  write.csv(submit,file = "newTestCsv.csv",row.names = FALSE)
  view(submit)
  
  