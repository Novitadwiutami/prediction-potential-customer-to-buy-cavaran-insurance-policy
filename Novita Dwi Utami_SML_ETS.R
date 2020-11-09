#Regresi logistik
#Read datasets
library(nnet)
setwd("E:/ITS/MATERI KULIAH/SEMESTER 7/STATISTICS MACHINE LEARNING/ETS SML Data/Novita Dwi Utami_SML_ETS")
data <- read.csv("soal ets sml.csv")
View(data)
colnames(data)
regresi_logistik<-multinom(X86 ~., data=data)
summary(regresi_logistik)
regresi_logistik
pred<-predict(regresi_logistik)
pred
target<-data[,86]
tbl.clas<-table(pred, data[,86]); tbl.clas
akurasi=mean(pred==data[,86])
akurasi

#-------------------------------------------------------------------------
model <- glm(X86 ~.,family=binomial(link='logit'),data=data)
summary(model)
fitted.results <- predict(model,data,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != data$X86)
print(paste('Accuracy',1-misClasificError))


#--------------------------------------------------------------------



#-----------------------------------------------------------------

# train testing
colnames(data)
n=nrow(data); n
n.train=floor(n*0.8); n.train
n.test=n-n.train; n.test
target=as.factor(data$X86)
train.index=sample(seq(n), size=n.train)
data.train=data[train.index,];head(data.train, 10); dim(data.train)
data.test=data[-train.index,];head(data.test, 10);  dim(data.test)
target.train=target[train.index]
target.test=target[-train.index]

#---------------------------------------------------------
model <- glm(X86 ~.,family=binomial(link='logit'),data=data.train)
summary(model)
fitted.results <- predict(model,data,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != data$X86)
print(paste('Accuracy',1-misClasificError))
tbl.clas.test=table(fitted.results, target.train)
akurasi.test = mean(fitted.results==target.train)
akurasi.test

#-----------------------------------------------------

reglog<-multinom(X86~., data=data.train)
summary(reglog)

pred.train= predict(reglog)
tbl.clas.train=table(pred.train, target.train); tbl.clas.train
akurasi.train=mean(pred.train==target.train)
akurasi.train

pred.test= predict(reglog, newdata=data.test)
tbl.clas.test=table(pred.test, target.test); tbl.clas.test
akurasi.test = mean(pred.test==target.test)
akurasi.test
