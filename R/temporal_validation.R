################################################################################
# Script: temporal_validation.R
# Authors: Ruben Urraca
# Comments: Evaluation of the generalization capacity of the models using 
# different training and testing periods. From the available time series 
# (2009-2013), models are trained with data from aunique year and then tested 
# in the restant period. 
################################################################################
rm(list=ls(all=TRUE))

# Workspace
home<-"/Users/rubenpazu"
setwd(paste0(home,"/Dropbox/softcomp/paper_enersol/"))
dir<-getwd()

# Packages
library(e1071)
library(RWeka)
library(elmNN)

# Load data (already normalized an split into train-test)
setwd(paste0(dir,"/data/"))
load("year_normalized.RData")

# Load model parameters and sets of features seleced for each techique
# the parameters of the GENERAL models are used
setwd(paste0(dir,"/output"))
load("test2013.RData")

# Create a dataframe to save the results
year_tr<-c(2009,2010,2011,2012,2013)
predictions.test<-list(); predictions.train<-list(); 

for (i in 1:length(year_tr)){
	pred.test<-as.data.frame(matrix(0, ncol=5, nrow=nrow(meteo.test[[i]])))
	colnames(pred.test)<-c("SVM","M5P","ELM","MLPReg", "real")
	pred.train<-as.data.frame(matrix(0, ncol=5, nrow=nrow(meteo.train[[i]])))
	colnames(pred.train)<-c("SVM","M5P","ELM","MLPReg", "real")
	#
	DATATRAIN<-meteo.train[[i]]
	DATATEST<-meteo.test[[i]]
	# SVM
	inputs<-resultsGA$SVM[which(names(resultsGA$SVM)=="TempMed"):which(names(resultsGA$SVM)=="M_mas1")]
	model<-svm(x=DATATRAIN[,which(inputs==1)], y=DATATRAIN[,14], kernel="radial", gamma=resultsGA$SVM$Gamma, cost=10^(resultsGA$SVM$LogCost), epsilon=resultsGA$SVM$Epsilon)
	predtest<-predict(model, newdata=DATATEST[,which(inputs==1)])
	predtrain<-predict(model, newdata=DATATRAIN[,which(inputs==1)])
	pred.test$SVM<-(predtest*(ranges.train[[i]][2,14]-ranges.train[[i]][1,14]))+ranges.train[[i]][1,14]
	pred.train$SVM<-(predtrain*(ranges.train[[i]][2,14]-ranges.train[[i]][1,14]))+ranges.train[[i]][1,14]
	# M5P
	inputs<-resultsGA$M5P[which(names(resultsGA$M5P)=="TempMed"):which(names(resultsGA$M5P)=="M_mas1")]
	model<-M5P(as.formula(paste("Rad", paste(names(inputs)[inputs==1], collapse=" + "), sep=" ~ ")),
		   data=DATATRAIN,control=Weka_control(M=as.numeric(resultsGA$M5P$M_par)))	
	predtest<-predict(model, newdata=DATATEST[,which(inputs==1)])
	predtrain<-predict(model, newdata=DATATRAIN[,which(inputs==1)])
	pred.test$M5P<-(predtest*(ranges.train[[i]][2,14]-ranges.train[[i]][1,14]))+ranges.train[[i]][1,14]
	pred.train$M5P<-(predtrain*(ranges.train[[i]][2,14]-ranges.train[[i]][1,14]))+ranges.train[[i]][1,14]
	# ELM
	inputs<-resultsGA$ELM[which(names(resultsGA$ELM)=="TempMed"):which(names(resultsGA$ELM)=="M_mas1")]
	model<-elmtrain(x=DATATRAIN[,which(inputs==1)], y=DATATRAIN[,14], nhid<-resultsGA$ELM$N, actfun="sig", C=resultsGA$ELM$Cost)
	predtest<-as.numeric(predict(model, newdata=DATATEST[,which(inputs==1)]))
	predtrain<-as.numeric(predict(model, newdata=DATATRAIN[,which(inputs==1)]))
	pred.test$ELM<-(predtest*(ranges.train[[i]][2,14]-ranges.train[[i]][1,14]))+ranges.train[[i]][1,14]
	pred.train$ELM<-(predtrain*(ranges.train[[i]][2,14]-ranges.train[[i]][1,14]))+ranges.train[[i]][1,14]
	# MLPReg
	inputs<-resultsGA$MLPReg[which(names(resultsGA$MLPReg)=="TempMed"):which(names(resultsGA$MLPReg)=="M_mas1")]
	MLPREG <- make_Weka_classifier("weka.classifiers.functions.MLPRegressor");
	model<-MLPREG(as.formula(paste("Rad", paste(names(inputs)[inputs==1], collapse=" + "), sep=" ~ ")),
		      data=DATATRAIN, control=Weka_control(N=resultsGA$MLPReg$Neurons,R=resultsGA$MLPReg$Ridge))
	predtest<-as.numeric(predict(model, newdata=DATATEST[,which(inputs==1)]))
	predtrain<-as.numeric(predict(model, newdata=DATATRAIN[,which(inputs==1)]))
	pred.test$MLPReg<-(predtest*(ranges.train[[i]][2,14]-ranges.train[[i]][1,14]))+ranges.train[[i]][1,14]
	pred.train$MLPReg<-(predtrain*(ranges.train[[i]][2,14]-ranges.train[[i]][1,14]))+ranges.train[[i]][1,14]
	# real
	pred.test$real<-(DATATEST[,14]*(ranges.train[[i]][2,14]-ranges.train[[i]][1,14]))+ranges.train[[i]][1,14]
	pred.train$real<-(DATATRAIN[,14]*(ranges.train[[i]][2,14]-ranges.train[[i]][1,14]))+ranges.train[[i]][1,14]
	#
	predictions.test[[i]]<-pred.test; 
	predictions.train[[i]]<-pred.test; 
}
names(predictions.test)<-year_tr; names(predictions.train)<-year_tr; 

# Save results
setwd(paste0(dir,"/output"))
save(predictions.test, predictions.train, file="testYear.RData")
