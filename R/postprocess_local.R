################################################################################
# Script: posprocess_local.R
# Authors: Ruben Urraca
# Comments: Generation of a dataframe with the preditions for 2013 of the 
# 4 optimized machine learning algorithms and the 2 parametric models. Models
# are trained with data from a unique station (local model)
################################################################################
rm(list=ls(all=TRUE))

# Workspace
home<-"/Users/rubenpazu"
setwd(paste0(home,"/Dropbox/softcomp/paper_enersol/"))
dir<-getwd()

# Packages
library(e1071)
library(elmNN)
library(rJava)
library(RWeka)
WPM("install-package","multiLayerPerceptrons")
WPM("package-info","installed","multiLayerPerceptrons")

################################################################################
# EXTRACT THE PARAMETERS OF THE OPTIMIZED MODELS FROM THE GENETIC ALGORITHMS FILES

tipo<-"local"

# Load the training database. Load the ranges in order to de-normalize the results. 
setwd(paste0(dir,"/data/"))
load(paste0(tipo,"_normalized.RData"))

# Create results dataframes
resultsGAlocal<-list()
modelo<-c("SVM", "M5P", "ELM", "MLPReg"); 

for (i in 1:length(modelo)){
	aux<-NULL
	for (j in 1:4){
		#extract the paramters and inputs chosen (best individual & last generation)
		setwd(paste0(dir,"/R/_models/",tipo,"_",modelo[[i]],"/RESULTS_",tipo,"_train_",j,"_",modelo[[i]]))
		data<-read.csv2(file="gen_FINAL.csv",  dec=".", sep=",", stringsAsFactors=FALSE)
		#best individual (first row). Obtain parameters.
		best<-data[nrow(data),]
		best<-best[-c(1,2,4,5)]
		#
		best$Cost1<-best$Cost1*(ranges.train[[j]][2,14]-ranges.train[[j]][1,14])
		best$CostTest<-best$CostTest*(ranges.train[[j]][2,14]-ranges.train[[j]][1,14])
		aux<-rbind(aux,best)
	}
	resultsGAlocal[[i]]<-aux
}
names(resultsGAlocal)<-modelo

################################################################################
# GENERATE THE PREDICTIONS OF EACH MODEL (MACHINE LEARNING AND PARAMETRICS) PER STATION
stations<-1:4
predictions.test<-list()

for (i in 1:4){
	pred.test<-as.data.frame(matrix(0, ncol=7, nrow=nrow(meteo.test[[i]])))
	colnames(pred.test)<-c("SVM","M5P","ELM","MLPReg","LS","Anto", "real")
	#
	DATATRAIN<-meteo.train[[i]]
	DATATEST<-meteo.test[[i]]
	# SVM
	inputs<-resultsGAlocal$SVM[i,which(names(resultsGAlocal$SVM)=="TempMed"):which(names(resultsGAlocal$SVM)=="M_mas1")]
	model<-svm(x=DATATRAIN[,which(inputs==1)], y=DATATRAIN[,14], kernel="radial", gamma=resultsGAlocal$SVM$Gamma[i], cost=10^(resultsGAlocal$SVM$LogCost[i]), epsilon=resultsGAlocal$SVM$Epsilon[i])
	predtest<-predict(model, newdata=DATATEST[,which(inputs==1)])
	pred.test$SVM<-(predtest*(ranges.train[[i]][2,14]-ranges.train[[i]][1,14]))+ranges.train[[i]][1,14]
	# M5P
	inputs<-resultsGAlocal$M5P[i,which(names(resultsGAlocal$M5P)=="TempMed"):which(names(resultsGAlocal$M5P)=="M_mas1")]
	model<-M5P(as.formula(paste("Rad", paste(names(inputs)[inputs==1], collapse=" + "), sep=" ~ ")),
		   data=DATATRAIN,control=Weka_control(M=as.numeric(resultsGAlocal$M5P$M_par[i])))	
	predtest<-predict(model, newdata=DATATEST[,which(inputs==1)])
	pred.test$M5P<-(predtest*(ranges.train[[i]][2,14]-ranges.train[[i]][1,14]))+ranges.train[[i]][1,14]
	# ELM
	inputs<-resultsGAlocal$ELM[i,which(names(resultsGAlocal$ELM)=="TempMed"):which(names(resultsGAlocal$ELM)=="M_mas1")]
	model<-elmtrain(x=DATATRAIN[,which(inputs==1)], y=DATATRAIN[,14], nhid<-resultsGAlocal$ELM$N[i], actfun="sig", C=resultsGAlocal$ELM$Cost[i])
	predtest<-as.numeric(predict(model, newdata=DATATEST[,which(inputs==1)]))
	pred.test$ELM<-(predtest*(ranges.train[[i]][2,14]-ranges.train[[i]][1,14]))+ranges.train[[i]][1,14]
	# MLPReg
	inputs<-resultsGAlocal$MLPReg[i,which(names(resultsGAlocal$MLPReg)=="TempMed"):which(names(resultsGAlocal$MLPReg)=="M_mas1")]
	MLPREG <- make_Weka_classifier("weka.classifiers.functions.MLPRegressor");
	model<-MLPREG(as.formula(paste("Rad", paste(names(inputs)[inputs==1], collapse=" + "), sep=" ~ ")),
		      data=DATATRAIN, control=Weka_control(N=resultsGAlocal$MLPReg$Neurons[i],R=resultsGAlocal$MLPReg$Ridge[i]))
	predtest<-as.numeric(predict(model, newdata=DATATEST[,which(inputs==1)]))
	pred.test$MLPReg<-(predtest*(ranges.train[[i]][2,14]-ranges.train[[i]][1,14]))+ranges.train[[i]][1,14]
	# Liu-Scott 2011 
	setwd(paste0(dir,"/R/_models/local_parametrics"))
	load("LS_local.RData")
	pred.test$LS<-LS_local[[i]]
	# Antonanzas 2011 
	setwd(paste0(dir,"/R/_models/local_parametrics"))
	load("AN_local.RData")
	pred.test$Anto<-AN_local[[i]]
	# Real
	pred.test$real<-(DATATEST[,14]*(ranges.train[[i]][2,14]-ranges.train[[i]][1,14]))+ranges.train[[i]][1,14]
  
  predictions.test[[i]]<-pred.test
}
predictions.local<-predictions.test

# Save parameters and predictions
setwd(paste0(dir,"/output"))
save(resultsGAlocal, predictions.local, file="test2013local.RData")


