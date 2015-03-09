################################################################################
# Script: posprocess_gen.R
# Authors: Ruben Urraca
# Comments: Generation of a dataframe with the preditions for 2013 of the 
# 4 optimized machine learning algorithms and the 2 parametric models. Models
# are trained with data from every station (general model)
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
tipo<-"general"

# Load the training database. Load the ranges in order to de-normalize the results. 
setwd(paste0(dir,"/data/"))
load(paste0(tipo,"_normalized.RData"))
range.total<-(ranges.train[2,14]-ranges.train[1,14])

# Create results dataframes
resultsGA<-list()
modelo<-c("SVM", "M5P", "ELM", "MLPReg"); 

sep<-c(",",";",",",",")
for (i in 1:length(modelo)){
#extract the paramters and inputs chosen (best individual & last generation)
setwd(paste0(dir,"/R/_models/",tipo,"_",modelo[[i]],"/RESULTS_",tipo,"_train_",modelo[[i]]))
data<-read.csv2(file="gen_FINAL.csv", sep=sep[i], dec=".")
#best individual (first row). Obtain parameters.
best<-data[nrow(data),]
resultsGA[[i]]<-best[-c(1,2,4,5)]
#
resultsGA[[i]]$Cost1<-resultsGA[[i]]$Cost1*(ranges.train[2,14]-ranges.train[1,14])
resultsGA[[i]]$CostTest<-resultsGA[[i]]$CostTest*(ranges.train[2,14]-ranges.train[1,14])
names(resultsGA)[i]<-modelo[[i]]
}


#################################################################################
# GENERATE THE PREDICTIONS OF EACH MODEL (MACHINE LEARNING AND PARAMETRICS) PER STATION

# Create the results dataframe
predictions<-as.data.frame(matrix(0,ncol=9,nrow=nrow(meteo.test)))
colnames(predictions)<-c("SVM","M5P", "ELM", "MLPreg", "LS", "Anto", "real", "station", "clearIndex")

# Train and test dataframes
DATATRAIN<-meteo.train; DATATEST<-meteo.test

# SVM
inputs<-resultsGA$SVM[which(names(resultsGA$SVM)=="TempMed"):which(names(resultsGA$SVM)=="M_mas1")]
model<-svm(x=DATATRAIN[,which(inputs==1)], y=DATATRAIN[,14], kernel="radial", gamma=resultsGA$SVM$Gamma, cost=10^(resultsGA$SVM$LogCost), epsilon=resultsGA$SVM$Epsilon)
predtest<-predict(model, newdata=DATATEST[,which(inputs==1)])
predictions$SVM<-(predtest*range.total)+ranges.train[1,14]
# M5P
inputs<-resultsGA$M5P[which(names(resultsGA$M5P)=="TempMed"):which(names(resultsGA$M5P)=="M_mas1")]
model<-M5P(as.formula(paste("Rad", paste(names(inputs)[inputs==1], collapse=" + "), sep=" ~ ")),
			data=DATATRAIN,control=Weka_control(M=as.numeric(resultsGA$M5P$M_par)))
predtest<-predict(model, newdata=DATATEST[,which(inputs==1)])
predictions$M5P<-(predtest*range.total)+ranges.train[1,14]
# ELM
inputs<-resultsGA$ELM[which(names(resultsGA$ELM)=="TempMed"):which(names(resultsGA$ELM)=="M_mas1")]
model<-elmtrain(x=DATATRAIN[,which(inputs==1)], y=DATATRAIN[,14], nhid<-resultsGA$ELM$N, actfun="sig", C=resultsGA$ELM$Cost)
predtest<-as.numeric(predict(model, newdata=DATATEST[,which(inputs==1)]))
predictions$ELM<-(predtest*range.total)+ranges.train[1,14]
# MLP
inputs<-resultsGA$MLPReg[which(names(resultsGA$MLPReg)=="TempMed"):which(names(resultsGA$MLPReg)=="M_mas1")]
MLPREG <- make_Weka_classifier("weka.classifiers.functions.MLPRegressor");
model<-MLPREG(as.formula(paste("Rad", paste(names(inputs)[inputs==1], collapse=" + "), sep=" ~ ")),
	      data=DATATRAIN, control=Weka_control(N=resultsGA$MLPReg$Neurons,R=resultsGA$MLPReg$Ridge))
predtest<-as.numeric(predict(model, newdata=DATATEST[,which(inputs==1)]))
predictions$MLPreg<-(predtest*range.total)+ranges.train[1,14]
# Liu-Scott 2011 
setwd(paste0(dir,"/R/_models/general_parametrics"))
load("LS.RData")
predictions$LS<-do.call(c,LS)
# Antonanzas 2011 
setwd(paste0(dir,"/R/_models/general_parametrics"))
load("AN.RData")
predictions$Anto<-do.call(c,AN)
# Real values (measured radiation de-normalized)
predictions$real<-(DATATEST[,14]*range.total)+ranges.train[1,14]
# Station (we add a last column to identify to which station belongs each radiation value)
predictions$station<-st_test
# Clear sky index (0-cloudy day, 1-clear day)
predictions$clearIndex<-clearIndex_test

# Save results
setwd(paste0(dir,"/output/"))
save(resultsGA, predictions, file="test2013.RData")


