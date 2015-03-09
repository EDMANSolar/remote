################################################################################
# Script: tables.R
# Authors: Ruben Urraca
# Comments: Generation of the tables for the paper. Tables are further edited
# in the latex code with the multicolumn package
################################################################################
rm(list=ls(all=TRUE))

# Workspace
home<-"/Users/rubenpazu"
setwd(paste0(home,"/Dropbox/softcomp/paper_enersol/"))
dir<-getwd()

# Packages
library(xtable)

# Functions
MAE<-function(pred, real){mean(abs(pred-real))}
RMSE<-function(pred, real){sqrt(sum((pred-real)^2)/length(pred-real))}
RSQUARED<-function(pred, real){1-sum(((pred-real)-mean(pred-real))^2)/sum((real-mean(real))^2)}

################################################################################
#TABLE 2 - GENETIC ALGORITHMS RESULTS
################################################################################

# Load predictions (training period 2009-2012; testing 2013) 
setwd(paste0(dir,"/output"))
load("test2013.RData")
load("test2013local.RData")

# Rearrange the GA dataframe
GA<-list()
GA<-lapply(1:length(resultsGA), function(i){
	GA[[i]]<-rbind(resultsGAlocal[[i]], resultsGA[[i]])
})
names(GA)<-names(resultsGAlocal)
# Eliminate unnecesary columns
for(i in 1:4){
	columns<-c(which(colnames(GA[[i]])==("MinutesAll")),
		   which(colnames(GA[[i]])==("Cost1")),
		   which(colnames(GA[[i]])==("Size1")),
		   which(colnames(GA[[i]])==("CostTest")))
	GA[[i]]<-GA[[i]][,-columns]
}
# Reoder
for (i in 1:4){
	newOrder<-c(which(colnames(GA[[i]])==("NumFeatures")),
		    which(colnames(GA[[i]])==("TempMed")):which(colnames(GA[[i]])==("M_mas1")),
		    1:(which(colnames(GA[[i]])==("TempMed"))-1))
	GA[[i]]<-GA[[i]][,newOrder]
}

# Print xtable
for(i in 1:4){
	table<-xtable(GA[[i]])
	digits(table)<-c(rep(0,15),rep(2,(ncol(GA[[i]])-14)))
	print(table)
}
	


###########################################################################################################################
#TABLE 3 MAEs and RMSEs of each model per station (training 2009-2012, testing 2013)
###########################################################################################################################

# Load predictions (training period 2009-2012; testing 2013) 
setwd(paste0(dir,"/output"))
load("test2013.RData")
load("test2013local.RData")

# Create the dataframes for MAEs and RMSEs
MAEgeneral<-as.data.frame(matrix(0,ncol=6,nrow=5))
rownames(MAEgeneral)<-c("Cordoba","Jaen","Puebla", "Nijar", "mean")
colnames(MAEgeneral)<-c("SVM","M5P", "ELM","MLPreg","LS", "Anto" )
MAElocal<-MAEgeneral; RMSElocal<-MAEgeneral; RMSEgeneral<-MAEgeneral

samples<-sapply(predictions.local, nrow)
for(i in 1:6){
	#error per station
	for(j in 1:4){
		MAEgeneral[j,i]<-MAE(predictions[which(predictions$station==j),i], predictions$real[which(predictions$station==j)])
		RMSEgeneral[j,i]<-RMSE(predictions[which(predictions$station==j),i], predictions$real[which(predictions$station==j)])
    MAElocal[j,i]<-MAE(predictions.local[[j]][,i],predictions.local[[j]]$real)
		RMSElocal[j,i]<-RMSE(predictions.local[[j]][,i],predictions.local[[j]]$real)
	}
	#error in all stations together
	MAEgeneral[5,i]<-MAE(predictions[,i], predictions$real)
	RMSEgeneral[5,i]<-RMSE(predictions[,i], predictions$real)
	MAElocal[5,i]<-sum(MAElocal[1:4,i]*samples)/sum(samples)
	RMSElocal[5,i]<-sum(RMSElocal[1:4,i]*samples)/sum(samples)
}

mergeByRow<-function(df1, df2){
  #function to merge two dataframes alternating rows
	df3<-NULL
	for(i in 1:nrow(df1)){
		df3<-rbind(df3,df1[i,],df2[i,])
	}
	return(df3)
}
errorsGENERAL<-list(MAE=mergeByRow(MAEgeneral,MAElocal), RMSE=mergeByRow(RMSEgeneral,RMSElocal))

# Print xtable
table1<-do.call(cbind, errorsGENERAL)
table<-xtable(table1,  caption="R2, MAE and RMSE testing errors (MJ/m2day) for the general models in the different locations")
align(table)<-c("l",rep ("c",ncol(table)))
digits(table)<-2
print(table, hline.after=c(-1,0,10), table.placement="H", scale=0.8,sanitize.text.function=function(x){x})


###########################################################################################################################
#TABLE 4: MAEs obtain training the algorithms in a unique year and testing in the rest
############################################################################################################################

# Load predictions obtained using one year for training and the rest for testing
setwd(paste0(dir,"/output"))
load("testYear.RData")
# Load data
setwd(paste0(dir,"/data/"))
load("year_normalized.RData")

# Training years
year_tr<-2009:2013

# MAE in the different stations (mean of all testing years)
MAEyears<-list()
for (i in 1:length(year_tr)){
	MAEyear<-as.data.frame(matrix(0,nrow=length(year_tr), ncol=4))
	colnames(MAEyear)<-c("SVM","M5P","ELM","MLPreg")
	rownames(MAEyear)<-c(1:4,"mean")
	for(j in 1:4){
		for(k in 1:4){
			MAEyear[k,j]<-MAE(predictions.test[[i]][which(st.test[[i]]==k),j],predictions.test[[i]]$real[which(st.test[[i]]==k)])
		}
		MAEyear[5,j]<-MAE(predictions.test[[i]][,j],predictions.test[[i]]$real)
	}
	MAEyears[[i]]<-MAEyear
}
names(MAEyears)<-year_tr

# Create the table
table3<-list(as.data.frame(matrix(0,ncol=5,nrow=5)), as.data.frame(matrix(0,ncol=5,nrow=5)),as.data.frame(matrix(0,ncol=5,nrow=5)),as.data.frame(matrix(0,ncol=5,nrow=5)))
names(table3)<-c("SVM", "M5P", "ELM", "MLP")
for (i in 1:length(MAEyears)){		#years
	for (j in 1:4){			#algorithms
		table3[[j]][,i]<-MAEyears[[i]][,j]	
	}
	
}
for (i in 1:length(table3)){
	colnames(table3[[i]])<-2009:2013
	rownames(table3[[i]])<-c("st 1", "st 2", "st 3", "st 4", "mean")
}

# Print xtable
table3<-do.call(cbind, table3)
table<-xtable(table3,  caption="MAE testing error obtain with different general models using 
	      data from a unique year (columns) for training and testing in the others")
align(table)<-c("l",rep ("c",ncol(table)))
digits(table)<-2
print(table, hline.after=c(-1,0,5), table.placement="H", scale=0.7,sanitize.text.function=function(x){x})


###########################################################################################################################
#TABLE 5 How statitistics evlove depending on the cloudiness per station
############################################################################################################################

# Load predictions (training period 2009-2012; testing 2013) 
setwd(paste0(dir,"/output"))
load("test2013.RData")
load("test2013local.RData")

predictions$factor<-as.numeric(cut(predictions$clearIndex, breaks=10))
predictions.split<-split(predictions, predictions$station)

# Create the table
datas<-list()
for (j in 1:4){
  data<-as.data.frame(matrix(0, ncol=4, nrow=10))
  colnames(data)<-c("MAE", "RMSE" , "instances","tol")
  for (i in 1:10){
    rows<-which(predictions.split[[j]]$factor==i)
    data$MAE[i]<-do.call(MAE, list(pred=predictions.split[[j]]$SVM[rows], real=predictions.split[[j]]$real[rows]))
    data$RMSE[i]<-do.call(RMSE, list(pred=predictions.split[[j]]$SVM[rows], real=predictions.split[[j]]$real[rows]))
    res.rel<-100*abs((predictions.split[[j]]$real[rows]-predictions.split[[j]]$SVM[rows])/predictions.split[[j]]$real[rows])
    data$tol[i]<-100*length(which(res.rel<5))/length(rows)
    data$instances[i]<-length(rows)
  }
  datas[[j]]<-data
}

# Print xtable
table<-do.call(cbind, datas)
rownames(table)<-as.character(levels((cut(predictions$clearIndex, breaks=10))))
table<-xtable(table,  caption="R2, MAE and RMSE testing errors (MJ/m2day) for the general models in the different locations")
align(table)<-c("l",rep ("c",ncol(table)))
digits(table)<-2
print(table, hline.after=c(-1,0,10), table.placement="H", scale=0.8,sanitize.text.function=function(x){x})







