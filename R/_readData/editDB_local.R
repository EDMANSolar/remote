################################################################################
# Script: editDB_local.R
# Authors: Ruben Urraca
# Comments: Selection of the training and testing periods for the local models.
# Database is also normalized between 0 and 1. 
################################################################################
rm(list=ls(all=TRUE))

# Workspace
home<-"/Users/rubenpazu"
setwd(paste0(home,"/Dropbox/softcomp/paper_enersol/"))
dir<-getwd()

# Load database (already cleaned)
setwd(paste(dir,"/data/",sep=""))
load("meteoReduced.RData")

# Split train-test. Test set = 2013
trainSet<-function(dframe_orig){
	dframe_train<-dframe_orig[dframe_orig$Year!=2013,]
	return(dframe_train)
}
testSet<-function(dframe_orig){
	dframe_test<-dframe_orig[dframe_orig$Year==2013,]
	return(dframe_test)
}
meteo.train<-lapply(meteo,trainSet)
meteo.test<-lapply(meteo, testSet)

# Create traninig database (select inputs and outputs)
inputs<-c(6:10,12,15:21) #not include rad and clear index
outputs<-11
create<-function(dframe_orig){
	dframe_edited<-as.data.frame(matrix(0,nrow=nrow(dframe_orig),ncol=length(inputs)+1))
	dframe_edited[,1:length(inputs)]<-dframe_orig[,inputs]
	dframe_edited[,length(inputs)+1]<-dframe_orig[,outputs]
	colnames(dframe_edited)<-colnames(dframe_orig)[c(inputs,outputs)]
	return(dframe_edited)
}
meteo.train<-lapply(meteo.train,create)
meteo.test<-lapply(meteo.test,create)

# Normalize training database per station
normalize.vector<-function(x, min, max){(x-min)/(max-min)}
ranges.train<-list();
for (i in 1:length(meteo)){
	ranges.train[[i]]<-apply(meteo.train[[i]], 2, range)
	for (j in 1:ncol(meteo.train[[i]])){
		meteo.train[[i]][,j]<-normalize.vector(meteo.train[[i]][,j], ranges.train[[i]][1,j], ranges.train[[i]][2,j])
		meteo.test[[i]][,j]<-normalize.vector(meteo.test[[i]][,j], ranges.train[[i]][1,j], ranges.train[[i]][2,j])
	}
}

# Save databases and ranges
setwd(paste(dir,"/data/",sep=""))
save(ranges.train, meteo.train, meteo.test, file="local_normalized.Rdata")
for (i in 1:length(meteo)){
	write.csv(meteo.train[[i]], file=paste0("local_train_",i,".csv"), row.names=FALSE)
	write.csv(meteo.test[[i]], file=paste0("local_test_",i,".csv"), row.names=FALSE)	
}
