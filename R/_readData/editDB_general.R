################################################################################
# Script: editDB_general.R
# Authors: Ruben Urraca
# Comments: Selection of the training and testing periods. Database is also
# normalized between 0 and 1.
################################################################################
rm(list=ls(all=TRUE))

# Workspace
home<-"/Users/rubenpazu"
setwd(paste0(home,"/Dropbox/softcomp/paper_enersol"))
dir<-getwd()

# Load database (already cleaned and reduced)
setwd(paste(dir,"/data/",sep=""))
load("meteoReduced.RData")

# Bind the list (stations) into a global dataframe
for (i in 1:length(meteo)){
	meteo[[i]]$station<-rep(i, nrow(meteo[[i]]))
}
meteo<-do.call(rbind,meteo)

# Split train-test. Test set = year 2013 
trainSet<-function(dframe_orig){
	dframe_train<-dframe_orig[dframe_orig$Year!=2013,]
	return(dframe_train)
}
testSet<-function(dframe_orig){
	dframe_test<-dframe_orig[dframe_orig$Year==2013,]
	return(dframe_test)
}
meteo.train<-trainSet(meteo)
meteo.test<-testSet(meteo)
st_train<-meteo.train$station
st_test<-meteo.test$station
clearIndex_test<-meteo.test$ClearIndex

# Create traninig database (select inputs and outputs)
inputs<-c(6:10,12,15:21) 
outputs<-11 # rad (daily global irradiation)
create<-function(dframe_orig){
	dframe_edited<-as.data.frame(matrix(0,nrow=nrow(dframe_orig),ncol=14))
	dframe_edited[,1:13]<-dframe_orig[,inputs]
	dframe_edited[,14]<-dframe_orig[,outputs]
	colnames(dframe_edited)<-colnames(dframe_orig)[c(inputs,outputs)]
	return(dframe_edited)
}
meteo.train<-create(meteo.train)
meteo.test<-create(meteo.test)

# Normalize the database
normalize.vector<-function(x, min, max){(x-min)/(max-min)}
ranges.train<-apply(meteo.train, 2, range)
#we have to save the range (to de-normalize at the end)
for (i in 1:ncol(meteo.train)){
	meteo.train[,i]<-normalize.vector(meteo.train[,i], ranges.train[1,i], ranges.train[2,i])
	meteo.test[,i]<-normalize.vector(meteo.test[,i], ranges.train[1,i], ranges.train[2,i])
}

# Save databases and ranges. .csv files are required for the GA optimization
setwd(paste(dir,"/data/",sep=""))
save(ranges.train,  meteo.train, meteo.test, st_train, st_test, clearIndex_test, file="general_normalized.Rdata")
write.csv(meteo.train, file="general_train.csv", row.names=FALSE)
write.csv(meteo.test, file="general_test.csv", row.names=FALSE)

