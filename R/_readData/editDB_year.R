################################################################################
# Script: editDB_year.R
# Authors: Ruben Urraca
# Comments: Creation of multiple databases for the temporal validation of the 
# models. The testing period in each database is varied from 2009 to 2013 making
# a total of 5 databases. Data is normalized between 0 and 1
################################################################################
rm(list=ls(all=TRUE))

# Workspace
home<-"/Users/rubenpazu"
setwd(paste0(home,"/Dropbox/softcomp/paper_enersol/"))
dir<-getwd()

# Load database (already cleaned)
setwd(paste(dir,"/data/",sep=""))
load("meteoReduced.RData")

# Bind the list (stations) into a global dataframe
for (i in 1:length(meteo)){
	meteo[[i]]$station<-rep(i, nrow(meteo[[i]]))
}
meteo<-do.call(rbind,meteo)

# Split train-test. Train with one year, test in the rest
years<-c(2009:2013)
trainSet<-function(dframe_orig){
	dframe_train<-dframe_orig[dframe_orig$Year==year_train,]
	return(dframe_train)
}
testSet<-function(dframe_orig){
	dframe_test<-dframe_orig[!(dframe_orig$Year==year_train),]
	return(dframe_test)
}

meteo.train<-list(); meteo.test<-list(); st.train<-list(); st.test<-list(); year.test<-list()
for (i in 1:length(years)){
	year_train<-years[i]
	meteo.train[[i]]<-trainSet(meteo)
	meteo.test[[i]]<-testSet(meteo)
	st.train[[i]]<-meteo.train[[i]]$station
	st.test[[i]]<-meteo.test[[i]]$station
	year.test[[i]]<-meteo.test[[i]]$Year
}

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
for (i in 1:length(years)){
	ranges.train[[i]]<-apply(meteo.train[[i]], 2, range)
	for (j in 1:ncol(meteo.train[[i]])){
		meteo.train[[i]][,j]<-normalize.vector(meteo.train[[i]][,j], ranges.train[[i]][1,j], ranges.train[[i]][2,j])
		meteo.test[[i]][,j]<-normalize.vector(meteo.test[[i]][,j], ranges.train[[i]][1,j], ranges.train[[i]][2,j])
	}
}

#Save databases and ranges
setwd(paste(dir,"/data/",sep=""))
save(ranges.train, meteo.train, meteo.test, st.train, st.test, year.test, file="year_normalized.Rdata")

