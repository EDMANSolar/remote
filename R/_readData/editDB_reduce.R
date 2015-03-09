################################################################################
# Script: editDB_reduce.R
# Authors: Ruben Urraca
# Comments: The original database is reduced to 4 stations taking data of the
# period after 2009
################################################################################
rm(list=ls(all=TRUE))

# Workspace
home<-"/Users/rubenpazu"
setwd(paste0(home,"/Dropbox/softcomp/paper_enersol/"))
dir<-getwd()

# Load original database (already cleaned)
setwd(paste(dir,"/data/",sep=""))
load("meteo.RData")

# Eliminate almeria and san lucar
meteo<-meteo[-13]#san lucar
meteo<-meteo[-1]#almeria

# Select 4 stations for the study: Cordoba, Jaen, Puebla Rio, Malaga and Nijar
meteo<-meteo[c(5:7,11)]

# Select data after 2009
meteo<-lapply(1:length(meteo), function(i){
	meteo[[i]]<-meteo[[i]][meteo[[i]]$Year>=2009,]
})

# Save the new database
setwd(paste(dir,"/data/",sep=""))
save(meteo, file="meteoReduced.RData")
