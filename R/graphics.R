################################################################################
# Script: graphics.R
# Authors: Ruben Urraca
# Comments: Generation of the figures for the paper
################################################################################
rm(list=ls(all=TRUE))

# Workspace
home<-"/Users/rubenpazu"
setwd(paste0(home,"/Dropbox/softcomp/paper_enersol/"))
dir<-getwd()

# Packages
library(lattice)
library(latticeExtra)
library(rasterVis)
library(grid)
library(dae)
library(grDevices)


################################################################################
# FIGURE 2: SCATTER PLOT PREDICTED-REAL
################################################################################

# Load predictions (training period 2009-2012; testing 2013) 
setwd(paste0(dir,"/output"))
load("test2013.RData")

# Adapt the predictions dataframe to a lattice inpute.
# A factor with 24 levels (4 stations x 6 algorithms) is created
algorithms=6;
graphics<-as.data.frame(matrix(0,nrow=algorithms*nrow(predictions), ncol=6))
colnames(graphics)<-c("real", "pred", "clearIndex", "algorithm", "station", "factor")
graphics$real<-rep(predictions$real,algorithms)
graphics$pred<-c(predictions$SVM, predictions$M5P, predictions$ELM, predictions$MLP, predictions$LS, predictions$Anto)
graphics$ClearIndex<-rep(predictions$clearIndex,algorithms)	
graphics$station<-as.factor(rep(predictions$station,algorithms))
graphics$algorithm<-as.factor(rep(1:algorithms, each=nrow(predictions)))	
graphics$factor<-as.factor(paste0(graphics$algorithm, graphics$station))
levels(graphics$factor)<-c("SVR Cordoba","SVR Jaen", "SVR Puebla", "SVR Nijar",
			   "M5P Cordoba", "M5P Jaen", "M5P Puebla", "M5P Nijar",
			   "ELM Cordoba", "ELM Jaen", "ELM Puebla", "ELM Nijar",
			   "MLP Cordoba", "MLP Jaen", "MLP Puebla", "MLP Nijar",
			   "LS Cordoba", "LS Jaen", "LS Puebla", "LS Nijar",
			   "AN Cordoba", "AN Jaen", "AN Puebla", "AN Nijar")

# Color palette 
breaks<-10
pal<-colorRampPalette(c("DarkBlue", "DodgerBlue", "Cyan"), interpolate="linear")(breaks)
palette(pal)

# Figures directory
setwd(paste0(dir,"/paper/figures"))

trellis.device(pdf, file='real_pred.pdf')
xyplot(graphics$real~graphics$pred|graphics$factor, 
       lwd=1, 
       alpha=0.6, 
       type="p", 
       superpose=TRUE,
       pch=21, 
       xlab="Estimated irradiation", ylab="Measured irradiation",  
       aspect=0.6,
       as.table=T, 
       ylim=c(0,35), xlim=c(0,35),
       scales=list(cex=0.6),
       key=list(space="right",         	 
       	 	rectangles=list(col=rep(1:length(pal))), 
       	 	size=1,
       	 	text=list(labels=levels(cut(graphics$ClearIndex, breaks=breaks, dig.lab=2)),cex=0.6),
      	 	title="CS Index", cex.title=0.8),
      par.settings = list(strip.background=list(col="yellow")),
       par.strip.text=list(cex=0.7),
       panel=function(x,y,...){
       	rows<-which(as.numeric(graphics$factor)==panel.number())
       	panel.xyplot(x,y, col=as.numeric(cut(graphics$ClearIndex[rows],breaks=breaks)), pch=20, cex=0.6)
       	#panel.points(x,y, col="black", pch=21, cex=0.6)
       	lmLine <- lm(y ~ x)
       	r2<-round(summary(lmLine)$r.squared, 2)
       	grid.text(paste('R??=', formatC(round(r2,2), format="f", digits=2)), .03, .9, just='left', gp=gpar(cex=0.7, col='black'))       
       	panel.abline(lmLine, col='black',lwd=1.3)
       	panel.abline(a=0, b=1, col="red")
       }
)

dev.off()

