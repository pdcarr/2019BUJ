# load libraries
library("Hmisc") # for error bars

# set up plot constants
ebar.color <- "lightgrey"
legendX <- 56000
legendY <- 0.2
num.filters <- 5
my.title <- "Gaia Source ID 4239741678796540800"
Psbands <- data.frame(bandinQ=c("g","r","i","z","y"),filterIDs<-c(1,2,3,4,5),plotColor=c("blue","red","magenta","violet","black"), symbols<-c(0,1,2,3,4),stringsAsFactors=FALSE)
#num.filters <- 2
#Psbands <- data.frame(bandinQ=c("g","r"),filterIDs<-c(1,2),plotColor=c("green","red"), symbols<-c(0,1), stringsAsFactors=FALSE)
### load .csv file into a data frame.
lightcurve.name <- "data/PS-5_16_2019.csv"
buj.lightcurve <- read.csv(file= lightcurve.name,header=TRUE,check.names=TRUE,na.strings="NA",stringsAsFactors=FALSE)
totalRec = length(buj.lightcurve$detectID)
cat("\n\nTotal records read from file: ",totalRec,"\n\n")
# plot PSF flux for each filter
quartz("PSF flux")
subtract.this <- 0*buj.lightcurve$sky 
my.y.plus <- (buj.lightcurve$psfFlux - subtract.this)*1000 + buj.lightcurve$psfFluxErr*1000 # plus error bar in milliJanskys
my.y.minus<- (buj.lightcurve$psfFlux - subtract.this)*1000 - buj.lightcurve$psfFluxErr*1000 # minus error bar in milliJanskys

# plot this stuff over the list of filters
plot.add <- FALSE
for(filter.num in 1:num.filters) {
#	cat("\n Filter Number",filter.num)
	this.filter <- Psbands$filterIDs[filter.num]	
	these.obs <- buj.lightcurve$filterID == this.filter
	my.plot.color <- Psbands$plotColor[filter.num]
	# plot points with error bars for the band in question
	errbar(buj.lightcurve$obsTime[these.obs],(buj.lightcurve$psfFlux[these.obs] - subtract.this[these.obs])*1000,
			col= my.plot.color,
			yplus=my.y.plus[these.obs],yminus=my.y.minus[these.obs],
			xlab = "MJD of observation time",ylab="PSF flux(mJ)",
			pch=Psbands$symbols[filter.num],cex.main=0.7,
			add= plot.add,errbar.col=ebar.color)
	plot.add <- TRUE
	# draw a grid and a legend
	grid(col="black")
	legend(x= legendX,y=legendY,legend=Psbands$bandinQ,col= Psbands$plotColor,pch=Psbands$symbols)
	title(main=my.title)

}