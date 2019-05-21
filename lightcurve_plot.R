# load libraries
library("Hmisc") # for error bars

# set up plot constants
plot.kron = TRUE
ebar.color <- "lightgrey"
legendX <- 56000
y.expand <- 1.02
y.resolution <- 0.05
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
subtract.this <- 0*buj.lightcurve$sky # it seems sky brightness is already subtracted (per reddit inquiries)
my.y.plus <- (buj.lightcurve$psfFlux - subtract.this)*1000 + buj.lightcurve$psfFluxErr*1000 # plus error bar in milliJanskys
my.y.minus<- (buj.lightcurve$psfFlux - subtract.this)*1000 - buj.lightcurve$psfFluxErr*1000 # minus error bar in milliJanskys

# plot this stuff over the list of filters
plot.add <- FALSE
my.ylim <- c(0,ceil(max(buj.lightcurve$psfFlux*1000)*y.expand/y.resolution)*y.resolution)
quartz("PSF flux")
for(filter.num in 1:num.filters) {
#	cat("\n Filter Number",filter.num)
	this.filter <- Psbands$filterIDs[filter.num]	
	these.obs <- buj.lightcurve$filterID == this.filter
	my.plot.color <- Psbands$plotColor[filter.num]
	# plot points with error bars for the band in question
	errbar(buj.lightcurve$obsTime[these.obs],(buj.lightcurve$psfFlux[these.obs] - subtract.this[these.obs])*1000,
			ylim=my.ylim,
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
# plot the Kron magnitudes
ok.pts  <- buj.lightcurve$kronFlux > 0
if(plot.kron) {
	# calculate the error bar heights
	my.y.plus <- buj.lightcurve$kronFlux*1000 + buj.lightcurve$kronFluxErr*1000 # plus error bar in milliJanskys
	my.y.minus<- buj.lightcurve$kronFlux*1000 - buj.lightcurve$kronFluxErr*1000 # minus error bar in milliJanskys
	# set the ylimits to something sensible
	my.ylim <- c(0,ceil(max(buj.lightcurve$kronFlux*1000)*y.expand/y.resolution)*y.resolution)

	plot.add <- FALSE
	quartz("Kron flux")
	for(filter.num in 1:num.filters) {
	#	cat("\n Filter Number",filter.num)
		this.filter <- Psbands$filterIDs[filter.num]	
		these.obs <- buj.lightcurve$filterID == this.filter & ok.pts
		my.plot.color <- Psbands$plotColor[filter.num]
		# plot points with error bars for the band in question
		errbar(buj.lightcurve$obsTime[these.obs],
			buj.lightcurve$kronFlux[these.obs]*1000,
			ylim = my.ylim,
			col= my.plot.color,
			yplus=my.y.plus[these.obs],yminus=my.y.minus[these.obs],
			xlab = "MJD of observation time",ylab="Kron flux(mJ)",
			pch=Psbands$symbols[filter.num],cex.main=0.7,
			add= plot.add,errbar.col=ebar.color)
		plot.add <- TRUE
		# draw a grid and a legend
		grid(col="black")
		legend(x= legendX,y=legendY,legend=Psbands$bandinQ,col= Psbands$plotColor,pch=Psbands$symbols)
		title(main=my.title)
		
	}
}

quartz("PSF/Kron")
plot.add <- FALSE
plot.mags = -2.5*log10(buj.lightcurve$psfFlux[ok.pts]/buj.lightcurve$kronFlux[ok.pts])
plot(buj.lightcurve$obsTime[ok.pts],
	plot.mags,
	pch=3,
	col="red",
	xlab = "MJD of observation time",ylab="(PSF/Kron) flux(mag)",
	main="Comparison of PSF to Kron flux")
grid(col="black")