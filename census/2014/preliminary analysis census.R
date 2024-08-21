library(XLConnect)
set.seed(1)

wb <- loadWorkbook("plant census 2014 updated.xlsx")
data.census <- readWorksheet(wb, sheet = "Data")
data.species <- readWorksheet(wb, sheet="Species codes")

speciesindices <- match(data.census$Morphoname, data.species$Code)

data.census <- cbind(data.census, data.species[speciesindices,1:4])
data.census$Taxon <- paste(data.census$Genus, data.census$Species)
data.census$Taxon <- factor(data.census$Taxon, levels=sample(names(sort(table(data.census$Taxon)))))
data.census$TaxonAlphabetic <- factor(data.census$Taxon, levels=sort(levels(data.census$Taxon)))

data.census$Tag.offset.x[is.na(data.census$Tag.offset.x)] <- -10

data.census$Tag.offset.y[is.na(data.census$Tag.offset.y)] <- -10


getcode <- function(string)
{
	z <- strsplit(as.character(string), split=" ")[[1]]
	w <- sapply(z, substr, 1, 3)
	return(paste(toupper(w),collapse=""))
}

drawcircle <- function(x,y,r,col,bcol='black')
{
	theta <- seq(0,2*pi,length.out=36)
	xpos <- r * cos(theta) + x
	ypos <- r * sin(theta) + y
	
	polygon(xpos, ypos, col=col,lwd=0.5,border=bcol)
}

pdf(width=5,height=5,file='census map v3.pdf')
par(mar=c(2,2,1,1))
par(cex.axis=0.5)
by(data.census, data.census$Plot, function(plotdata) {
	plot(0,0,type='n',xlim=c(0,200),ylim=c(0,200))
	

	for (i in 1:nrow(plotdata))
	{
		drawcircle(plotdata$X..cm.[i],plotdata$Y..cm.[i],plotdata$Length..cm.[i]/2,col=NULL,bcol='gray')

		segments(plotdata$X..cm.[i]+0,plotdata$Y..cm.[i]+0,plotdata$X..cm.[i]+plotdata$Tag.offset.x[i],plotdata$Y..cm.[i]+plotdata$Tag.offset.y[i],col='gray')
		text(jitter(plotdata$X..cm.[i]+plotdata$Tag.offset.x[i],amount=3),jitter(plotdata$Y..cm.[i]+plotdata$Tag.offset.y[i],amount=3),paste(plotdata$Tag[i],getcode(plotdata$Taxon[i]),sep="\n"),col='black',cex=0.3)
	}
	
	mtext(text=paste("Plot",plotdata$Plot[1]),line=0,cex=0.75)
})
dev.off()

# write out clean file
cleandata <- data.census[,c("Plot","Tag","X..cm.","Y..cm.","Tag.offset.x","Tag.offset.y","Taxon","Length..cm.","Height..cm.","X..Capitulescences")]
write.csv(cleandata, 'cleandata2014.csv',row.names=F)


# abundance distribution
pdf(width=6,height=6,file='census abundance distribution.pdf')
par(mar=c(4,4,1,1))

hist(table(data.census$Taxon),xlim=c(0,400),col='red',xlab='Abundance',ylab='Number of species',main='')
dev.off()

# size distribution per species 
pdf(width=10,height=8,file='census size distribution.pdf')
par(mfrow=c(4,5))
par(mar=c(4,4,1,1))
par(mgp=c(2,1,0))
by(data.census, data.census$TaxonAlphabetic, function(plotdata) 
{
	hist(plotdata$Length..cm.,breaks=seq(0,150,by=1),main=plotdata$Taxon[1],xlab='Length (cm)',ylab='Number of individuals',xlim=c(0,30),col='blue',ylim=c(0,25),cex.main=0.75)	
	box()
})
dev.off()




pdf(width=10,height=8,file='census fecundity.pdf')
par(mfrow=c(4,5))
par(mar=c(4,4,1,1))
par(mgp=c(2,1,0))
by(data.census, data.census$TaxonAlphabetic, function(plotdata) 
{
	plot(X..Capitulescences~Length..cm.,data=plotdata,main=plotdata$Taxon[1],xlab='Length (cm)',ylab='Number of capitulescences',xlim=c(0,30),col='blue',cex.main=0.75)	
	box()
})
dev.off()