library(XLConnect)
set.seed(1)

wb <- loadWorkbook("plant census 2015.xlsx")
data.census <- readWorksheet(wb, sheet = "Data")
data.species <- readWorksheet(wb, sheet="Species codes")[,c("Family","Genus","Species","Code")]

# provide dummy codes for species with full latin names
realnames <- unique(data.species[,c("Family","Genus","Species")])
realnames$Code <- paste(realnames$Genus, realnames$Species)
data.species <- rbind(data.species, realnames)

# remove rows that should be removed
# remove dummy rows
data.census <- data.census[-which(data.census$Tag.should.not.exist==1),]


# check to see if there are any morphonames that should not exist
namematches <- data.census$Morphoname[! data.census$Morphoname %in% c(data.species$Code)]
stopifnot(length(namematches)==0)
print(namematches)




speciesindices <- match(data.census$Morphoname, c(data.species$Code))

data.census <- cbind(data.census, data.species[speciesindices,1:3])
data.census$Taxon <- paste(data.census$Genus, data.census$Species)
data.census$Taxon <- factor(data.census$Taxon, levels=sample(names(sort(table(data.census$Taxon)))))

data.census$Tag.offset.x..cm.[is.na(data.census$Tag.offset.x..cm.)] <- -10

data.census$Tag.offset.y..cm.[is.na(data.census$Tag.offset.y..cm.)] <- -10

# remove dummy rows
data.census <- data.census[!is.na(data.census$Tag),]



checkduplicates <- function(d)
{
	# check for duplicate tags
	duplicates <- do.call("rbind",by(d, paste(d$Year, d$Preliminary.seedling.census), function(x) { 
		xss <- x[!is.na(x$Tag),]
		tags <- xss$Tag[duplicated(xss$Tag)]
		
		return(x[x$Tag %in% tags,])
	} ))
	duplicates <- duplicates[order(duplicates$Tag),]
	row.names(duplicates) <- NULL
	return(duplicates)	
}

duplicates <- checkduplicates(data.census)
stopifnot(nrow(duplicates)==0)


# check for duplicates again
duplicates <- checkduplicates(data.census)
stopifnot(nrow(duplicates)==0)

# check for things that are seedlings in more than one year
dc_noprelim <- data.census
dc_noprelim <- dc_noprelim[-which(dc_noprelim $Preliminary.seedling.census==1),]
dc_noprelim <- dc_noprelim[-which(dc_noprelim $Died.this.census ==1),]

multiseed <- tapply(dc_noprelim $Is.seedling, dc_noprelim $Tag, sum)
multiseed <- multiseed[multiseed > 1]
multiseed
stopifnot(length(multiseed) == 0)

# fill in NA values for empty cells
data.census$Died.this.census[is.na(data.census$Died.this.census)] <- 0
data.census$Tag.should.not.exist[is.na(data.census$Tag.should.not.exist)] <- 0
data.census$Should.be.in.starting.year[is.na(data.census$Should.be.in.starting.year)] <- 0
data.census$Preliminary.seedling.census[is.na(data.census$Preliminary.seedling.census)] <- 0
# fill in NA values for multi-seedling tags
data.census$Number.of.individuals.represented.by.tag[is.na(data.census$Number.of.individuals.represented.by.tag)] <- 1
data.census$Number.of.individuals.represented.by.tag[is.na(data.census$Died.this.census)] <- 0
# fill in NA values for dead plants
data.census$X..Capitulescences[data.census$Died.this.census==1] <- 0
data.census$Length..cm.[data.census$Died.this.census==1] <- 0
data.census$Height..cm.[data.census$Died.this.census==1] <- 0




# add previous-year plants
startyear <- min(data.census$Year,na.rm=T)
toadd <- subset(data.census, Should.be.in.starting.year >= startyear)
alladd <- NULL
stopifnot(all(toadd$Is.seedling==0))
toadd[toadd$Is.seedling==TRUE,]

for (i in 1:nrow(toadd))
{
	tempadd <- NULL
	for (whichyear in toadd$Should.be.in.starting.year[1]:(toadd$Year[1]-1))
	{
		newrecord <- toadd[i,]
		newrecord$Year <- whichyear
		newrecord$Should.be.in.starting.year <- 0
		newrecord$Date <- NA
		
		tempadd <- rbind(tempadd, newrecord)
	}
	alladd <- rbind(alladd, tempadd)
}
data.census <- rbind(data.census, alladd)




# check for strange sizes and locations (not necessarily problems)
subset(data.census,(Length..cm. <=0 | Length..cm. > 150) & Died.this.census!=1)
subset(data.census,(Height..cm. <=0 | Height..cm. > 30) & Died.this.census!=1)
subset(data.census,abs(Tag.offset.x..cm.) > 50)
subset(data.census,abs(Tag.offset.y..cm.) > 50)


# look for missing capitulescences
subset(data.census,is.na(X..Capitulescences))




# overwrite location / offset / ID for more recently-determined censuses

for (tag in unique(data.census$Tag))
{
	
	dcss_current_ids <- which(data.census$Tag==tag)
	
	currentyear <- max(data.census$Year[dcss_current_ids])
	
	dcss_thisyear_ids <- which(data.census$Tag==tag & data.census$Year==currentyear & data.census$Preliminary.seedling.census!=1)

	# make sure we only have one entry for the most current version of the tag
	stopifnot(length(dcss_thisyear_ids)<=1)
	

	
	if (length(dcss_thisyear_ids)==1)
	{	
		# rewrite position, tag position, and taxon information
		# note that morphoname is unchanged
		data.census[dcss_current_ids,"X..cm."] <- data.census[dcss_thisyear_ids,"X..cm."]
		data.census[dcss_current_ids,"Y..cm."] <- data.census[dcss_thisyear_ids,"Y..cm."]
		data.census[dcss_current_ids,"Tag.offset.x..cm."] <- data.census[dcss_thisyear_ids,"Tag.offset.x..cm."]
		data.census[dcss_current_ids,"Tag.offset.y..cm."] <- data.census[dcss_thisyear_ids,"Tag.offset.y..cm."]
		data.census[dcss_current_ids,"Taxon"] <- data.census[dcss_thisyear_ids,"Taxon"]
		data.census[dcss_current_ids,"Family"] <- data.census[dcss_thisyear_ids,"Family"]
		data.census[dcss_current_ids,"Genus"] <- data.census[dcss_thisyear_ids,"Genus"]
		data.census[dcss_current_ids,"Species"] <- data.census[dcss_thisyear_ids,"Species"]
	}
	else
	{
		print(tag)
		print(length(dcss_current_ids))
		print(length(dcss_thisyear_ids))
		print("\n")		
	}
}





# look for NAs in various columns that should not have them
dev.new()
par(mar=c(1,10,1,1))
image(is.na(data.census),xaxt='n',yaxt='n');
axis(side=2,at=seq(0,1,length.out=length(names(data.census))),labels=names(data.census),las=2)

# make sure final format is OK
print(str(data.census))







# arrange by quadrant
assigngridpos <- function(data, ng)
{
	xv <- as.numeric(cut(data$X..cm.,breaks=seq(0,200,by=200/ng),include.lowest=T))
	yv <- as.numeric(cut(data$Y..cm.,breaks=seq(0,200,by=200/ng),include.lowest=T))
	
	pos <- (yv-1)*ng+xv
	
	#return(cbind(data$X..cm.,data$Y..cm.,xv,yv,pos))
	return(pos)
}

data.census$grid <- assigngridpos(data.census, 4)



# sort data
data.census <- data.census[order(data.census$Year, data.census$Preliminary.seedling.census, data.census$Plot, data.census$grid, data.census$Tag),]















# generate clean data
makecleandf <- function(dataforyear,prelimseedlingsforyear, includepreviousyearnotes)
{
	datass_seed_no <- subset(data.census, Year %in% dataforyear & Preliminary.seedling.census==0)
	datass_seed_yes <- subset(data.census, Year %in% prelimseedlingsforyear & Preliminary.seedling.census==1)

	print(nrow(datass_seed_no))
	print(nrow(datass_seed_yes))

	datass <- rbind(datass_seed_no, datass_seed_yes)

	colnames <- c("Year","Plot","Tag","Morphoname","Taxon","Identification.uncertain","X..cm.","Y..cm.","Tag.offset.x..cm.","Tag.offset.y..cm.","Died.this.census","Length..cm.","Height..cm.","X..Capitulescences","Is.seedling","Number.of.individuals.represented.by.tag","Length.per.individual..cm.","Height.per.individual..cm.","Notes")
	datass <- datass[,colnames]
	
	row.names(datass) <- NULL

	if (includepreviousyearnotes==TRUE)
	{	
		for (i in 1:nrow(datass))
		{
			whichtag <- datass$Tag[i]
			
			whichnotes <- data.census$Notes[data.census$Tag==whichtag]
			whichyears <- data.census$Year[data.census$Tag==whichtag]
			whichid <- data.census$Preliminary.seedling.census[data.census$Tag==whichtag]
			
			allnotes <- paste(paste(paste(whichyears, whichid, sep="-"), whichnotes, sep=": "),collapse=", ")
			
	
			datass$Notes[i] <- allnotes
	
		}
	}	
	
	return(datass)	
}

# make clean dataframe
dfcurrent <- makecleandf(dataforyear=2015, prelimseedlingsforyear=NULL, includepreviousyearnotes=FALSE)






getcode <- function(string)
{
	z <- strsplit(as.character(string), split=" ")[[1]]
	w <- sapply(z, substr, 1, 3)
	return(paste(toupper(w),collapse=""))
}

drawcircle <- function(x,y,r,col,bcol,lwd=0.5,lty=1)
{
	theta <- seq(0,2*pi,length.out=36)
	xpos <- r * cos(theta) + x
	ypos <- r * sin(theta) + y
	
	polygon(xpos, ypos, col=col,lwd=0.5,border=bcol,lwd=lwd,lty=lty)
}

drawplot <- function(plotid, dataforyear, prelimseedlingsforyear,xlim=c(-10,210),ylim=c(-10,210))
{
	library(RColorBrewer)
	
	datass_seed_no <- subset(data.census, Year %in% dataforyear & Preliminary.seedling.census==0 & Plot==plotid)
	datass_seed_yes <- subset(data.census, Year %in% prelimseedlingsforyear & Preliminary.seedling.census==1 & Plot==plotid)

	plotdata <- rbind(datass_seed_no, datass_seed_yes)
	
	
	print(plotid)

	plot(0,0,type='n',xlim=xlim,ylim=ylim)

	if (nrow(plotdata) > 0)
	{
		for (i in 1:nrow(plotdata))
		{
			whichcolor <- rainbow(length(levels(data.census$Taxon)),alpha=0.25)[as.numeric(plotdata$Taxon[i])]
			if (plotdata$Died.this.census[i]==1)
			{
				whichcolor <- "#000000"
			}
			#print(whichcolor)
			
			if (plotdata$Number.of.individuals.represented.by.tag[i] > 1)
			{
				for (j in 1:plotdata$Number.of.individuals.represented.by.tag[i])
				{
					rx <- plotdata$X..cm.[i] + plotdata$Length..cm.[i]*cos(j/plotdata$Number.of.individuals.represented.by.tag[i]*(2*pi))/2
					ry <- plotdata$Y..cm.[i] + plotdata$Length..cm.[i]*sin(j/plotdata$Number.of.individuals.represented.by.tag[i]*(2*pi))/2
					
					drawcircle(rx,ry,r=plotdata$Length.per.individual..cm.[i]/2,col=whichcolor,bcol='gray',lwd=0.25)
					segments(plotdata$X..cm.[i],plotdata$Y..cm.[i],rx,ry,lwd=0.25,lty=2)
				}
			}
			else
			{
				drawcircle(plotdata$X..cm.[i],plotdata$Y..cm.[i],plotdata$Length..cm.[i]/2,bcol='gray',col=whichcolor)
			}
			
			if(plotdata$Died.this.census[i]==1)
			{
				points(plotdata$X..cm.[i],plotdata$Y..cm.[i],col='black',pch=4,cex=0.3)
			}
			if(plotdata$Is.seedling[i]==1)
			{
				points(plotdata$X..cm.[i],plotdata$Y..cm.[i],col='black',pch=3,cex=0.3)
			}
	
			segments(plotdata$X..cm.[i]+0,plotdata$Y..cm.[i]+0,plotdata$X..cm.[i]+plotdata$Tag.offset.x[i],plotdata$Y..cm.[i]+plotdata$Tag.offset.y[i],col=whichcolor,lwd=0.5)
			text(plotdata$X..cm.[i]+plotdata$Tag.offset.x[i],plotdata$Y..cm.[i]+plotdata$Tag.offset.y[i],paste(plotdata$Tag[i],getcode(plotdata$Taxon[i]),sep="\n"),col='black',cex=0.15)
			
		}
	}
	
	mtext(text=paste("Plot",plotid),line=0,cex=0.75)	
}

pdf(width=5,height=5,file='census map 2015 plot 46.pdf')
par(mar=c(2,2,1,1))
par(cex.axis=0.5)

	drawplot(46, 2015,NULL,xlim=c(-10,110),ylim=c(-10,110))
	drawplot(46, 2015,NULL,xlim=c(90,210),ylim=c(-10,110))
	drawplot(46, 2015,NULL,xlim=c(-10,110),ylim=c(90,210))
	drawplot(46, 2015,NULL,xlim=c(90,210),ylim=c(90,210))
dev.off()



data.census <- data.census[data.census$Genus %in% c("Heterotheca", "Ivesia"),]

pdf(width=5,height=5,file='census map 2015 plot 46 most species gone.pdf')
par(mar=c(2,2,1,1))
par(cex.axis=0.5)

	drawplot(46, 2015,NULL,xlim=c(-10,110),ylim=c(-10,110))
	drawplot(46, 2015,NULL,xlim=c(90,210),ylim=c(-10,110))
	drawplot(46, 2015,NULL,xlim=c(-10,110),ylim=c(90,210))
	drawplot(46, 2015,NULL,xlim=c(90,210),ylim=c(90,210))
dev.off()



