library(XLConnect)
set.seed(1)

wb <- loadWorkbook("plant census 2014-2017.xlsx")
data.census <- readWorksheet(wb, sheet = "Data")
data.species <- readWorksheet(wb, sheet="Species codes")[,c("Family","Genus","Species","Code")]

# provide dummy codes for species with full latin names
realnames <- unique(data.species[,c("Family","Genus","Species")])
realnames$Code <- paste(realnames$Genus, realnames$Species)
data.species <- rbind(data.species, realnames)

# merge in new tags
merge_id <- which(data.census$Tag.should.not.exist==1 & !is.na(data.census$Merge.tag.with))
for (i in 1:length(merge_id))
{
	right_id = which(data.census$Year==data.census$Year[merge_id[i]] & data.census$Tag==data.census$Merge.tag.with[merge_id[i]])
	stopifnot(length(unique(data.census$Morphoname[c(merge_id[i],right_id)]))==1)
	
	# keep original offset, Died status, identification status
	# take mean position
	data.census$X..cm.[right_id] = mean(c(data.census$X..cm.[right_id], data.census$X..cm.[merge_id[i]]),na.rm=T)
	data.census$Y..cm.[right_id] = mean(c(data.census$Y..cm.[right_id], data.census$Y..cm.[merge_id[i]]),na.rm=T)
	# take max size
	data.census$Length..cm.[right_id] = max(c(data.census$Length..cm.[right_id], data.census$Length..cm.[merge_id[i]]),na.rm=T)
	data.census$Height..cm.[right_id] = max(c(data.census$Height..cm.[right_id], data.census$Height..cm.[merge_id[i]]),na.rm=T)
	data.census$X..Capitulescences[right_id] = max(c(data.census$X..Capitulescences[right_id], data.census$X..Capitulescences[merge_id[i]]),na.rm=T)
	# throw an error if we are trying to merge a seedling patch - has not happened
	stopifnot(data.census$Number.of.individuals.represented.by.tag[merge_id[i]]<=1 | is.na(data.census$Number.of.individuals.represented.by.tag[merge_id[i]]))

	print(data.census[c(merge_id[i],right_id),])
}

# remove rows that should be removed
data.census <- data.census[-which(data.census$Tag.should.not.exist==1),]


# check to see if there are any morphonames that should not exist
namematches <- data.census$Morphoname[! data.census$Morphoname %in% c(data.species$Code)]
stopifnot(all(is.na(namematches)))
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
data.census$Length..cm.[data.census$Died.this.census==1 & is.na(data.census$Length..cm.)] <- 0
data.census$Height..cm.[data.census$Died.this.census==1 & is.na(data.census$Length..cm.)] <- 0
data.census$Identification.uncertain[is.na(data.census$Identification.uncertain)] <- 0



# add previous-year plants
startyear <- min(data.census$Year,na.rm=T)
toadd_index <- which(data.census$Should.be.in.starting.year >= startyear)
toadd = data.census[toadd_index,]
alladd <- NULL

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
data.census$Is.seedling[toadd_index] <- 0 # can't be a seedling in the current year



# check for things that are seedlings in more than one year
dc_noprelim <- data.census
dc_noprelim <- dc_noprelim[-which(dc_noprelim $Preliminary.seedling.census==1),]
dc_noprelim <- dc_noprelim[-which(dc_noprelim $Died.this.census ==1),]

multiseed <- tapply(dc_noprelim $Is.seedling, dc_noprelim $Tag, sum)
multiseed <- multiseed[multiseed > 1]
multiseed
stopifnot(length(multiseed) == 0)
multiseed



# check for strange sizes and locations (not necessarily problems)
subset(data.census,(data.census $Length..cm. <0 | data.census $Length..cm. > 150) & Died.this.census!=1)
subset(data.census,(data.census $Height..cm. <0 | data.census $Height..cm. > 35) & Died.this.census!=1)
subset(data.census,abs(data.census $Tag.offset.x..cm.) > 50)
subset(data.census,abs(data.census $Tag.offset.y..cm.) > 50)


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
		# rewrite plot, position, tag position, and taxon information
		# note that morphoname is unchanged
		data.census[dcss_current_ids,"Plot"] <- data.census[dcss_thisyear_ids,"Plot"]
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
		print(sprintf("Tag %s, %d %d", tag, length(dcss_current_ids), length(dcss_thisyear_ids)))	
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










# make list of dead tags to pull at the end of the year
identify_dead_tags <- function(year.now, year.past)
{
	dead_tags <- vector(mode="list",length=(year.now-year.past))
	for (year in year.past:year.now)
	{
		tags_dead_this_year <- data.census[which(data.census$Year==year & data.census$Length..cm.==0),"Tag"]
		dead_tags[[year-year.past+1]] <- tags_dead_this_year
	}
	# find tags that are dead in all the requested years
	dead_all_years <- do.call('intersect',dead_tags)
	
	# make a data frame of current year locations for the dead tags
	df_pull_tags <- data.census[which(data.census$Tag %in% dead_all_years & data.census$Year==year.now),]
	
	return(df_pull_tags)
}
dead_tags_current <- identify_dead_tags(2017, 2016)
write.csv(dead_tags_current[,c("Plot","Tag","Taxon","X..cm.","Y..cm.","Tag.offset.x..cm.","Tag.offset.y..cm.","Is.seedling","Number.of.individuals.represented.by.tag","Notes","Tag.removed.in.year")], "dead_tags_2017.csv",row.names=FALSE)

#
warning('Need to go to field to remove these dead tags')


# flag the removed tags
data.census$Tag.removed.in.year[data.census$Tag %in% dead_tags_current$Tag] <- 2017








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
dfcurrent2017 <- makecleandf(dataforyear=2017, prelimseedlingsforyear=NULL, includepreviousyearnotes=FALSE)
dfcurrent <- dfcurrent2017
# export file
write.csv(dfcurrent2017, file='census 2017 clean.csv',row.names=F,na="")

dfcurrent2016 <- makecleandf(dataforyear=2016, prelimseedlingsforyear=NULL, includepreviousyearnotes=FALSE)
# export file
write.csv(dfcurrent2016, file='census 2016 clean.csv',row.names=F,na="")


# make clean dataframe
dfcurrent2015 <- makecleandf(dataforyear=2015, prelimseedlingsforyear=NULL, includepreviousyearnotes=FALSE)
# export file
write.csv(dfcurrent2015, file='census 2015 clean.csv',row.names=F,na="")

dfcurrent2014 <- makecleandf(dataforyear=2014, prelimseedlingsforyear=NULL, includepreviousyearnotes=FALSE)
# export file
write.csv(dfcurrent2014, file='census 2014 clean.csv',row.names=F,na="")




# write out abundances
abundancescurrent <- do.call("rbind",by(dfcurrent$Taxon, dfcurrent$Plot, table))
row.names(abundancescurrent) <- paste("Plot",row.names(abundancescurrent))

write.csv(abundancescurrent,'baldy abundances current 2017 bblonder.csv')



















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


drawplot <- function(data.census, plotid, dataforyear, prelimseedlingsforyear,xlim=c(-10,210),ylim=c(-10,210), specieshighlight=NA, title="")
{
	
	datass_seed_no <- subset(data.census, Year %in% dataforyear & Preliminary.seedling.census==0 & Plot==plotid)
	datass_seed_yes <- subset(data.census, Year %in% prelimseedlingsforyear & Preliminary.seedling.census==1 & Plot==plotid)

	plotdata <- rbind(datass_seed_no, datass_seed_yes)
	
	
	print(plotid)

	plot(0,0,type='n',xlim=xlim,ylim=ylim)

	if (nrow(plotdata) > 0)
	{
		for (i in 1:nrow(plotdata))
		{
			if (plotdata$Taxon[i]==specieshighlight || is.na(specieshighlight))
			{
				whichcolor <- rainbow(length(levels(data.census$Taxon)),alpha=0.75)[as.numeric(plotdata$Taxon[i])]
				whichcolor_light <- rainbow(length(levels(data.census$Taxon)),alpha=0.25)[as.numeric(plotdata$Taxon[i])]
				whichcolor_text <- gray(0,0.75)
			}
			else
			{
				whichcolor <- gray(0.5,0.1)
				whichcolor_light <- gray(0.5,0.1)
				whichcolor_text <- gray(0.5,0.1)
			}
			if (plotdata$Died.this.census[i]==1)
			{
				whichcolor <- "#000000"
			}
			
			if (plotdata$Number.of.individuals.represented.by.tag[i] > 1)
			{
				for (j in 1:plotdata$Number.of.individuals.represented.by.tag[i])
				{
					rx <- plotdata$X..cm.[i] + plotdata$Length..cm.[i]*cos(j/plotdata$Number.of.individuals.represented.by.tag[i]*(2*pi))/2
					ry <- plotdata$Y..cm.[i] + plotdata$Length..cm.[i]*sin(j/plotdata$Number.of.individuals.represented.by.tag[i]*(2*pi))/2
					
					drawcircle(rx,ry,r=plotdata$Length.per.individual..cm.[i]/2,col= whichcolor_light,bcol=whichcolor,lwd=0.25)
					segments(plotdata$X..cm.[i],plotdata$Y..cm.[i],rx,ry,lwd=0.25,lty=2)
				}
			}
			else
			{
				drawcircle(plotdata$X..cm.[i],plotdata$Y..cm.[i],plotdata$Length..cm.[i]/2,bcol=whichcolor,col= whichcolor_light)
			}
			
			if(plotdata$Died.this.census[i]==1)
			{
				points(plotdata$X..cm.[i],plotdata$Y..cm.[i],col= whichcolor_text,pch=4,cex=0.3)
			}
			if(plotdata$Length..cm.[i]==0)
			{
				points(plotdata$X..cm.[i],plotdata$Y..cm.[i],col= 'lightgray',pch=0,cex=0.3,lwd=0.5)
			}
			if(plotdata$Is.seedling[i]==1)
			{
				points(plotdata$X..cm.[i],plotdata$Y..cm.[i],col= whichcolor_text,pch=3,cex=0.3)
			}
	
		  # show zero-size (dead) individuals in different colors
		  if (plotdata$Length..cm.[i]==0)
		  {
		    whichcolor_text = 'lightgray'
		  }
		  
			segments(plotdata$X..cm.[i]+0,plotdata$Y..cm.[i]+0,plotdata$X..cm.[i]+plotdata$Tag.offset.x[i],plotdata$Y..cm.[i]+plotdata$Tag.offset.y[i],col= whichcolor_text,lwd=0.5)
			text(plotdata$X..cm.[i]+plotdata$Tag.offset.x[i],plotdata$Y..cm.[i]+plotdata$Tag.offset.y[i],plotdata$Tag[i],col= whichcolor_text,cex=0.4,font=2)
			text(plotdata$X..cm.[i],plotdata$Y..cm.[i],getcode(plotdata$Taxon[i]),col= whichcolor_text,cex=0.4,font=2)
			
		}
	}
	
	mtext(text=sprintf("Plot %d %s",plotid,title),line=0,cex=0.75)	
}


pdf(width=5,height=5,file='census map 2017.pdf')
par(mar=c(2,2,1,1))
par(cex.axis=0.5)
for (i in 1:50)
{
	drawplot(data.census, i, 2017,NULL)
}
dev.off()



pdf(width=5,height=5,file='census map 2017 zoomed.pdf')
par(mar=c(2,2,1,1))
par(cex.axis=0.5)
for (i in 1:50)
{
	drawplot(data.census, i, 2017,NULL,xlim=c(-10,110),ylim=c(-10,110),title="Lower left")
	drawplot(data.census, i, 2017,NULL,xlim=c(90,210),ylim=c(-10,110),title="Lower right")
	drawplot(data.census, i, 2017,NULL,xlim=c(-10,110),ylim=c(90,210),title="Upper left")
	drawplot(data.census, i, 2017,NULL,xlim=c(90,210),ylim=c(90,210),title="Upper right")
}
dev.off()


for (taxon in levels(data.census$Taxon))
{
	pdf(width=5,height=5,file=sprintf('census map 2017 %s.pdf',as.character(taxon)))
	par(mar=c(2,2,1,1))
	par(cex.axis=0.5)
	for (i in 1:50)
	{
		drawplot(data.census, i, 2017,NULL,specieshighlight=as.character(taxon),title=as.character(taxon))
	}
	dev.off()
}


