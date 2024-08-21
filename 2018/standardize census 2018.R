library(readxl)
set.seed(1)

data.census <- as.data.frame(read_excel("plant census 2014-2018.xlsx",sheet=1, col_types=
                                          c("numeric", # Year
                                            "text", # Date 
                                            "text", # Census.team
                                            "numeric", # Plot
                                            "text", # Tag
                                            "numeric", # X
                                            "numeric", # Y
                                            "numeric", # Xoff
                                            "numeric", # Yoff
                                            "text", # Morpho
                                            "numeric", # Died.this
                                            "numeric", # Tag.should.not.exit
                                            "text", # Merge.tag.with
                                            "numeric", # Length
                                            "numeric", # Height
                                            "numeric", # X.Capit
                                            "numeric", # Id.uncertain
                                            "numeric", # Is.seedling
                                            "numeric", # Should.be.in.starting.year
                                            "numeric", # Num.indivs
                                            "numeric", # Length.indiv
                                            "numeric", # Height.indiv
                                            "text", # Notes
                                            "numeric", # Prelim.census
                                            "numeric" # Tag.removed.in.year
                                            
                                          )))

names(data.census) <- make.names(names(data.census))

data.species <- as.data.frame(read_excel("plant census 2014-2018.xlsx",sheet=3)); names(data.species) <- make.names(names(data.species))


# provide dummy codes for species with full latin names
realnames <- unique(data.species[,c("Family","Genus","Species")])
realnames$Code <- paste(realnames$Genus, realnames$Species)
data.species <- rbind(data.species[,c("Family","Genus","Species","Code")], realnames)

# merge in new tags
merge_id <- which(!is.na(data.census$Merge.tag.with))
for (i in 1:length(merge_id))
{
  # if there already is another individual this plant should be 'merged with...
	right_id = which(data.census$Year==data.census$Year[merge_id[i]] & data.census$Tag==data.census$Merge.tag.with[merge_id[i]])
	if (length(right_id)==1)
	{
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
  	cat(sprintf("Merging tag %s into existing tag %s in year %d\n",data.census$Tag[merge_id[i]], data.census$Tag[right_id], data.census$Year[merge_id[i]]))
	}
	else # if there is no tag existing already
	{
	  ids_old <- which(data.census$Tag == data.census$Tag[merge_id[i]])
	  # find all instances of this tag
	  #print(data.census[ids_old,])

	  cat(sprintf('Merging tag %s into new tag %s for years %s\n',data.census$Tag[merge_id[i]],data.census$Merge.tag.with[merge_id[i]], paste(data.census[ids_old,"Year"],collapse=", ")))
	  
	  	  
	  # overwrite all of the old tag names with the new 'replace with' tag name
	  data.census[ids_old,"Tag"] <- data.census$Merge.tag.with[merge_id[i]]
	  #print(data.census[ids_old,])
	  
	}
}

# remove old tag IDs that should be no longer exist (i.e. because they were merged)
removed_tags <- apply(data.census[which(data.census$Tag.should.not.exist==1),c("Year","Tag")],1,function(x) {sprintf("%s (%s)",x[2],x[1])})
data.census <- data.census[-which(data.census$Tag.should.not.exist==1),]
for (i in 1:length(removed_tags)) 
{ 
  cat(sprintf("Removed tag %s\n", removed_tags[i]))
}




# check to see if there are any morphonames that should not exist
namematches <- data.census$Morphoname[! data.census$Morphoname %in% c(data.species$Code)]
stopifnot(all(is.na(namematches)))
print(namematches)



# assign real taxon names
speciesindices <- match(data.census$Morphoname, c(data.species$Code))
data.census <- cbind(data.census, data.species[speciesindices,1:3])
data.census$Taxon <- paste(data.census$Genus, data.census$Species)
data.census$Taxon <- factor(data.census$Taxon, levels=sample(names(sort(table(data.census$Taxon)))))

# ad default offset
data.census$Tag.offset.x..cm.[is.na(data.census$Tag.offset.x..cm.)] <- -10
data.census$Tag.offset.y..cm.[is.na(data.census$Tag.offset.y..cm.)] <- -10

# remove dummy rows
data.census <- data.census[!is.na(data.census$Tag),]


# check for duplicate tag names
checkduplicates <- function(d)
{
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

# check for duplicates again (needed for hack)
duplicates <- checkduplicates(data.census)
stopifnot(nrow(duplicates)==0)



# fill in NA values for empty cells with appropriate values
data.census$Died.this.census[is.na(data.census$Died.this.census)] <- 0
data.census$Tag.should.not.exist[is.na(data.census$Tag.should.not.exist)] <- 0
data.census$Should.be.in.starting.year[is.na(data.census$Should.be.in.starting.year)] <- 0
data.census$Preliminary.seedling.census[is.na(data.census$Preliminary.seedling.census)] <- 0
# fill in NA values for multi-seedling tags with appropriate values
data.census$Number.of.individuals.represented.by.tag[is.na(data.census$Number.of.individuals.represented.by.tag)] <- 1
data.census$Number.of.individuals.represented.by.tag[is.na(data.census$Died.this.census)] <- 0
# fill in NA values for dead plants with appropriate values
data.census$X..Capitulescences[data.census$Died.this.census==1] <- 0
data.census$Length..cm.[data.census$Died.this.census==1 & is.na(data.census$Length..cm.)] <- 0
data.census$Height..cm.[data.census$Died.this.census==1 & is.na(data.census$Length..cm.)] <- 0
data.census$Height..cm.[is.na(data.census$Height..cm.)] <- 0
data.census$Identification.uncertain[is.na(data.census$Identification.uncertain)] <- 0



# add previous-year plants
startyear <- min(data.census$Year,na.rm=T)
toadd_index <- which(data.census$Should.be.in.starting.year >= startyear)
toadd = data.census[toadd_index,]
alladd <- NULL

if (nrow(toadd) > 0)
{
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
			cat(sprintf('Added tag %s into year %d\n',toadd[i,"Tag"], toadd[i,"Year"]))
		}
		alladd <- rbind(alladd, tempadd)
	}
	data.census <- rbind(data.census, alladd)
	data.census$Is.seedling[toadd_index] <- 0 # can't be a seedling in the current year
} else
{
	cat('Nothing to add.\n')
}


# check for things that are seedlings in more than one year
dc_noprelim <- data.census
dc_noprelim <- dc_noprelim[-which(dc_noprelim $Preliminary.seedling.census==1),]
dc_noprelim <- dc_noprelim[-which(dc_noprelim $Died.this.census ==1),]

multiseed <- tapply(dc_noprelim $Is.seedling, dc_noprelim $Tag, sum)
multiseed <- multiseed[multiseed > 1]
stopifnot(length(multiseed) == 0)
multiseed



# check for strange sizes and locations (not necessarily problems)
subset(data.census,(data.census $Length..cm. <0 | data.census $Length..cm. > 150) & Died.this.census!=1)
subset(data.census,(data.census $Height..cm. <0 | data.census $Height..cm. > 35) & Died.this.census!=1)
subset(data.census,abs(data.census $Tag.offset.x..cm.) > 50)
subset(data.census,abs(data.census $Tag.offset.y..cm.) > 50)
subset(data.census,data.census $Height..cm. ==0 & data.census $Length..cm. > 0)
subset(data.census,data.census $Height..cm. >0 & data.census $Length..cm. == 0)


# look for missing data
subset(data.census,is.na(X..Capitulescences))
subset(data.census,is.na(Height..cm.))
subset(data.census,is.na(Length..cm.))




# overwrite info for more recently-determined censuses
for (tag in unique(data.census$Tag))
{
	
	dcss_current_ids <- which(data.census$Tag==tag)
	
	currentyear <- max(data.census$Year[dcss_current_ids])
	
	dcss_thisyear_ids <- which(data.census$Tag==tag & data.census$Year==currentyear & data.census$Preliminary.seedling.census!=1)

	# make sure we only have one entry for the most current version of the tag
	stopifnot(length(dcss_thisyear_ids)<=1)

	if (length(dcss_thisyear_ids)==1)
	{	
	  cat('.')
	  #cat(sprintf('Overwriting info for tag %s with info from year %d\n',tag,currentyear))
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
		print(sprintf("Too much info for tag %s, %d %d - ignoring (fix eventually)", tag, length(dcss_current_ids), length(dcss_thisyear_ids)))	
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
data.census <- data.census[order(data.census$Year, data.census$Preliminary.seedling.census, data.census$Plot, data.census$Tag),]










# make list of dead tags to pull at the end of the year
identify_dead_tags <- function(year.now, year.past)
{
	dead_tags <- vector(mode="list",length=(year.now-year.past))
	for (year in year.past:year.now)
	{
	  # flag zero-size plants
		tags_dead_this_year <- data.census[which(data.census$Year==year & data.census$Length..cm.==0),"Tag"]
		dead_tags[[year-year.past+1]] <- tags_dead_this_year
	}
	# find tags that are dead in all the requested years
	dead_all_years <- do.call('intersect',dead_tags)
	
	# add ones needing pulling
	needs_pulling <- data.census[which(data.census$Year==year & grepl("pull",data.census$Notes)),"Tag"]
	
	dead_all_years <- unique(c(dead_all_years, needs_pulling))
	
	# make a data frame of current year locations for the dead tags
	ids_pull <- data.census$Tag[which(data.census$Tag %in% dead_all_years & data.census$Year==year.now)]
	
	return(ids_pull)
}
# first year of pulling
dead_tags_2017 <- identify_dead_tags(2017, 2016)
# find tags needing pulling in second year only, i.e. the ones that are unique to the current year
dead_tags_2018 <- setdiff(identify_dead_tags(2018, 2017),dead_tags_2017)

# flag removed tags
data.census$Tag.removed.in.year[data.census$Tag %in% dead_tags_2017 & data.census$Year %in% c(2017,2018)] <- 2017
data.census$Tag.removed.in.year[data.census$Tag %in% dead_tags_2018 & data.census$Year %in% c(2018)] <- 2018



# prepare a 'pull list'
dead_tags_2018 <- data.census[which(data.census$Tag.removed.in.year %in% c(2017,2018) & data.census$Year==2018),]

write.csv(dead_tags_2018[,c("Year","Plot","Tag","Taxon","X..cm.","Y..cm.","Tag.offset.x..cm.","Tag.offset.y..cm.","Is.seedling","Number.of.individuals.represented.by.tag","Notes","Tag.removed.in.year")], "dead_tags_2018.csv",row.names=FALSE)

#
warning('Need to go to field to remove these dead tags. Recommend conditional formatting on year pulled.')










# generate clean data
makecleandf <- function(year,prelimseedlingsforyear, includepreviousyearnotes)
{
	datass_seed_no <- subset(data.census, Year %in% year & Preliminary.seedling.census==0)
	datass_seed_yes <- subset(data.census, Year %in% prelimseedlingsforyear & Preliminary.seedling.census==1)

	print(nrow(datass_seed_no))
	print(nrow(datass_seed_yes))

	datass <- rbind(datass_seed_no, datass_seed_yes)
	
	# remove all pullable tags to reflect only tags that are in the field
	datass <- datass[is.na(datass$Tag.removed.in.year) | datass$Tag.removed.in.year > year,]
	
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


# make clean dataframes
for (year in 2014:2018)
{
  # write census
  dfcurrent <- makecleandf(year=year, prelimseedlingsforyear=NULL, includepreviousyearnotes=FALSE)
  # export file
  write.csv(dfcurrent, file=sprintf('census %d clean.csv',year),row.names=F,na="")  
  
  # write abundances
  dfcurrent_alive <- dfcurrent[dfcurrent$Length..cm. > 0,]
  abundancescurrent <- do.call("rbind",by(dfcurrent_alive$Taxon, dfcurrent_alive$Plot, table))
  row.names(abundancescurrent) <- paste("Plot",row.names(abundancescurrent))
  
  write.csv(abundancescurrent,sprintf('abundances %d clean.csv',year))
}



# summarize plots
library(reshape2)
library(ggplot2)

summary_stats <- melt(do.call("rbind",by(data.census, data.census$Year, function(x) { 
  data.frame(
    Number.seedlings=length(which(x$Is.seedling==1)),
    Number.mortality.events=length(which(x$Died.this.census==1)),
    Number.fecundity.events=length(which(x$X..Capitulescences>0)),
    Number.total=length(which(x$Length..cm.>0)),
    Year=x$Year[1]
  )})),id.vars="Year")

# NA the mortality events in the 1st year of the study since we can't know this!
summary_stats[summary_stats$variable=="Number.mortality.events" & summary_stats$Year==2014,"value"]<-NA

g <- ggplot(summary_stats,aes(x=Year,y=value,col=variable)) + geom_line(size=2) + facet_wrap(~variable,scales='free') + theme_bw() + ylab("Count") + theme(legend.position = "none")
ggsave(g,file='demography trends.pdf',width=7,height=5)


species_counts <- as.data.frame(do.call("rbind",by(data.census,data.census$Year, function(data_census) {
  do.call("rbind",by(data_census, data_census$Taxon, function(x) { Taxon=data.frame(x$Taxon[1],Count=length(which(x$Length..cm. > 0)),Year=x$Year[1]) }))
  
})))

row.names(species_counts) <- NULL
names(species_counts) <- c("Taxon","Count","Year")
species_counts <- species_counts[order(species_counts$Taxon, species_counts$Year),]

g_counts <- ggplot(species_counts,aes(x=Year,y=Count,col=Taxon)) + geom_line(show.legend = FALSE) + geom_point(show.legend = FALSE) + theme_classic() + ylab('Abundance') + facet_wrap(~Taxon,scales='free')
ggsave("abundance trends.pdf",g_counts,width=10,height=8)
















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


drawplot <- function(data.census, plotid, dataforyear, prelimseedlingsforyear,xlim=c(-10,210),ylim=c(-10,210), specieshighlight=NA, title="",axes=TRUE,names=TRUE)
{
	
	datass_seed_no <- subset(data.census, Year %in% dataforyear & Preliminary.seedling.census==0 & Plot==plotid)
	datass_seed_yes <- subset(data.census, Year %in% prelimseedlingsforyear & Preliminary.seedling.census==1 & Plot==plotid)

	plotdata <- rbind(datass_seed_no, datass_seed_yes)
	plotdata <- plotdata[is.na(plotdata$Tag.removed.in.year) | plotdata$Tag.removed.in.year > year,]
	
	print(plotid)

	plot(0,0,type='n',xlim=xlim,ylim=ylim,axes=axes)

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
		  
		  if (names==TRUE)
		  {
			  segments(plotdata$X..cm.[i]+0,plotdata$Y..cm.[i]+0,plotdata$X..cm.[i]+plotdata$Tag.offset.x[i],plotdata$Y..cm.[i]+plotdata$Tag.offset.y[i],col= whichcolor_text,lwd=0.5)
			  text(plotdata$X..cm.[i]+plotdata$Tag.offset.x[i],plotdata$Y..cm.[i]+plotdata$Tag.offset.y[i],plotdata$Tag[i],col= whichcolor_text,cex=0.4,font=2)
			  text(plotdata$X..cm.[i],plotdata$Y..cm.[i],getcode(plotdata$Taxon[i]),col= whichcolor_text,cex=0.4,font=2)
		  }
			
		}
	}
	if (names==TRUE)
	{
	  mtext(text=sprintf("Plot %d %s",plotid,title),line=0,cex=0.75)	
	}
	box()
}

# make plots over space
for (year in 2014:2018)
{
  pdf(width=5,height=5,file=sprintf('census map %d.pdf',year))
  par(mar=c(3,3,1,1))
  par(cex.axis=0.5)
  for (i in 1:50)
  {
    drawplot(data.census, i, year,NULL)
  }
  dev.off()  
  
  pdf(width=5,height=5,file=sprintf('census map zoomed %d.pdf',year))
  par(mar=c(3,3,1,1))
  par(cex.axis=0.5)
  for (i in 1:50)
  {
    drawplot(data.census, i, year,NULL,xlim=c(-10,110),ylim=c(-10,110),title="Lower left")
    drawplot(data.census, i, year,NULL,xlim=c(90,210),ylim=c(-10,110),title="Lower right")
    drawplot(data.census, i, year,NULL,xlim=c(-10,110),ylim=c(90,210),title="Upper left")
    drawplot(data.census, i, year,NULL,xlim=c(90,210),ylim=c(90,210),title="Upper right")
  }
  dev.off()  
}

# make plots over time
for (plot in 1:50)
{
  pdf(width=5,height=5,file=sprintf('census time series plot %d.pdf',plot))
  par(mar=c(3,3,1,1))
  par(oma=c(0,0,3,3))
  par(cex.axis=0.5)
  for (year in 2014:2018)
  {
    drawplot(data.census, plot, year,NULL)
    mtext(side=3,year,line=2)
  }
  dev.off()  
}


pdf(width=5*2,height=10*2,file='census site time series.pdf')
par(mfrow=c(10,5))
par(mar=c(1,1,1,1))
par(oma=c(0,0,4,4))
for (year in 2014:2018)
{
  for (i in 1:50)
  {
    drawplot(data.census, i, year,NULL,axes=F,names=T)
  }
  mtext(side=3,sprintf("%d",year),font=2,outer=TRUE)
}
dev.off()