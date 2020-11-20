#To do
#Add number of recruits output
#Make sure script is filtering out plants that have been dead for >2 years. In the raw file there are many entries of dead plants in 2018 that died in 2016
#Handle 2020 preliminary seedling census
       #Not removeing tags in prelim census that are dead in the full census 
       #Calculte positions of 2020 seedling patches
#Standardize merges. For a tag getting merged:
         #1) Size info (LHC) should all equal zeros
         #2) Tag should not exist=0
         #3) # of individuals represented by tag=0
#Standardize dead plants
         #1) # of individuals represented by tag=0


library(dplyr)
library(tidyr)
library(readxl)
library(devtools)
library(reshape2)
library(ggplot2)


set.seed(1)

# set year
current_year = 2020

data.census <- as.data.frame(read_excel(sprintf("plant census 2014-%d.xlsx",current_year),sheet=1, col_types=
                                          c("numeric", # Year
                                            "text", # Date 
                                            "text", # Census.team
                                            "numeric", # Plot
                                            "text", # Tag
                                            "text", # Morpho name
                                            "numeric", # Id.uncertain
                                            "numeric", # X
                                            "numeric", # Y
                                            "numeric", # Xoff
                                            "numeric", # Yoff
                                            "numeric", # Died.this.census
                                            "numeric", # Tag.should.not.exit
                                            "text", # Merge.tag.with
                                            "numeric", # Length
                                            "numeric", # Height
                                            "numeric", # Num.Capit
                                            "numeric", # Is.seedling
                                            "numeric", # Should.be.in.starting.year
                                            "numeric", # Num.indivs
                                            "numeric", # Length.indiv
                                            "numeric", # Height.indiv
                                            "text", # Notes
                                            "numeric", # Prelim.census
                                            "text", #needs.check
                                            "text", #check.reason
                                            "text", #Resolution
                                            "text", #Resolved
                                            "numeric", #Check.downstream.that.this.is.resolved
                                            "numeric", # Tag.removed.in.year
                                            "text", #Photo number
                                            "text", #Sub-tag
                                            "numeric", #Page
                                            "numeric" #Row
                                          )))


names(data.census) <- make.names(names(data.census))

data.species <- as.data.frame(read_excel(sprintf("plant census 2014-%d.xlsx",current_year),sheet=3)); names(data.species) <- make.names(names(data.species))

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

# check for spurious removals that will cause overwrite issues
problem_tagremovals <- data.census %>% 
  group_by(Tag) %>% 
  summarize(n.remove.requests=sum(Tag.should.not.exist,na.rm=TRUE),n.years=1+max(Year)-min(Year),n.rows=length(Year)) %>% 
  filter(n.years != n.rows) %>% # if the tag is 
  filter(n.remove.requests > 1)

print(problem_tagremovals)
stopifnot(nrow(problem_tagremovals)==0)
# action required at this stage


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

#!!!!!!!!!!!!!!!!!!!!
#Here, rather than caluculate an individual patch size, for the prelim census I am taking a unique tag number from that row. All duplicates are in the prelim seedling census

data.census.test<-data.census%>% distinct(Tag, Year, Preliminary.seedling.census, .keep_all = TRUE) # Looks good
#Testing that I didn't affect other parts of the dataset
nrow(filter(data.census.test, Year == "2014"))==nrow(filter(data.census, Year == "2014"))
nrow(filter(data.census.test, Year == "2015"))==nrow(filter(data.census, Year == "2015"))
nrow(filter(data.census.test, Year == "2016"))==nrow(filter(data.census, Year == "2016"))
nrow(filter(data.census.test, Year == "2017"))==nrow(filter(data.census, Year == "2017"))
nrow(filter(data.census.test, Year == "2018"))==nrow(filter(data.census, Year == "2018"))
nrow(filter(data.census.test, Year == "2019"))==nrow(filter(data.census, Year == "2019"))
nrow(filter(data.census.test, Year == "2020" & Preliminary.seedling.census==0))==nrow(filter(data.census, Year == "2020" & Preliminary.seedling.census==0))
nrow(filter(data.census.test, Year == "2020" & Preliminary.seedling.census==1))==nrow(filter(data.census, Year == "2020" & Preliminary.seedling.census==1))



data.census<-data.census%>% distinct(Tag, Year, Preliminary.seedling.census, .keep_all = TRUE) # Looks good. Re-running the duplicate code above

# check for duplicates again (needed for hack)
duplicates <- checkduplicates(data.census)
stopifnot(nrow(duplicates)==0)



#!!!!!! The below overwrites on dead plants would be better to do later in the code after determining died this census final

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


#!!!! Need to use Died.this.census.final for sorting
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



# set 0-tag patches to 1-tag patches. Every tag represents one individual, even if dead
data.census$Number.of.individuals.represented.by.tag[data.census$Number.of.individuals.represented.by.tag==0] = 1


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

#Up to this point number of size 0 seedlings appears correct
nrow(data.census %>% filter(Preliminary.seedling.census==0 & Is.seedling==1 & Year==current_year & Length..cm.==0)) #163

#Died this census appears to be appropriately entered for size 0 seedlings
nrow(data.census %>% filter(Length..cm.==0 & Is.seedling==1 & Year==current_year & Died.this.census==1)) #163
nrow(data.census %>% filter(Length..cm.==0 & Is.seedling==1 & Year==current_year & Died.this.census==0)) #0

#Some seedlings that have a size >0 are listed as dying this census, these are all from the preliminary census where dead seedlings were counted
nrow(data.census %>% filter(Length..cm.>0 & Is.seedling==1 & Year==current_year & Died.this.census==1)) #13
nrow(data.census %>% filter(Length..cm.>0 & Is.seedling==1 & Year==current_year & Died.this.census==0)) #425

# propagate mortality and sizes from prelim to later census
data.census= data.census %>% group_by(Tag,Year) %>% mutate(Died.this.census = max(Died.this.census),
                                                  #Length..cm.=max(Length..cm.), # don't overwrite sizes
                                                  #Height..cm.=max(Height..cm.), # don't overwrite sizes
                                                  X..Capitulescences=max(X..Capitulescences))

#Here is where we lose size 0 seedlings
nrow(data.census %>% filter(Preliminary.seedling.census==0 & Is.seedling==1 & Year==current_year & Length..cm.==0)) # 163


get_dead_years <- function(Year, Length, Preliminary.seedling.census)
{
  df = data.frame(Year,Length, Preliminary.seedling.census) %>% arrange(Year)
  
  # use only final census data
  df = df %>% filter(Preliminary.seedling.census==0)
 
  if (nrow(df) > 1)
  {
    df$flag.year.died = ((df$Length==0) & (c(NA,df$Length[1:(length(df$Length)-1)])==0))
    
    if(all(df$flag.year.died==FALSE))
    {
      first.year.died = NA
    }
  else
  {
    first.year.died = min(df$Year[df$flag.year.died],na.rm=T)
    #year.died = df$Year[df$flag.year.died]
  }
  }
  else
  {
    first.year.died = NA
  }
  return(first.year.died)
}

get_dead_seedling_year <- function(df)
{
  result = ifelse(df$Preliminary.seedling.census==0 & df$Is.seedling==1 & df$Length..cm.==0,df$Year, NA)
  return(result)
}

 
# fix by hand
data.census$Length..cm.[data.census$Year==2016 & data.census$Tag=="740"] = data.census$Length..cm.[data.census$Year==2017 & data.census$Tag=="740"]
data.census$Length..cm.[data.census$Year==2016 & data.census$Tag=="756"] = data.census$Length..cm.[data.census$Year==2017 & data.census$Tag=="756"]
data.census$Length..cm.[data.census$Year==2016 & data.census$Tag=="99"] = data.census$Length..cm.[data.census$Year==2017 & data.census$Tag=="99"]


# find tags dead two years or seedlings dead this year
data.census = data.census %>% 
  group_by(Tag) %>% 
  mutate(Year.absent.twice=get_dead_years(Year,Length..cm.,Preliminary.seedling.census)) %>% 
  mutate(Died.this.census.final = ifelse(is.na(Year.absent.twice), FALSE, (Year==(Year.absent.twice-1)))) %>%
  ungroup %>% 
  mutate(Year.seedling.dead=get_dead_seedling_year(.)) %>% # must come after ungroup as we are not doing within tags
  mutate(Remove = (Year>=Year.absent.twice) | (Year >= Year.seedling.dead)) %>% # set final remove flag
  mutate(Remove=ifelse(is.na(Remove), FALSE, Remove))

# also add died.this.census.final if it is a dead seedling this time
rowids_deadseedling = which((data.census$Year==data.census$Year.seedling.dead))
data.census$Died.this.census.final[rowids_deadseedling] = TRUE
data.census$Died.this.census.final = as.numeric(data.census$Died.this.census.final)

# find tags that have died and come back to life
zombie_tags = data.census %>% group_by(Tag) %>% summarize(Problem=any(Remove==TRUE & Length..cm.>0)) %>% filter(Problem==TRUE) %>% select(Tag)
# show problems
data.census %>% filter(Tag %in% zombie_tags$Tag) %>% select(Year, Tag, Length..cm.) %>% arrange(Tag,Year) 

# repeat until nothing shows up as zombies





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













# make list of tags to remove
tags_to_remove_this_year = data.census %>% 
  filter(Remove==TRUE & Year==current_year & Preliminary.seedling.census==0) %>% 
  select(Plot, Tag, X..cm., Y..cm., Tag.offset.x..cm., Tag.offset.y..cm., Year.absent.twice, Year.seedling.dead,Is.seedling, Notes) %>% 
  unique %>% 
  arrange(Plot, Tag)


write.csv(tags_to_remove_this_year, file=sprintf('tags_to_remove_thisyear_%d.csv',current_year),row.names=F)
# assume all tags removed when they satisfy the criterion
data.census$Tag.removed.in.year = apply(cbind(data.census$Year.absent.twice,data.census$Year.seedling.dead,Inf),1,min,na.rm=T)

# generate clean data
makecleandf <- function(year,prelimseedlingsforyear, includepreviousyearnotes, includedeadseedlings)
{
	datass_seed_no <- subset(data.census, Year %in% year & Preliminary.seedling.census==0)
	datass_seed_yes <- subset(data.census, Year %in% prelimseedlingsforyear & Preliminary.seedling.census==1)

	datass <- rbind(datass_seed_no, datass_seed_yes)
	
	# remove all pullable tags to reflect only tags that are in the field
	if (includedeadseedlings==TRUE)
	{
	  whichrows = setdiff(1:nrow(datass),which(datass$Year >= datass$Year.absent.twice))
	  datass <- datass[whichrows,] # don't include the seedling criterion
	}
	else
	{
	  whichrows = setdiff(1:nrow(datass),which( ((datass$Year>=datass$Year.absent.twice) | (datass$Year >= datass$Year.seedling.dead)) ))
	  datass <- datass[whichrows,] # don't include the seedling criterion
	}
	
	colnames <- c("Year",
	              "Plot",
	              "Tag",
	              "Morphoname",
	              "Taxon",
	              "Identification.uncertain",
	              "X..cm.",
	              "Y..cm.",
	              "Tag.offset.x..cm.",
	              "Tag.offset.y..cm.",
	              "Died.this.census", # whether it was field-observed dead
	              "Died.this.census.final", # whether the census decides it is dead based on 2 years data
	              "Length..cm.",
	              "Height..cm.",
	              "X..Capitulescences",
	              "Is.seedling",
	              "Number.of.individuals.represented.by.tag",
	              "Length.per.individual..cm.",
	              "Height.per.individual..cm.",
	              "Notes")
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
	
	print(nrow(datass))
	
	return(datass)	
}


# make clean dataframes
for (year in 2014:current_year)
{
  # write census
  dfcurrent_fieldtags <- makecleandf(year=year, prelimseedlingsforyear=NULL, includepreviousyearnotes=FALSE, includedeadseedlings=FALSE)
  # export file
  write.csv(dfcurrent_fieldtags, file=sprintf('census %d clean only field tags.csv',year),row.names=F,na="")  
  
  dfcurrent_deadseedlings <- makecleandf(year=year, prelimseedlingsforyear=NULL, includepreviousyearnotes=FALSE, includedeadseedlings=TRUE)
  # export file
  write.csv(dfcurrent_deadseedlings, file=sprintf('census %d clean with dead seedlings.csv',year),row.names=F,na="")  
  
  
  # write abundances
  dfcurrent_alive <- dfcurrent_fieldtags[dfcurrent_fieldtags$Length..cm. > 0,]
  abundancescurrent <- do.call("rbind",by(dfcurrent_alive$Taxon, dfcurrent_alive$Plot, table))
  row.names(abundancescurrent) <- paste("Plot",row.names(abundancescurrent))
  
  write.csv(abundancescurrent,sprintf('abundances %d clean only field tags.csv',year))
}






# write out trasnformed dataset for demography
# in this case do include dead seedlings to note they existed
df_clean_all = do.call("rbind",lapply(2014:current_year, makecleandf, prelimseedlingsforyear=NULL, includepreviousyearnotes=FALSE, includedeadseedlings=TRUE))
df_clean_all %>% arrange(Year, Plot) %>% select(Year, 
                                               Plot, 
                                               Tag, 
                                               Taxon, 
                                               X..cm., 
                                               Y..cm., 
                                               Length..cm., 
                                               Height..cm., 
                                               X..Capitulescences, 
                                               Is.seedling, 
                                               Died.this.census.final,
                                               Number.of.individuals.represented.by.tag,
                                               Length.per.individual..cm.,
                                               Height.per.individual..cm.)

# need to assign each individual a new tag (maybe a * suffix?)
# locate tags randomly within the patch
# 
# need to divide up the flowers among individuals

allocate_integer <- function(total, num_bins)
{
  #print(c(total, num_bins))
  
  vals = rep(0, num_bins)
  for (i in 1:total)
  {
    index = (i) %% (num_bins+1)
    vals[index] = vals[index] + 1
  }
  return(vals)
}

letters_all = c(LETTERS,
                paste(LETTERS, LETTERS, sep=""),
                paste(LETTERS, LETTERS, LETTERS, sep=""),
                paste(LETTERS, LETTERS, LETTERS, LETTERS, sep="")
                )


# make pseudorandom points on a disc
# devtools::install_github("will-r-chase/poissondisc")
require(poissondisc)
set.seed(1)
offsets = poisson_disc(2, 2, 0.05)
offsets$x = offsets$x - 1
offsets$y = offsets$y - 1
offsets = offsets %>% filter(sqrt(x^2+y^2) < 1)
offsets = offsets[sample(1:nrow(offsets)),]
offsets$index = 1:nrow(offsets)
row.names(offsets) = NULL

replicate_indivs <- function(df)
{
  max.indivs = max(df$Number.of.individuals.represented.by.tag)
  # if this is never a patch
  if (max.indivs<=1) # including the case of 0 indivs
  {
    # give a tag suffix to track patches that become non-patches
    df_out = df %>% mutate(Tag=paste(Tag,"A",sep="*"))
  }
  else
  {
    # repeat data frame for as many indivs as we have
    df_out = df[rep(1,max.indivs),] %>% 
      # give each tag a new name
      # first indiv keeps original tag; all others get letter codes
      mutate(TagNew=paste(Tag,letters_all[1:max.indivs],sep="*")) %>%
      # give new locations
      #mutate(rho = (Length..cm./2) * sqrt(runif(max.indivs)), theta=runif(max.indivs,0,2*pi)) %>%
      #mutate(X..cm. = X..cm. + rho*cos(theta), Y..cm. = Y..cm. + rho*sin(theta)) %>%
      mutate(X..cm. = X..cm. + (Length..cm./2)*offsets$x[1:length(Length..cm.)], 
             Y..cm. = Y..cm. + (Length..cm./2)*offsets$y[1:length(Length..cm.)]) %>%
      # allocate the flowers among individuals
      mutate(X..Capitulescences = allocate_integer(X..Capitulescences[1], max.indivs)) %>%
      # give the individual lengths
      mutate(Length..cm. = Length.per.individual..cm.) %>%
      # give the individual heights
      mutate(Height..cm. = Height.per.individual..cm.) %>%
      ungroup %>%
      mutate(Tag=TagNew)

  }
  
  # finalize the columns
  df_out = df_out %>% 
    select(Year, 
           Plot,
           Tag, 
           Taxon, 
           X..cm., Y..cm., 
           Tag.offset.x=Tag.offset.x..cm., Tag.offset.y=Tag.offset.y..cm.,
           Length..cm., Height..cm., 
           X..Capitulescences, 
           Is.seedling, 
           Died.this.census.final)
    #select(-TagNew, -rho, -theta, -Length.per.individual..cm., -Height.per.individual..cm.)
  
  return(df_out)
}

# pad out all the individuals to make an occurrence dataset
df_individual_panel = df_clean_all %>% group_by(Tag, Year) %>% do(replicate_indivs(.))

# make demography data
df_individual_demo = df_individual_panel %>% 
  ungroup %>% group_by(Tag) %>%
  # get previous size
  mutate(Length..cm..prev = c(NA,head(Length..cm.,-1))) %>%
  # calculate growth as differences in sizes
  mutate(Growth = Length..cm. - Length..cm..prev) %>%
  # calculate survival as whether did not die
  mutate(Survival = as.numeric(!Died.this.census.final))
  

#myplant = df_individual_demo %>% filter(Tag=="1061") %>% View

write.csv(df_individual_demo, file=sprintf('data_demography_%d.csv', current_year), row.names=F)














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


drawplot <- function(data.census, plotid, dataforyear, xlim=c(-10,210),ylim=c(-10,210), specieshighlight=NA, title="",axes=TRUE,names=TRUE)
{
  plotdata <- data.census %>% filter(Year==dataforyear & Plot==plotid)

  #print(str(plotdata))
  
  print(plotid)
  
  # set NAs to 0 for plotting
  plotdata$Length..cm.[is.na(plotdata$Length..cm.)] = 0
  
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
      if (plotdata$Died.this.census.final[i]==1)
      {
        whichcolor <- "#000000"
      }
      
      drawcircle(plotdata$X..cm.[i],plotdata$Y..cm.[i],plotdata$Length..cm.[i]/2,bcol=whichcolor,col= whichcolor_light)

      
      if(plotdata$Died.this.census.final[i]==1)
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
for (year in 2014:current_year)
{
  pdf(width=5,height=5,file=sprintf('census map %d.pdf',year))
  par(mar=c(3,3,1,1))
  par(cex.axis=0.5)
  for (i in 1:50)
  {
    drawplot(df_individual_panel, i, year)
  }
  dev.off()  
  
  pdf(width=5,height=5,file=sprintf('census map zoomed %d.pdf',year))
  par(mar=c(3,3,1,1))
  par(cex.axis=0.5)
  for (i in 1:50)
  {
    drawplot(df_individual_panel, i, year,xlim=c(-10,110),ylim=c(-10,110),title="Lower left")
    drawplot(df_individual_panel, i, year,xlim=c(90,210),ylim=c(-10,110),title="Lower right")
    drawplot(df_individual_panel, i, year,xlim=c(-10,110),ylim=c(90,210),title="Upper left")
    drawplot(df_individual_panel, i, year,xlim=c(90,210),ylim=c(90,210),title="Upper right")
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
  for (year in 2014:current_year)
  {
    drawplot(df_individual_panel, plot, year)
    mtext(side=3,year,line=2)
  }
  dev.off()  
}


pdf(width=5*2,height=10*2,file='census site time series.pdf')
par(mfrow=c(10,5))
par(mar=c(1,1,1,1))
par(oma=c(0,0,4,4))
for (year in 2014:current_year)
{
  for (i in 1:50)
  {
    drawplot(df_individual_panel, i, year,axes=F,names=T)
  }
  mtext(side=3,sprintf("%d",year),font=2,outer=TRUE)
}
dev.off()







getcoord <- function(plot, corner=NULL, plotcoordx=NULL, plotcoordy=NULL)
{
  coords <- which(matrix(1:50,nrow=10,ncol=5,byrow=TRUE)==plot,arr.ind=TRUE)
  
  xpos <- (coords[2]-1)*400
  ypos <- (coords[1]-1)*(-400)	
  
  if (!is.null(corner) & is.null(plotcoordx) & is.null(plotcoordy))
  {
    offset_x <- NA
    offset_y <- NA
    if (corner=="upper left")
    {
      offset_x <- 0
      offset_y <- 0
    }
    else if (corner=="upper right")
    {
      offset_x <- 200
      offset_y <- 0
    }
    else if (corner=="lower left")
    {
      offset_x <- 0
      offset_y <- -200
    }
    else if (corner=="lower right")
    {
      offset_x <- 200
      offset_y <- -200
    }
    
    xpos <- xpos + offset_x
    ypos <- ypos + offset_y
  }
  else if (!is.null(plotcoordx) & !is.null(plotcoordy) & is.null(corner))
  {
    xpos <- xpos + plotcoordx
    ypos <- ypos - (200 - plotcoordy)
  }
  else
  {
    stop("incorrect input")
  }
  
  return(c(x=xpos, y=ypos))
}

data.census$x <- NA
data.census$y <- NA
for (i in 1:nrow(data.census))
{
  coord <- NULL
  try(coord <- getcoord(plot=as.numeric(data.census$Plot[i]), plotcoordx=data.census$X..cm.[i],plotcoordy=data.census$Y..cm.[i]))
  if (!is.null(coord))
  {
    data.census$x[i] <- coord["x"]
    data.census$y[i] <- coord["y"]
  }
}


plotdata = data.census %>% 
  filter(Year==2019)


pdf(file='~/Downloads/2019.pdf',width=18/2,height=38/2);
par(mar=c(1,1,1,1))
plot(0,0,xlim=c(0,1800),ylim=c(-3800,0),type='n',axes=F,xlab="",ylab="")
for (i in 1:nrow(plotdata))
{
  whichcolor <- rainbow(length(levels(data.census$Taxon)),alpha=0.75)[as.numeric(plotdata$Taxon[i])]
  whichcolor_light <- rainbow(length(levels(data.census$Taxon)),alpha=0.25)[as.numeric(plotdata$Taxon[i])]
  whichcolor_text <- gray(0,0.75)
  
  if (plotdata$Died.this.census.final[i]==1)
  {
    whichcolor <- "#000000"
  }
  
  drawcircle(plotdata$x[i],plotdata$y[i],plotdata$Length..cm.[i]/2,bcol=whichcolor,col= whichcolor_light)
  
  
  if(plotdata$Died.this.census.final[i]==1)
  {
    points(plotdata$x[i],plotdata$y[i],col= whichcolor_text,pch=4,cex=0.5)
  }
  if(plotdata$Length..cm.[i]==0)
  {
    points(plotdata$x[i],plotdata$y[i],col= 'lightgray',pch=0,cex=0.5,lwd=0.5)
  }
  if(plotdata$Is.seedling[i]==1)
  {
    points(plotdata$x[i],plotdata$y[i],col= whichcolor_text,pch=3,cex=0.5)
  }
  if(plotdata$X..Capitulescences[i]>0)
  {
    points(plotdata$x[i],plotdata$y[i],col= whichcolor_text,pch=6,cex=0.5)
  }
  
  # show zero-size (dead) individuals in different colors
  if (plotdata$Length..cm.[i]==0)
  {
    whichcolor_text = 'lightgray'
  }
  
}

# draw boxes
for (i in 1:50)
{
  box_coords=as.data.frame(t(sapply(c("upper left","upper right","lower right","lower left"),getcoord,plot=i)))
  polygon(box_coords$x,box_coords$y)
}

# draw ghost plants
data_dead = data.census %>% filter(Died.this.census.final==1 & Year != 2019)

points(data_dead$x, data_dead$y,col=gray(1 - (data_dead$Year - 2012)/(2021 - 2012),alpha=0.5),pch=4,cex=0.5)

dev.off()

















# do demography plots
summary_stats = df_individual_demo %>% 
  group_by(Year, Taxon) %>%
  summarize(
    Number.mortality.events=length(which(Died.this.census.final==1)),
    Mean.size.change = mean(Length..cm. - Length..cm..prev,na.rm=T),
    Number.fecundity.events=length(which(X..Capitulescences>0)),
    Number.seedlings=length(which(Is.seedling==1)),
    Number.total=(length(which(Length..cm.>0)) + Number.mortality.events)) # also include the dead ones from this year in the total abundance, so mortality rates are between 0 and 1

# NA the mortality events and growth in the 1st year of the study since we can't know this!
summary_stats[summary_stats$Year==2014,c("Number.mortality.events")]<-NA
summary_stats[summary_stats$Year==2014,"Mean.size.change"]<-NA

# remove the NA
summary_stats = summary_stats %>% 
  filter(!Taxon %in% c("Indet indet","NA NA")) %>% ungroup

summary_stats = summary_stats %>% 
  mutate(Survival.rate = Number.mortality.events / Number.total) %>%
  mutate(Flowering.probability = Number.fecundity.events / Number.total) %>%
  select(-Number.mortality.events,-Number.fecundity.events)

# change format
summary_stats_short = gather(summary_stats, key='variable',value='value',Mean.size.change:Flowering.probability) %>%
  mutate(Taxon=as.character(Taxon)) %>%
  mutate(variable=factor(variable,levels=c("Survival.rate","Mean.size.change","Flowering.probability","Number.seedlings","Number.total"),ordered=TRUE))

g_demo = ggplot(summary_stats_short, aes(x=Year,y=value,col=Taxon)) + 
  geom_line() + 
  geom_point() +
  geom_abline(slope=0,intercept=0) +
  facet_wrap(~variable,scales='free_y',labeller=labeller(variable=c(Survival.rate="A) Survival rate",
                                                                    Mean.size.change='B) Size change, mean (cm)',
                                                                    Flowering.probability='C) Flowering probability',
                                                                    Number.seedlings='D) Recruitment, total (#)',
                                                                    Number.total='E) Abundance, total (#)'))) +
  theme_bw() +
  #scale_y_continuous(trans='pseudo_log') +
  #theme(legend.position='bottom') +
  ylab("Value") +
  theme(legend.text = element_text(face = 'italic'))

ggsave(g_demo, file='g_demo.pdf',width=10,height=5)

