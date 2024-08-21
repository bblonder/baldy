# To do spring 2023

# fix locations for plants with odd georeferencing

# Add something for ‘first year in dataset’ for plots so that we don’t erroneously get infinite growth for the R plots

# Check if 470 = 2826?

  # Convert script to report summary stats as per unit land area, not as total counts now that area changes in each year
  # done 7/23/23


  # Update QC script to handle R-side plots
  # done 7/23/23

# Decide how to handle ‘should be in last year’ plants - in prior years do we assume first year of appearance is as a seedling? 
# This might be challenging for plants that have vegetative resprouts. 

# Also if we include seedlings in prior year what do we do with their sizes. Check!


# To do fall 2022
#Script needs to rename tag assignments for clustering data (eg if a host is retagged, make sure the inside/outside data follow)
## punting on this until later - CR agrees
  
  #Check aspect ratios in qc script
  ## BB now doing this - 5 plants are flagged. CR to manually confirm they are OK - they look OK to me. cntrl-F for 'aspect' and you will find the code #*CR confirmed that these are not errors on 12/13/21. There are now 9 individuals with large aspect ratios

#Need to standardize whether replaced rows get a tag should not exist and if an additional row should be added. 
# ** BB needs an example of this - believes the data are being handled OK. CR to help
  
  #Merge where plant appears in the prelim census, i.e. 1136
  # ** BB fixed this 8/16 - this was a case where the merge occurred in a prelim but not later, and then tag.should.not.exist was not properly set. this is now error checked and handled automatically
  
  #identify zombies that are dead two years and then are back. 
  # ** BB decided these should require manual correction, not a script fix. the script now flags these in the zombie section
  
  # figure out why 2918 has incorrect starting year of 2014 not 2021
  # ** BB this was a big bug in how the 'should be in starting year' code was handled - indexed by 1, not by i so not looping correctly. this is now fixed throughout the script and will affect many plants.
  
  # figure out why 1507 does not appear in 2021 census data (1505 was merged into 1507, so 1505 should have disappeared, not 1507)
  # ** BB cannot duplicate this issue after fixing some other issues - seems to be OK Now. CR to manually confirm.
  
  #Make sure script is filtering out plants that have been dead for >2 years. In the raw file there are many entries of dead plants in 2018 that died in 2016
  # ** BB believes this should be dealt with by the script, CR to manually confirm
  
  #Add number of recruits to the demography output
  # ** BB implemented this as plants with NA prev year size and non-seedlings after 2014, please check if working

library(dplyr)
library(tidyr)
library(readxl)
library(devtools)
library(ggplot2)
library(reshape2)
library(pracma)

# create output directory
if (!file.exists('outputs'))
{
  dir.create('outputs')
}

# set seed
set.seed(1)

# set year
current_year = 2022

data.census <- as.data.frame(read_excel(sprintf("plant census 2014-%d.xlsx",current_year),sheet=1, col_types=           c("numeric", # Year
         "text", # Month 
         "text", # Day 
         "text", # Census.team
         "text", # Plot
         "text", # Tag
         "text", # Morpho name
         "numeric", # Id.uncertain
         "numeric", # X
         "numeric", # Y
         "numeric", # Xoff
         "numeric", # Yoff
         "numeric", # Died.this.census
         "numeric", # Tag.should.not.exist
         "text", # Merge.tag.with
         "text", #Part of a merge
         "numeric", # Length
         "numeric", # Height
         "numeric", # Num.Capit
         "numeric", # Is.seedling
         "numeric", # Should.be.in.starting.year
         "numeric", # Num.indivs
         "numeric", # Length.indiv
         "numeric", # Height.indiv
         "text", # In cluster
         "text", # Dom tag in cluster 
         "text", #Is ant nest
         "text", # Notes
         "numeric", # Prelim.census
         "text", #needs.check
         "text", #check.reason
         "text", #Resolution
         "text", #Resolved
         "numeric", #Check.downstream
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
  	if(!(data.census$Number.of.individuals.represented.by.tag[merge_id[i]]<=1 | is.na(data.census$Number.of.individuals.represented.by.tag[merge_id[i]]))) { warning('tried to merge seedling patch') }
  	
  	### NEW 7/29/2021
  	# clear out some info for the old plant
  	# set size info to zero
  	data.census$Length..cm.[merge_id[i]] <- 0
  	data.census$Height..cm.[merge_id[i]] <- 0
  	data.census$X..Capitulescences[merge_id[i]] <- 0
  	# clear patches
  	data.census$Number.of.individuals.represented.by.tag[merge_id[i]] <- 0
  	
  	# set old tag to be removed
  	data.census$Tag.should.not.exist[merge_id[i]] <- 1
  	
  	cat(sprintf("Merging tag %s into existing tag %s in year %d\n",data.census$Tag[merge_id[i]], data.census$Tag[right_id], data.census$Year[merge_id[i]]))
	}
	else # if there is no tag existing already
	{
	  ids_old <- which(data.census$Tag == data.census$Tag[merge_id[i]])
	  # find all instances of this tag
	  print(data.census[ids_old,])

	  cat(sprintf('Merging tag %s into new tag %s for years %s\n',data.census$Tag[merge_id[i]],data.census$Merge.tag.with[merge_id[i]], paste(data.census[ids_old,"Year"],collapse=", ")))
	  
	  	  
	  # overwrite all of the old tag names with the new 'replace with' tag name
	  data.census[ids_old,"Tag"] <- data.census$Merge.tag.with[merge_id[i]]
	  
	  # set tag should not exist to 0 as we have now effectively deleted the old tag
	  data.census[ids_old,"Tag.should.not.exist"] <- NA
	  
	  print(data.census[ids_old,])
	}
}


# make sure merges are consistent across preliminary and full census
year_tag_prelim_merges = data.census %>% 
  group_by(Year,Tag) %>% 
  summarize(prelim_merge=ifelse(any(!is.na(Merge.tag.with)) & any(!is.na(Preliminary.seedling.census)),1,0)) %>% 
  filter(prelim_merge==1)

for (i in 1:nrow(year_tag_prelim_merges))
{
  print(sprintf('Removing extra prelim seedling merge for %s',paste(year_tag_prelim_merges[1,c("Year","Tag")],collapse="-")))
  data.census[data.census$Year==year_tag_prelim_merges$Year[i] & data.census$Tag==year_tag_prelim_merges$Tag[i] & !is.na(data.census$Merge.tag.with),"Tag.should.not.exist"] <- 1
}

# check for spurious removals that will cause overwrite issues
problem_tagremovals <- data.census %>% 
  group_by(Tag) %>% 
  summarize(n.remove.requests=sum(Tag.should.not.exist,na.rm=TRUE),n.years=1+max(Year)-min(Year),n.rows=length(Year)) %>% 
  filter(n.years != n.rows) %>% # if the tag is 
  filter(n.remove.requests > 1)

print(problem_tagremovals)
if(!nrow(problem_tagremovals)==0) { warning('problem tag removals nonzero') }
# action required at this stage


# remove old tag IDs that should be no longer exist (i.e. because they were merged)
removed_tags <- apply(data.census[which(data.census$Tag.should.not.exist==1),c("Year","Tag")],1,function(x) {sprintf("%s (%s)",x[2],x[1])})
data.census <- data.census[-which(data.census$Tag.should.not.exist==1),]
for (i in 1:length(removed_tags)) 
{ 
  cat(sprintf("Removed tag %s\n", removed_tags[i]))
}


# check to see if there are any morphonames that should not exist
namematches_morphonames <- data.census$Morphoname[! data.census$Morphoname %in% c(data.species$Code)]
if(!all(is.na(namematches_morphonames))) { warning('name matching issue')}
print(unique(namematches_morphonames))


# assign real taxon names
speciesindices <- match(data.census$Morphoname, c(data.species$Code))
data.census <- cbind(data.census, data.species[speciesindices,1:3])
data.census$Taxon <- paste(data.census$Genus, data.census$Species)
data.census$Taxon <- factor(data.census$Taxon, levels=sample(names(sort(table(data.census$Taxon)))))

# add default offset to cells that are blank
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
if(!nrow(duplicates)==0) { warning('duplicates issue')}
duplicates
#
#All duplicates are in the prelim seedling census from 2020-CR confirmed 12/10/21

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
		for (whichyear in toadd$Should.be.in.starting.year[i]:(toadd$Year[i]-1))
		{
			newrecord <- toadd[i,]
			newrecord$Year <- whichyear
			newrecord$Should.be.in.starting.year <- 0
			newrecord$Month <- NA
			newrecord$Day <- NA
			
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
if(!length(multiseed) == 0) { warning('multiyear seedling issue') }
print(multiseed)

# check for sizes that are out of realistic range
subset(data.census,(data.census $Length..cm. <0 | data.census $Length..cm. > 150) & Died.this.census!=1)
subset(data.census,(data.census $Height..cm. <0 | data.census $Height..cm. > 50) & Died.this.census!=1)


subset(data.census,(data.census $Length..cm. <0 | data.census $Length..cm. > 150) & Died.this.census!=0)
subset(data.census,(data.census $Height..cm. <0 | data.census $Height..cm. > 50) & Died.this.census!=0)

# check unrealistic offsets
subset(data.census,abs(data.census $Tag.offset.x..cm.) > 50)
subset(data.census,abs(data.census $Tag.offset.y..cm.) > 50)

# check heightless or lengthless plants that are otherwise alive
subset(data.census,data.census $Height..cm. ==0 & data.census $Length..cm. > 0)
subset(data.census,data.census $Height..cm. >0 & data.census $Length..cm. == 0)

# check aspect ratio
subset(data.census,(data.census $Height..cm. / data.census $Length..cm.) > 10)

#Plot length by height for each taxa and check for outliers
ggplot(data.census, aes(x=Length..cm., y=Height..cm.))+
  geom_point() +
  theme_classic() + 
  facet_grid(Year~Taxon)

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
	if(!length(dcss_thisyear_ids)<=1) { warning('multiple entry issue') }

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

# check for missing taxon names
data.census %>% 
  filter(Taxon=="NA NA")


# look for NAs in various columns that should not have them
pdf(file='outputs/g_missing_data.pdf')
par(mar=c(1,10,1,1))
image(is.na(data.census),xaxt='n',yaxt='n');
axis(side=2,at=seq(0,1,length.out=length(names(data.census))),labels=names(data.census),las=2)
dev.off()

# make sure final format is OK
print(str(data.census))

#There are many size 0 seedlings. All these plants were tagged in either the 2015 or 2020 preliminary seedling census and then were found to be dead in the main census
nrow(data.census %>% filter(Preliminary.seedling.census==0 & Is.seedling==1 & Length..cm.==0)) #193 rows
size0seedling <-data.census %>% filter(Preliminary.seedling.census==0 & Is.seedling==1 & Length..cm.==0)

#Died this census appears to be appropriately entered for size 0 seedlings
nrow(data.census %>% filter(Length..cm.==0 & Is.seedling==1 & Died.this.census==1)) #193 rows
nrow(data.census %>% filter(Length..cm.==0 & Is.seedling==1 & Died.this.census==0)) #0 rows

#Some seedlings that have a size >0 are listed as dying this census, these nearly all from the 2020 preliminary census where dead seedlings were counted. The one exception is a 2016 seedling that is listed as died this census with a non-zero size
nrow(data.census %>% filter(Length..cm.>0 & Is.seedling==1 & Died.this.census==1)) #31 rows
dead.seedlings<-
  filter(data.census, Length..cm.>0 & Is.seedling==1 & Died.this.census==1)
dead.seedlings


# propagate mortality and sizes from prelim to later census
data.census= data.census %>% group_by(Tag,Year) %>% mutate(Died.this.census = max(Died.this.census),
                                                  #Length..cm.=max(Length..cm.), # don't overwrite sizes
                                                  #Height..cm.=max(Height..cm.), # don't overwrite sizes
                                                  X..Capitulescences=max(X..Capitulescences))

#There are no longer any size 0 seedlings
nrow(data.census %>% filter(Preliminary.seedling.census==0 & Is.seedling==1 & Year==current_year & Length..cm.==0)) # 0


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
get_dead_seedling_year

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

### NEW 7/29/2021
data.census$Number.of.individuals.represented.by.tag[which(data.census$Died.this.census.final==1)] <- 1
### END


# find tags that have died and come back to life
zombie_tags = data.census %>% group_by(Tag) %>% summarize(Problem=any(Remove==TRUE & Length..cm.>0)) %>% filter(Problem==TRUE) %>% select(Tag)
zombie_tags_series = data.census %>% filter(Tag %in% zombie_tags$Tag) %>% select(Year, Tag, Length..cm.) %>% arrange(Tag,Year)
# show problems

if(length(zombie_tags$Tag) > 0)
{
  zombie_tags_series %>% group_by(Tag) %>% group_split %>% print
  stop('These tags must be manually fixed in the raw data by renaming them as "-old" in the pre-zombie years')
}

# repeat until nothing shows up as zombies

### NEW 7/29/2021
# find tags that ghosted--i.e. disappeared from the dataset without having been dead for 2 years
find_ghosts <- function(data.census)
{
  tag_sequences = data.census %>% 
      group_by(Tag) %>% 
      arrange(Tag, Year) %>% 
      select(Year, Tag, Length..cm., Died.this.census.final, Merge.tag.with) %>% 
      summarize(has.died=sum(Died.this.census.final), last.year=max(Year), size=paste(Length..cm., collapse = " _ "), final.size=last(Length..cm.), Merge.tag.with=paste(Merge.tag.with,collapse=" - "))
  ghost_tag_sequences = tag_sequences %>% filter(has.died==0 & last.year!=current_year)
  return(ghost_tag_sequences)
}
ghost_tag_sequences = find_ghosts(data.census)
print(ghost_tag_sequences,n=Inf)
write.csv(ghost_tag_sequences, file='outputs/ghost_tag_sequences.csv',row.names=F)

# kill all the ghosts the last year they appeared
data.census$Died.this.census.final[paste(data.census$Tag, data.census$Year) %in% paste(ghost_tag_sequences$Tag,ghost_tag_sequences$last.year)] = 1

warning('These tags stopped appearing in a certain year')
ghost_tag_sequences_final = find_ghosts(data.census)
print(ghost_tag_sequences_final,n=Inf)

### END

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
# write the removal list
write.csv(tags_to_remove_this_year, file=sprintf('outputs/tags_to_remove_thisyear_%d.csv',current_year),row.names=F)

# assume all tags removed when they satisfy the criterion
data.census$Tag.removed.in.year = apply(cbind(data.census$Year.absent.twice,data.census$Year.seedling.dead,Inf),1,min,na.rm=T)

### NEW 7/29/2021
# find dead seedlings in 2020 (special case)
ids_dead_seedlings_2020 = which(data.census$Preliminary.seedling.census==1 & data.census$Year==2020 & data.census$Died.this.census==1)
tags_dead_seedlings_2020 = unique(data.census$Tag[ids_dead_seedlings_2020])
data.census$Tag.removed.in.year[data.census$Tag %in% tags_dead_seedlings_2020] <- 2020
### END

### NEW 7/29/2021
# deal with the large #s of seedlings that should be 2020 patches

# find the problem patches
ids_seedling_patches_2020 = which(data.census$Is.seedling==1 & data.census$Year==2020)
# get summary data grouped by tag and prelim census
data_2020_patches_replacement = data.census[ids_seedling_patches_2020,] %>% 
  group_by(Tag, Preliminary.seedling.census) %>% 
  summarize(X..cm.=mean(X..cm., na.rm=T), # pick mean location
            Y..cm.=mean(Y..cm., na.rm=T), # pick mean location
            Height..cm.=max(Height..cm., na.rm=T), # pick tallest height
            X..Capitulescences=max(X..Capitulescences, na.rm=T),  # pick most flowers
            Number.of.individuals.represented.by.tag=sum(Number.of.individuals.represented.by.tag, na.rm=T), # count rows
            Length.per.individual..cm.=mean(Length.per.individual..cm., na.rm=T), # average size of indivs
            Height.per.individual..cm.=mean(Height.per.individual..cm., na.rm=T), # average height of indivs
            Notes = paste(Notes,collapse=" - ")# average height 
            ) %>%
  ungroup

##
## During the 2020 preliminary seedling census. Data for multiple individuals was collected under the same tag number with sub tag letters. As an example tag 1A could be a multi-individual patch and tag 1B could be a single individual. 
##

# get all other metadata as first row for each group
data_2020_patches_first = data.census[ids_seedling_patches_2020,] %>% 
  group_by(Tag, Preliminary.seedling.census) %>% 
  slice(1) %>%
  ungroup

# overwrite the metadata with the patch average data to make a revised data subset
data_2020_patches_first[,names(data_2020_patches_replacement)] <- data_2020_patches_replacement

# drop the rows in the raw census corresponding to problem patches 
data.census = data.census[-ids_seedling_patches_2020,]
# add in the revised data subset
data.census = rbind(data.census, data_2020_patches_first)

## new checks 12/23/2021
# check for big seedlings
rows_big_seedlings = which(data.census$Is.seedling==1 & data.census$Length..cm. >= 5 & data.census$Number.of.individuals.represented.by.tag==1)
big_seedlings = data.census[rows_big_seedlings,] %>% 
  arrange(Year, Tag)
print(big_seedlings)
# remove seedling designation for these by force
data.census$Is.seedling[rows_big_seedlings] = 0

# check for date issues
unique_dates = paste(data.census$Month, data.census$Day) %>% unique
print(sort(unique_dates))
# look for pre-june months
data.census %>% filter(Month < 6)

### END 12/23/2021


### END

# generate clean data
make_clean_df <- function(year,prelimseedlingsforyear, includepreviousyearnotes, includedeadseedlings)
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
	
	datass = datass %>% arrange(Year, Plot)
	
	return(datass)	
}

# make clean dataframes
for (year in 2014:current_year)
{
  # write census
  dfcurrent_fieldtags <- make_clean_df(year=year, prelimseedlingsforyear=NULL, includepreviousyearnotes=FALSE, includedeadseedlings=FALSE)
  # export file
  write.csv(dfcurrent_fieldtags, file=sprintf('outputs/census %d clean only field tags.csv',year),row.names=F,na="")  
  
  dfcurrent_deadseedlings <- make_clean_df(year=year, prelimseedlingsforyear=NULL, includepreviousyearnotes=FALSE, includedeadseedlings=TRUE)
  # export file
  write.csv(dfcurrent_deadseedlings, file=sprintf('outputs/census %d clean with dead seedlings.csv',year),row.names=F,na="")  
  
     
  # write abundances
  dfcurrent_alive <- dfcurrent_fieldtags[dfcurrent_fieldtags$Length..cm. > 0,]
  abundancescurrent = dfcurrent_alive %>% 
    group_by(Taxon, Plot) %>% 
    tally %>% 
    arrange(Plot, Taxon) %>% 
    pivot_wider(names_from = Taxon,
                values_from = n, 
                values_fill = 0)
  write.csv(abundancescurrent,
            file=sprintf('outputs/abundances %d clean only field tags.csv',year), 
            row.names=FALSE)
  
  abundancescurrent_noseedling = dfcurrent_alive %>% 
    group_by(Taxon, Plot) %>% 
    filter(Is.seedling==FALSE) %>%
    tally %>% 
    arrange(Plot, Taxon) %>% 
    pivot_wider(names_from = Taxon,
                values_from = n, 
                values_fill = 0)
  write.csv(abundancescurrent_noseedling,
            file=sprintf('outputs/abundances %d clean only field tags no seedlings.csv',year), 
            row.names=FALSE)
}


# summarize plots

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

# count the number of plots
summary_plot_count = data.census %>% 
  select(Year, Plot) %>% 
  unique %>% 
  group_by(Year) %>% 
  tally %>% 
  mutate(plot_count = ceiling(n/50)*50) %>%
  select(-n)

summary_stats = summary_stats %>% 
  left_join(summary_plot_count, by='Year') %>%
  mutate(plot_count = ifelse(plot_count==100,99,plot_count)) %>%
  mutate(value_per_plot = value / plot_count)
warning('this assumes plot 46R does not exist in 2022, fix if in 2023 we add another plot')


g_demography_trends <- ggplot(summary_stats,aes(x=Year,y=value_per_plot,fill=variable)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~variable,scales='free_y') + 
  theme_bw() + 
  ylab("Mean count per plot") + 
  theme(legend.position = "none") +
  scale_x_continuous(breaks = function(x) unique(floor(seq(min(x), max(x)))))
#missing value is the 2014 Number.mortality.events, which is 'NA'
ggsave(g_demography_trends,file='outputs/g_demography_trends.pdf',width=9,height=5)


species_counts <- as.data.frame(do.call("rbind",by(data.census,data.census$Year, function(data_census) {
  do.call("rbind",by(data_census, data_census$Taxon, function(x) { Taxon=data.frame(x$Taxon[1],Count=length(which(x$Length..cm. > 0)),Year=x$Year[1]) }))
  
})))
row.names(species_counts) <- NULL
names(species_counts) <- c("Taxon","Count","Year")
species_counts <- species_counts[order(species_counts$Taxon, species_counts$Year),]

species_trends = species_counts %>% 
  left_join(summary_plot_count, by='Year') %>%
  mutate(count_per_plot = Count / plot_count)

g_abundance_trends = ggplot(species_trends,aes(x=Year,y=count_per_plot)) + 
  geom_bar(show.legend = FALSE,stat='identity') + 
  theme_bw() + 
  ylab('Mean abundance per plot') + 
  facet_wrap(~Taxon,scales='free_y') +
  scale_x_continuous(breaks = function(x) unique(floor(seq(min(x), max(x)))))
ggsave(g_abundance_counts, file="outputs/g_abundance_trends.pdf",width=20,height=8)


####
####
####

# write out transformed dataset for demography
# in this case do include dead seedlings to note they existed
df_clean_all = do.call("rbind",lapply(2014:current_year, make_clean_df, prelimseedlingsforyear=NULL, includepreviousyearnotes=FALSE, includedeadseedlings=TRUE))

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
  # evenly divide items into bins as possible
  result = rep(floor(total/num_bins), num_bins)
  # figure out if there is a remainder
  residual = total %% num_bins
  # if there is
  if (residual > 0)
  {
    # add one to any bin that needs one
    result[1:residual] = result[1:residual] + 1
  }
  return(result)
}

# make a lot of letters in case the patches are big
letters_all = c(LETTERS,
                paste(LETTERS, LETTERS, sep=""),
                paste(LETTERS, LETTERS, LETTERS, sep=""),
                paste(LETTERS, LETTERS, LETTERS, LETTERS, sep="")
                )


# make pseudorandom points on a disc
get_random_points_on_disc <- function(num)
{
  offsets_all = NULL
  num_accepted = 0
  
  while(num_accepted < num)
  {
    offsets = poisson2disk(n=num, a = 1, b = 1, m = 10, info = FALSE) %>%
      as.data.frame
    names(offsets) <- c("x","y")
  
    offsets$x = offsets$x - 1
    offsets$y = offsets$y - 1
    offsets = offsets %>% filter(sqrt(x^2+y^2) < 1)

    offsets_all = rbind(offsets_all, offsets)
    num_accepted = num_accepted + nrow(offsets)
  }
  
  offsets_all$index = 1:nrow(offsets_all)
  row.names(offsets_all) = NULL
  
  offsets_all = offsets_all[1:num,]
  
  return(offsets_all)
}

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
    offsets = get_random_points_on_disc(num = max.indivs)

    # repeat data frame for as many indivs as we have
    df_out = df[rep(1,max.indivs),] %>% 
      # give each tag a new name
      # first indiv keeps original tag; all others get letter codes
      mutate(TagNew=paste(Tag,letters_all[1:max.indivs],sep="*")) %>%
      # give new locations
      mutate(X..cm. = X..cm. + (Length..cm./2)*offsets$x, 
             Y..cm. = Y..cm. + (Length..cm./2)*offsets$y) %>%
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
df_individual_panel = df_clean_all %>% 
  group_by(Tag, Year) %>% 
  do(replicate_indivs(.))

# make demography data
df_individual_demo = df_individual_panel %>% 
  ungroup %>% group_by(Tag) %>%
  # get previous size
  mutate(Length..cm..prev = c(NA,head(Length..cm.,-1))) %>%
  # calculate growth as differences in sizes
  mutate(Growth = Length..cm. - Length..cm..prev) %>%
  # calculate survival as whether did not die
  mutate(Survival = as.numeric(!Died.this.census.final)) %>%
  # calculate recruits as plants that are new non-seedlings after the first study year
  mutate(Is.recruit = (is.na(Length..cm..prev) & Is.seedling==0) | Is.seedling==1) # fixed this line to include the OR statement 12/23/2021

# NEW 12/28/2021
# set all 2014 recruit status to NA, since we don't really know if they are new plants unless clearly seedlings
df_individual_demo$Is.recruit[df_individual_demo$Year==2014] = NA
df_individual_demo$Is.recruit[df_individual_demo$Year==2014 & df_individual_demo$Is.seedling==TRUE] = 1
# END

## NEW DEC 23 2021
# for B-onward tags, 
# find non-A tags
tags_non_A = unique(df_individual_demo$Tag[!grepl("\\*A$", df_individual_demo$Tag)])
last_year_non_A = df_individual_demo %>% 
                    filter(Tag %in% tags_non_A) %>% 
                    group_by(Tag) %>% 
                    summarize(year_last_appeared = max(Year))

# iterate through all the non-A tags in their last year
for (i in 1:nrow(last_year_non_A))
{
  # find dataset rows corresponding to this last year
  rows_this_last = which((df_individual_demo$Year == last_year_non_A$year_last_appeared[i]) & (df_individual_demo$Tag == last_year_non_A$Tag[i]))
  # if the last year of appearance is not this year... (I.e. is previous)
  if (last_year_non_A$year_last_appeared[i] < current_year)
  {
    # assume the tag died
    df_individual_demo$Died.this.census.final[rows_this_last] = 1
    df_individual_demo$Survival[rows_this_last] = 0 ## added 12/28/2021
  } else
  {
    # otherwise no action
  }
  
  
  print(df_individual_demo[rows_this_last,] %>% select(Tag, Year))
}

# check for multi-tag deaths having being applied correctly
df_individual_demo %>% filter(Tag %in% tags_non_A) %>% 
  arrange(Tag, Year) %>% 
  select(Tag,Year, Died.this.census.final)
### END DEC 23 2021 edit

#Check for plants with multiple survival scores=0. First I want to filter so that just plants with a Survival score=0 are kept
survival.check <- df_individual_demo %>% filter(Survival==0)
survival.check %>% group_by(Tag) %>% filter(n()>1) %>% select("Year", "Tag", "Survival")

#There are 4 tags that have survival scores of 0 in multiple years. These are all plants that were intentionally tagged in the 2015 pre-census, were dead in the 2015 main census, and were recorded as alive in 2016. As this demography pattern is unusual in the data (dead seedlings from pre-censuses are not typically kept), it requires a manual patch. 

df_individual_demo[df_individual_demo$Tag=="1095*A" & df_individual_demo$Year==2015, "Survival"] <-1
df_individual_demo[df_individual_demo$Tag=="1143*A" & df_individual_demo$Year==2015, "Survival"] <-1
df_individual_demo[df_individual_demo$Tag=="1397*A" & df_individual_demo$Year==2015, "Survival"] <-1
df_individual_demo[df_individual_demo$Tag=="1456*A" & df_individual_demo$Year==2015, "Survival"] <-1

#Check that the demog data for these tags looks good
df_individual_demo %>% filter(Tag=="1095*A" | Tag=="1143*A" | Tag=="1397*A" | Tag=="1456*A") %>% select("Year", "Tag", "Length..cm.", "Survival")

#Write final CSV
write.csv(df_individual_demo, file=sprintf('outputs/demography_2014-%d.csv', current_year), row.names=F)







#
#
#Plot plants

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
        text(plotdata$X..cm.[i]+plotdata$Tag.offset.x[i],plotdata$Y..cm.[i]+plotdata$Tag.offset.y[i],plotdata$Tag[i],col= whichcolor_text,cex=0.2,font=2)
        text(plotdata$X..cm.[i],plotdata$Y..cm.[i],getcode(plotdata$Taxon[i]),col= whichcolor_text,cex=0.2,font=2)
      }
      
    }
  }
  if (names==TRUE)
  {
    mtext(text=sprintf("Plot %s %s",plotid,title),line=0,cex=0.75)	
  }
  box()
}

# make plots over space
plot_names_all = setdiff(c(paste(1:50), paste(paste(1:50), "R",sep="")),"46R")

for (year in 2014:current_year)
{
  pdf(width=5,height=5,file=sprintf('outputs/g_census_map_%d.pdf',year))
  par(mar=c(3,3,1,1))
  par(cex.axis=0.5)
  for (plot_name in plot_names_all)
  {
    cat('.')
    # only plot R plots after 2022
    if ((length(grep("R",plot_name))>0 & year >= 2022) | (length(grep("R",plot_name))==0))
    {
      drawplot(df_individual_panel, plot_name, year)
    }
  }
  dev.off()  
  cat('\n')
}

# make plots over time
for (plot_name in plot_names_all)
{
  pdf(width=5,height=5,file=sprintf('outputs/g_census_time_series_plot_%s.pdf',plot_name))
  par(mar=c(3,3,1,1))
  par(oma=c(0,0,3,3))
  par(cex.axis=0.5)
  for (year in 2014:current_year)
  {
    cat('.')
    # only plot R plots after 2022
    if ((length(grep("R",plot_name))>0 & year >= 2022) | (length(grep("R",plot_name))==0))
    {
      drawplot(df_individual_panel, plot_name, year)
      mtext(side=3,year,line=2)
    }
  }
  dev.off()  
  cat('\n')
}


