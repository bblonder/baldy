# To do
      # Add something for ‘first year in dataset’ for plots so that we don’t erroneously get infinite growth for the R plots

      # Fix missing years in demography output caused by bi-directional changes in number of indivs-example 1183
      #Exclude plants that are size 0 in their first year (or 0 in all main census periods)
          #Would allow us to ignore the unknown plants that appeared in the pre-lim census but were dead by the time the main census occurred
        
library(dplyr)
library(tidyr)
library(readxl)
library(devtools)
library(ggplot2)
library(reshape2)
library(pracma)
library(data.table)

# set year
current_year = 2024

plot_names_all = paste(sort(rep(1:50,2)),c("","R"),sep="")


# create output directory
if (!file.exists('outputs_data'))
{
  dir.create('outputs_data')
}

# set seed
set.seed(1)


col_types =            c("numeric", # Year
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
                         "numeric", # Is.seedling.in.starting.year
                         "numeric", # Num.indivs
                         "numeric", # Length.indiv
                         "numeric", # Height.indiv
                         "text", # Neighbors
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
                         "text", #Page
                         "numeric" #Row
)

data.census <- as.data.frame(read_excel(sprintf("plant census 2014-%d.xlsx",current_year),sheet=1, col_types=col_types))

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
			
			# allow prior seedling status to propagate
			# defaults to not a seedling
			newrecord$Is.seedling=0
			# if both conditions are met...
			if (!is.na(toadd$Is.seedling.in.starting.year[i]))
			{
  			if (whichyear==toadd$Should.be.in.starting.year[i] & toadd$Is.seedling.in.starting.year[i]==1)
  		  {
  			  newrecord$Is.seedling=1
  			}
			}
			
			tempadd <- rbind(tempadd, newrecord)
			cat(sprintf('Added tag %s into year %d\n',toadd[i,"Tag"], toadd[i,"Year"]))
		}
		alladd <- rbind(alladd, tempadd)
	}
	data.census <- rbind(data.census, alladd)
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



# new aug 2024 - update coordinates for new plants in plot 46 and 46R due to physically moving the string boundary on lower isde
tags_46_46R_2024 = data.census %>% 
  filter(Year==2024 & Plot %in% c("46","46R")) %>% 
  pull(Tag)
tags_2023_or_older = data.census %>% 
                     filter(Year<2024) %>%
                     pull(Tag)
tags_46_46R_2024_new = tags_46_46R_2024[!(tags_46_46R_2024 %in% tags_2023_or_older)]
print(tags_46_46R_2024_new)

# find all tag-year combos that are not these 2024-new-tags in 46/46R
rows_position_change_46_46R = which(data.census$Plot %in% c("46","46R") & !(data.census$Tag %in% tags_46_46R_2024_new & data.census$Year==2024))
# add 10 cm to y (this is how much we moved the baseline down in aug 2024) and clamp at 200
data.census$Y..cm.[rows_position_change_46_46R] = 10 + data.census$Y..cm.[rows_position_change_46_46R]
data.census$Y..cm.[rows_position_change_46_46R] = ifelse(data.census$Y..cm.[rows_position_change_46_46R] > 200, 
                                                         200, 
                                                         data.census$Y..cm.[rows_position_change_46_46R])

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
write.csv(ghost_tag_sequences, file='outputs_data/ghost_tag_sequences.csv',row.names=F)

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
write.csv(tags_to_remove_this_year, file=sprintf('outputs_data/tags_to_remove_thisyear_%d.csv',current_year),row.names=F)

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


## fix edge codes (new aug 2024)
check_neighbor_codes <- function(dc, year)
{
  if (year<2022)
  {
    stop('neighbor codes were not collected in these years')
  }
  
  dc_this_year = dc %>% filter(Year==year)
  
  # remove extra spaces, split by comma
  neighbor_codes = strsplit(gsub(" ","",dc_this_year$Neighbors), ",")
  df_all_pairs = rbindlist(lapply(1:nrow(dc_this_year), function(i) { 
    result = data.frame(focal.tag=dc_this_year$Tag[i],
                        focal.length=dc_this_year$Length..cm.[i],
                        target.raw=neighbor_codes[[i]]) 
    return(result)
    })) 
  df_all_pairs$target.raw = ifelse(df_all_pairs$target.raw == "NA",NA, df_all_pairs$target.raw)
  lengths = sapply(strsplit(df_all_pairs$target.raw,split='\\.'),length)

  
  if(any(lengths>2))
  {  
    print("distribution of number of dots")
    print(table(lengths))
    print(df_all_pairs[which(lengths>2),])
    warning('neighbor codes have more than one dot in code')
  }
  else
  {
    df_all_pairs$neighbor.type = sapply(strsplit(df_all_pairs$target.raw,split='\\.'),head,1)
    df_all_pairs$target.tag = sapply(strsplit(df_all_pairs$target.raw,split='\\.'),tail,1)
    # no partner for A tags
    df_all_pairs$target.tag[df_all_pairs$neighbor.type=='A'] = NA
    
    # check all the NAs correspond to dead plants
    df_na = df_all_pairs %>% filter(is.na(target.raw))
    if(!all(df_na$focal.length==0))
    {
      print(df_na %>% filter(focal.length!=0))
      warning('some NA edge codes are not dead plants')
    }

    # check H plants are H to themself
    host_plants = df_all_pairs %>% 
      filter(neighbor.type=='H')
    host_plants_not_focal = host_plants %>% 
      filter(target.tag!=focal.tag)
    if(nrow(host_plants_not_focal) > 0)
    {
      print('not all host plants are H to themself')
      print(host_plants_not_focal)
    }
    
    # check I plants are not in themself
    inside_plants = df_all_pairs %>% 
      filter(neighbor.type=='I')
    inside_plants_self = inside_plants %>% 
      filter(target.tag==focal.tag)
    if(nrow(inside_plants_self) > 0)
    {
      print('not all inside plants are inside a different tag')
      print(inside_plants_self)
    }
    # check I plants are in a H plant
    inside_plant_hosts = inside_plants$target.tag
    putative_hosts = df_all_pairs %>% 
      filter(focal.tag %in% inside_plant_hosts)
    if(!all(putative_hosts$neighbor.type=='H'))
    {
      print('not all inside plants\' putative hosts are labeled hosts')
      print(table(putative_hosts$neighbor.type))
      print(putative_hosts %>% 
              filter(neighbor.type!='H') %>%
              select(focal.tag, neighbor.type, target.tag))
    }
    

    # check E plants are not edge to themself
    edge_plants = df_all_pairs %>% 
      filter(neighbor.type=='E')
    edge_plants_self = edge_plants %>% 
      filter(target.tag==focal.tag)
    if(nrow(edge_plants_self) > 0)
    {
      print('not all edge plants are edge to a different tag')
      print(edge_plants_self)
    }
    
    # check reciprocality of edges
    edge_plants_mergers = edge_plants %>% 
      select(focal.tag, target.tag) %>%
      rename(target.tag=focal.tag, focal.target.tag=target.tag)
    edge_plants_joined = edge_plants %>%
      left_join(edge_plants_mergers, by='target.tag', relationship='many-to-many')
    edge_plants_non_reciprocal = edge_plants_joined %>%
           filter(focal.tag!=focal.target.tag)
    if(nrow(edge_plants_non_reciprocal) > 0)
    {
      print('not all edge plant relationships are reciprocal')
      print(edge_plants_non_reciprocal)
    }
    
    pos_focal = df_all_pairs %>% 
      select(focal.tag, neighbor.type) %>%
      left_join(dc_this_year %>% select(focal.tag=Tag, Plot.focal=Plot, X..cm..focal=X..cm., Y..cm..focal=Y..cm.), by='focal.tag')
    pos_target = df_all_pairs %>% 
      select(target.tag) %>%
      left_join(dc_this_year %>% select(target.tag=Tag, Plot.target=Plot, X..cm..target=X..cm., Y..cm..target=Y..cm.), by='target.tag')
    df_distances = data.frame(pos_focal, pos_target) %>%
      mutate(Plot.focal.sequential = match(Plot.focal, plot_names_all)) %>%
      mutate(Plot.target.sequential = match(Plot.target, plot_names_all)) %>%
      mutate(Plot.delta = Plot.focal.sequential - Plot.target.sequential) %>%
      mutate(X..cm..focal = X..cm..focal + Plot.delta*200) %>% # rewrite the X coordinate assuming the plots are adjacent in X
      filter(neighbor.type!='A' & (focal.tag != target.tag)) %>%
      mutate(Distance.naive = sqrt((X..cm..focal - X..cm..target)^2 + (Y..cm..focal - Y..cm..target)^2))
    df_distances_non_adjacent = df_distances %>% 
      filter(abs(Plot.delta)>1)
    df_distances_adjacent = df_distances %>% 
      filter(abs(Plot.delta)<=1 & Distance.naive>40) # can change this number - this is the cm distance for 'too far'
    if (nrow(df_distances_non_adjacent) > 0)
    {
      print('Neighbors are too far apart in plots:')
      print(df_distances_non_adjacent)
    }
    if (nrow(df_distances_adjacent) > 0)
    {
      print('Neighbors are too far apart within plots:')
      print(df_distances_adjacent)
    }
  }
  
  warning('may still need to account for tags that are renamed to fix remaining errors')
  
  return(NULL)
}

# delete any neighbor codes for years before when we did neighbors
data.census$Neighbors[data.census$Year < 2022] = NA

# check neighbor issues for each year
for (year_this in 2022:current_year)
{
  print(year_this)
  check_neighbor_codes(data.census, year_this)
}


# new aug 2024
# convert clumps to patches for demography purposes
process_clumps <- function(data.census)
{
  rows_clumps = which(grepl("stem|culm|clump",data.census$Notes) & 
                        !(data.census$Taxon %in% c("Eriogonum umbellatum")))
  
  strings = data.census$Notes[rows_clumps]
  strings = gsub("single", "1", strings)
  strings = gsub("[Oo]ne", "1", strings)
  strings = gsub("[Tt]wo", "2", strings)
  strings = gsub("[Tt]hree", "3", strings)
  strings = gsub("[Ff]our", "4", strings)
  strings = gsub("[Ff]ive", "5", strings)
  strings = gsub("big", "", strings)
  strings = gsub("small", "", strings)
  strings = gsub("large", "", strings)
  strings = gsub("main", "", strings)
  strings = gsub("little", "", strings)
  strings = gsub("horizontal", "", strings)
  strings = gsub("vertical", "", strings)
  strings = gsub("\\.", " ", strings)
  strings = gsub("\\,", " ", strings)
  strings = gsub("\\=", " ", strings)
  strings = gsub("  ", " ", strings)
  
  rows_split = strsplit(strings, split=" ")
  # find first instance of 'clump' in the notes
  num_clumps_string = sapply(rows_split, function(x) { 
    # rewrite characters to numbers
    
    index_clump = head(which(grepl("clump", x)),1)
    # get the string at position before the clump
    string_clump = x[index_clump-1] 
    if (length(string_clump)==0) 
    { 
      string_clump=""
    }
    return(string_clump)
  })
  num_clumps_numeric = as.numeric(num_clumps_string)
  
  result = data.frame(Taxon=data.census$Taxon[rows_clumps], Tag=data.census$Tag[rows_clumps], Year=data.census$Year[rows_clumps], Note=data.census$Notes[rows_clumps],num_clumps_string, num_clumps_numeric)

  result %>% group_by(Taxon, num_clumps_numeric) %>%
    filter(num_clumps_numeric > 1) %>%
    tally %>%
    print(n=Inf)
  
  print("some clumps did not parse")
  print(result %>% filter(is.na(num_clumps_numeric)))
  write.csv(result %>% filter(is.na(num_clumps_numeric)),file='outputs_data/clumps_did_not_parse.csv',row.names=FALSE)
  
  # anything that did not parse, set to 1 individual
  result$num_clumps_numeric[is.na(result$num_clumps_numeric)] = 1
  
  data.census$Number.of.individuals.represented.by.tag[rows_clumps] = result$num_clumps_numeric
  
  return(data.census)
  
}

data.census.clumps = process_clumps(data.census)

# get training data to guess patch length/height per indiv
df_for_estimation = data.census.clumps %>%
  mutate(row_key = row_number()) %>%
  # only work on clumped plants with more than 1 clump
  filter(grepl("clump",Notes) & Number.of.individuals.represented.by.tag > 1) %>%
  # rewrite the taxon for species that are multi-stem clumps for which we don't have 
  mutate(Taxon=as.character(Taxon)) %>%
  mutate(Taxon=ifelse(Taxon %in% c('Achnatherum lettermanii','Poa stenantha'), 
                      'Elymus lanceolatus',
                      Taxon))

# predict new length per indiv values based on taxon and patch size
m_length = lm(Length.per.individual..cm. ~ Taxon+Number.of.individuals.represented.by.tag*Length..cm.,
              data=data.census %>% filter(Number.of.individuals.represented.by.tag > 1))
# clamp values to patch size and a min value of 0.5
length.new = predict(m_length, newdata=df_for_estimation)
length.new = ifelse(length.new<0.5,0.5,ifelse(df_for_estimation$Length..cm.<length.new,df_for_estimation$Length..cm.,length.new))
plot(df_for_estimation$Length..cm.,length.new,asp=1); abline(0,1,col='red')
# same for height
m_height = lm(Height.per.individual..cm. ~ Taxon+Number.of.individuals.represented.by.tag*Height..cm.,
              data=data.census %>% filter(Number.of.individuals.represented.by.tag > 1))
height.new = predict(m_height, newdata=df_for_estimation)
height.new = ifelse(height.new<0.5,0.5,ifelse(df_for_estimation$Height..cm.<height.new,df_for_estimation$Height..cm.,height.new))
plot(df_for_estimation$Height..cm.,height.new,asp=1); abline(0,1,col='red')

# write in the new values
data.census.clumps$Length.per.individual..cm.[df_for_estimation$row_key] = length.new
data.census.clumps$Height.per.individual..cm.[df_for_estimation$row_key] = height.new

# overwrite the old census with the new assumed patches from the clumps
data.census = data.census.clumps



























### END QC

# generate clean data
make_clean_df <- function(year, prelimseedlingsforyear, includepreviousyearnotes, includedeadseedlings)
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
	
	datass = datass %>% 
	  mutate(Plot = factor(Plot, levels=plot_names_all, ordered=TRUE)) %>%
	  arrange(Year, Plot)
	
	return(datass)	
}

# make clean dataframes - aug 2024 rewrote this to put all the years in a single file rather than splitting
dfcurrent_fieldtags = rbindlist(lapply(2014:current_year, function(year)
{
  # write census
  make_clean_df(year=year, prelimseedlingsforyear=NULL, includepreviousyearnotes=FALSE, includedeadseedlings=FALSE)
}))
write.csv(dfcurrent_fieldtags, file=sprintf('outputs_data/census clean only field tags 2014-%d.csv',current_year),row.names=F,na="")  

dfcurrent_deadseedlings = rbindlist(lapply(2014:current_year, function(year)
{
  # write census
  make_clean_df(year=year, prelimseedlingsforyear=NULL, includepreviousyearnotes=FALSE, includedeadseedlings=TRUE)
}))
write.csv(dfcurrent_deadseedlings, file=sprintf('outputs_data/census clean with dead seedlings 2014-%d.csv',current_year),row.names=F,na="")  


     
# write abundances
df_current_alive <- dfcurrent_fieldtags[dfcurrent_fieldtags$Length..cm. > 0,]
abundances_current = df_current_alive %>% 
  group_by(Year, Taxon, Plot) %>% 
  tally %>% 
  arrange(Year, Plot, Taxon) %>% 
  pivot_wider(names_from = Taxon,
              values_from = n, 
              values_fill = 0)
write.csv(abundances_current,
          file=sprintf('outputs_data/abundances clean only field tags 2014-%d.csv',current_year), 
          row.names=FALSE)




# write out transformed dataset for demography
# in this case do include dead seedlings to note they existed
df_clean_all = do.call("rbind",lapply(2014:current_year, make_clean_df, prelimseedlingsforyear=NULL, includepreviousyearnotes=FALSE, includedeadseedlings=TRUE))

df_clean_all = df_clean_all %>% arrange(Year, Plot) %>% select(Year, 
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

# need to assign each individual a new tag
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
  mutate(Is.recruit = (is.na(Length..cm..prev) & Is.seedling==0) | Is.seedling==1) %>% # fixed this line to include the OR statement 12/23/2021
  mutate(Growth = ifelse(Survival==0, NA, Growth)) %>% # no growth if dead; can't be sure if the death was beginning or end season
  mutate(Growth = ifelse(Is.recruit==1, Length..cm., Growth)) %>%
  mutate(Growth = ifelse(Year==2014 & Is.seedling==0, NA, Growth))
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
  
  
  #print(df_individual_demo[rows_this_last,] %>% select(Tag, Year))
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
write.csv(df_individual_demo, file=sprintf('outputs_data/demography_2014-%d.csv', current_year), row.names=F)





