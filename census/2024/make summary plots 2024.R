library(dplyr)
library(ggplot2)
library(reshape2)
library(vegan)
library(sf)
library(tidyr)
library(ggpubr)
library(ggplot2)
library(tidyverse)
library(glue)
library(pbapply)
library(ggtext)
library(ggforce)

current_year = 2024
plot_names_all = paste(sort(rep(1:50,2)),c("","R"),sep="")


set.seed(1)


# create output directory
if (!file.exists('outputs_figures'))
{
  dir.create('outputs_figures')
}


########

data.census = read.csv(sprintf('outputs_data/census clean with dead seedlings 2014-%d.csv', current_year))
data.demography = read.csv(sprintf('outputs_data/demography_2014-%d.csv', current_year))

#########









# look for NAs in various columns that should not have them
pdf(file='outputs_figures/g_missing_data.pdf')
par(mar=c(1,10,1,1))
for (year in sort(unique(data.census$Year)))
{
  data.census.year = data.census %>% 
    filter(Year==year)
  image(is.na(data.census.year),xaxt='n',yaxt='n',main=paste("Missing data", year));
  axis(side=2,at=seq(0,1,length.out=length(names(data.census))),labels=names(data.census),las=2)
}
dev.off()












# summarize plots
df_summary_stats = data.demography %>% 
  mutate(Taxon=ifelse(Taxon %in% c("NA NA","Indet indet1","Indet indet2","Indet indet3"),"Other",Taxon)) %>%
  group_by(Year, Taxon) %>%
  summarize(    Number.seedlings=length(which(Is.seedling==1)),
                Number.recruits=length(which(Is.recruit==1)),
                Number.mortality=length(which(Died.this.census.final==1)),
                Number.flowering=length(which(X..Capitulescences>0)),
                Number.total=length(which(Length..cm.>0)),
                Mean.length = mean(Length..cm.[Length..cm.>0]))


# NA the mortality events in the 1st year of the study since we can't know this!
df_summary_stats$Number.mortality[df_summary_stats$Year==2014] = NA

df_summary_stats_long = df_summary_stats %>%
  pivot_longer(!c(Year,Taxon))

# count the number of plots
summary_plot_count = data.census %>% 
  select(Year, Plot) %>% 
  unique %>% 
  group_by(Year) %>% 
  tally %>% 
  mutate(plot_count = ceiling(n/50)*50) %>%
  mutate(plot_count = ifelse(plot_count==100 & Year==2022,99,plot_count)) %>% # in 2022, we did not do 46R
  select(-n)

df_summary_stats_long_joined = df_summary_stats_long %>% 
  left_join(summary_plot_count, by='Year') %>%
  mutate(value_final = ifelse(name=='Mean.length',value / 20,value / plot_count)) %>%
  mutate(name = gsub("Number","Number.per.plot",name)) %>%
  mutate(name = gsub("Mean.length","Mean.length.x20.cm",name))

g_demography_trends <- ggplot(df_summary_stats_long_joined,aes(x=Year,y=value_final,fill=name)) + 
  geom_bar(stat='identity') + 
  facet_grid(Taxon~name,scales='free_y') + 
  theme_bw() + 
  ylab("Value") + 
  theme(legend.position = "none") +
  scale_x_continuous(breaks = function(x) unique(floor(seq(min(x), max(x)))))
#missing value is the 2014 Number.mortality.events, which is 'NA'
ggsave(g_demography_trends,file='outputs_figures/g_demography_trends.pdf',width=20,height=30)




df_summary_stats_abundance = data.demography %>%
  mutate(Taxon=ifelse(Taxon %in% c("NA NA","Indet indet1","Indet indet2","Indet indet3"),"Other",Taxon)) %>%
  group_by(Taxon, Year) %>%
  tally() %>%
  left_join(summary_plot_count, by='Year') %>%
  mutate(count_per_plot = n / plot_count)



g_abundance_trends = ggplot(df_summary_stats_abundance,aes(x=Year,y=count_per_plot)) + 
  geom_bar(show.legend = FALSE,stat='identity') + 
  theme_bw() + 
  ylab('Mean abundance per plot') + 
  facet_wrap(~Taxon) +
  scale_x_continuous(breaks = function(x) unique(floor(seq(min(x), max(x))))) +
  scale_y_sqrt()
ggsave(g_abundance_trends, file="outputs_figures/g_abundance_trends.pdf",width=20,height=8)


df_summary_stats_biomass = data.demography %>%
  mutate(Taxon=ifelse(Taxon %in% c("NA NA","Indet indet1","Indet indet2","Indet indet3"),"Other",Taxon)) %>%
  group_by(Taxon, Year) %>%
  summarize(biomass.proxy=sum(Length..cm.,na.rm=TRUE)^2) %>% # guess at the exponent
  left_join(summary_plot_count, by='Year') %>%
  mutate(biomass.proxy.per.plot = biomass.proxy / plot_count)



g_biomass_trends = ggplot(df_summary_stats_biomass,aes(x=Year,y=biomass.proxy.per.plot)) + 
  geom_bar(show.legend = FALSE,stat='identity') + 
  theme_bw() + 
  ylab('Mean biomass proxy per plot') + 
  facet_wrap(~Taxon) +
  scale_x_continuous(breaks = function(x) unique(floor(seq(min(x), max(x))))) +
  scale_y_sqrt()
ggsave(g_biomass_trends, file="outputs_figures/g_biomass_trends.pdf",width=20,height=8)



df_summary_stats_biomass_total = df_summary_stats_biomass %>%
  group_by(Year) %>%
  summarize(total.biomass.proxy.per.plot = sum(biomass.proxy.per.plot))

g_biomass_total_trends = ggplot(df_summary_stats_biomass_total,aes(x=Year,y=total.biomass.proxy.per.plot)) + 
  geom_bar(show.legend = FALSE,stat='identity') + 
  theme_bw() + 
  ylab('Mean biomass proxy per plot') + 
  scale_x_continuous(breaks = function(x) unique(floor(seq(min(x), max(x))))) +
  scale_y_sqrt()
ggsave(g_biomass_total_trends, file="outputs_figures/g_biomass_total_trends.pdf",width=7,height=5)



warning('add some clustering over time code')





### NMDS
# keep all combos
data_counts = data.demography %>% 
  mutate(Taxon=ifelse(Taxon %in% c("NA NA","Indet indet1","Indet indet2","Indet indet3"),"Other",Taxon)) %>%
  mutate(Plot=factor(Plot, levels=plot_names_all,ordered=TRUE)) %>%
  group_by(Year, Plot, Taxon, .drop=FALSE) %>%
  tally() %>%
  pivot_wider(values_from = 'n',names_from='Taxon',values_fill=0) %>%
  mutate(Plot=factor(Plot, levels=plot_names_all,ordered=TRUE)) %>%
  select(-"NA") # because of the grouping in tally??

set.seed(1)
nmds_all = metaMDS(data_counts %>% 
                      ungroup %>%
                      select(-Year, -Plot) %>% 
                      as.data.frame %>% 
                      as.matrix,
                   distance='euclidean',
                   try=10)



nmds_points = nmds_all$points %>%
  as.data.frame %>%
  cbind(data_counts$Year, data_counts$Plot)
names(nmds_points) = c("NMDS1","NMDS2","Year","Plot")
nmds_points = nmds_points %>%
  mutate(Plot=factor(Plot, levels=plot_names_all,ordered=TRUE))


nmds_axes = nmds_all$species %>%
  as.data.frame
nmds_axes$Taxon = row.names(nmds_axes)

g_nmds_by_year = ggplot(nmds_points, aes(x=NMDS1, y=NMDS2,color=factor(Year))) +
  geom_point() +
  theme_bw() +
  scale_color_viridis_d(name='Year') +
  stat_ellipse() +
  geom_segment(data=nmds_axes, aes(x=0,y=0,xend=MDS1,yend=MDS2),color='black',inherit.aes = FALSE) +
  geom_text(data=nmds_axes, aes(x=MDS1,y=MDS2,label=Taxon),color='black',inherit.aes = FALSE)
ggsave(g_nmds_by_year, file='outputs_figures/g_nmds_by_year.pdf',width=10,height=10)

g_nmds_by_plot = ggplot(nmds_points, aes(x=NMDS1, y=NMDS2,color=factor(Year),group=Plot)) +
  geom_point() +
  geom_line() + 
  theme_bw() +
  scale_color_viridis_d(name='Year') +
  facet_wrap(~Plot,ncol=10) +
  geom_segment(data=nmds_axes, aes(x=0,y=0,xend=MDS1,yend=MDS2),color='gray',inherit.aes = FALSE,alpha=0.5,linewidth=0.5) +
  geom_text(data=nmds_axes, aes(x=MDS1,y=MDS2,label=Taxon),color='gray',inherit.aes = FALSE,alpha=0.5,size=1)
ggsave(g_nmds_by_plot, file='outputs_figures/g_nmds_by_plot.pdf',width=25,height=25)









## make spatial
map_plot_year <- function(data, year, plot, legend=TRUE, title=TRUE, axes=TRUE)
{
  df_pre_spatial = data.demography %>%
    mutate(Taxon=ifelse(Taxon %in% c("NA NA","Indet indet1","Indet indet2","Indet indet3"),"Other",Taxon)) %>%
    mutate(Taxon = factor(Taxon)) %>%
    filter(Year==year & Plot==plot) %>%
    filter(!is.na(X..cm.) & !is.na(Y..cm.)) %>%
    mutate(Is.recruit=factor(ifelse(is.na(Is.recruit),0,Is.recruit),levels=c(0,1))) %>%
    mutate(Died.this.census.final=factor(Died.this.census.final, levels=c(0,1)))
  
  if (nrow(df_pre_spatial) > 0)
  {
    sf_coords = st_as_sf(df_pre_spatial, coords = c("X..cm.","Y..cm."))
    radii = df_pre_spatial$Length..cm./2
    radii[is.na(radii)] = 0 # needed or sf crashes
    sf_coords_circles = st_buffer(sf_coords, radii)
  }
  
  sf_box = st_buffer(st_as_sf(data.frame(x=100,y=100),coords=c("x","y")),100,endCapStyle = 'SQUARE')
  
  g_presence = ggplot() + 
    geom_sf(data=sf_box,fill=NA)
  
  if (axes==TRUE)
  {
    g_presence = g_presence + theme_minimal()
  }
  else 
  {
    g_presence = g_presence + theme_void()
  }
  g_presence = g_presence + 
    scale_fill_discrete(drop=FALSE, name='Taxon') +
    scale_color_manual(drop=FALSE, values=c('black','red'),name='Mortality') +
    scale_shape_manual(drop=FALSE, values=c(1,3),name='Recruit') +
    theme(plot.title = element_text(size=10), legend.text=element_text(size=6), legend.title=element_text(size=8)) +
    theme(legend.spacing.x = unit(1.0, 'mm'), legend.spacing.y = unit(1.0, 'mm'),legend.key.size = unit(6, "mm"))
  
  if (nrow(df_pre_spatial) > 0)
  {
    g_presence = g_presence + 
      geom_sf(data=sf_coords_circles,aes(fill=Taxon),alpha=0.5) + 
      geom_sf(data=sf_coords_circles,aes(color=Died.this.census.final),fill=NA) + 
      geom_sf(data=sf_coords,aes(shape=Is.recruit),alpha=0.25)
  }
  
  if (legend==FALSE)
  {
    g_presence = g_presence + theme(legend.position='none')
  }
  if (title==TRUE)
  {
    g_presence = g_presence + ggtitle(sprintf('Year %d - Plot %s',year, plot))
  }
  g_presence = g_presence + coord_sf(xlim=c(0,200),ylim=c(0,200))

  return(g_presence)
}

# gave up on this - ggarrange legend issue
# map_plot_all_years <- function(data, plot)
# {
#   plots_all = lapply(2014:current_year, function(year) {
#     map_plot_year(data=data, plot=plot, year=year)
#   })
#   g_all = ggarrange(plotlist = plots_all)
#   return(g_all)
# }

pdf(file='outputs_figures/g_time_series.pdf',width=10,height=10)
for (plot in plot_names_all)
{
  for (year in 2014:current_year)
  {
    print(paste(plot,year))
    print(map_plot_year(data.demography, year, plot))
  }
}
dev.off()

pdf(file='outputs_figures/g_time_series_all_plots.pdf',width=20,height=30)
for (year in 2014:current_year)
{
    plots_for_year = lapply(plot_names_all, function(plot)
    {
      print(paste(plot,year))
      return(map_plot_year(data.demography, year, plot, title=FALSE, legend=FALSE, axes=FALSE))
    })
    plots_for_year_arranged = ggarrange(plotlist=plots_for_year, nrow=10,ncol=10)
    print(annotate_figure(plots_for_year_arranged, top = text_grob(paste(year), 
                                          color = "black", face = "bold", size = 14)))
}
dev.off()









# plot individual trajectory
plot_individual_trajectory <- function(data, tag)
{
  colors = c("Unknown"='gray',"Seedling"='green',"Resprout"='blue',"Vegetative"='purple',"Died"='red')
  
  years_all = 2014:current_year
  
  indiv_this = data %>% 
    filter(Tag==tag) %>%
    mutate(Event=ifelse(is.na(Is.recruit),"Unknown",ifelse(Is.recruit, ifelse(Is.seedling,"Seedling","Resprout"), ifelse(Died.this.census.final,"Died","Vegetative"))) %>%
             factor(levels=c("Unknown","Seedling","Resprout","Vegetative","Died")))
  
  years_missing = setdiff(years_all, indiv_this$Year)
  indiv_this_blank = data.frame(matrix(nrow=length(years_missing),ncol=length(names(indiv_this))))
  names(indiv_this_blank) = names(indiv_this)
  indiv_this_blank$Year = years_missing
  indiv_this = rbind(indiv_this, indiv_this_blank) %>%
    mutate(
      color = colors[Event],
      name = glue("<i style='color:{color}'>{Year}</i>"),
      name = fct_reorder(name, Year))
  
  g = ggplot(data=indiv_this, aes(x=name,y=0)) +
    #scale_x_continuous(breaks=2014:current_year, labels = 2014:current_year, limits=c(2014-1,1+current_year)) + 
    theme_minimal()+ 
    theme(
      axis.text.x = element_markdown())  +
    geom_bar(data=indiv_this, aes(x=name,y=X..Capitulescences/max(X..Capitulescences)), inherit.aes = FALSE, stat='identity', width=0.1, color='black',fill='black') +
    geom_circle(data=indiv_this, alpha=0.5, aes(x0=as.numeric(name),y0=0,r=Length..cm./50, fill=Event, color=Event), inherit.aes = FALSE) +
   theme(axis.text.y=element_blank(),
                            axis.title.y=element_blank(),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            axis.title.x=element_blank(),
                            plot.title = element_text(size=12)) +
    coord_equal() +
    ggtitle(paste(indiv_this$Tag, indiv_this$Taxon,sep=" - ")) +
    scale_fill_manual(drop=FALSE, values=colors,name='') +
    scale_color_manual(drop=FALSE, values=colors,name='') +
    theme(legend.position='bottom') +
    ylim(-2,2) # 50*1.5 cm for the max
  
  return(g)
}

# eventually do all tags - this is faster for now
random_tags = data.demography %>% 
  group_by(Taxon) %>%
  slice_sample(n=5, replace=TRUE) %>%
  select(Taxon, Tag) %>%
  unique

pdf(file='outputs_figures/g_trajectories.pdf',width=(current_year-2014),height=3)
  result_plots = pblapply(random_tags$Tag, function(tag) {
  #print(tag)
    try(print(plot_individual_trajectory(data.demography, tag))) # try is because incompletely entered data will throw error
  })
dev.off()

