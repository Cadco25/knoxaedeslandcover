# **************************************************************************** #
# Title: Summary Analysis and Plotting 
# Project: Day and Trout Fryxell Land Cover Aedes Project 
# Author: Corey Day 
# Date Created: March 2024
# Questions? email coreyallenday96@gmail.com 
# **************************************************************************** #

# run renv::restore()

# load packages -----------------------------------------------------------

library(tidyverse) # for data manipulation and pipes
library(extrafont) # for loading fonts
library(ggpubr) # for creating graphical texts for axis labels
library(gridExtra) # for combining graphs in a grid



# define custom functions ---------------------------------------------------------------

## custom theme for plot ----

custom_theme <- function(legend_pos = c(0,1)){
  theme_bw() + 
    theme(text=element_text(family="Century Gothic"),
          plot.title = element_text(size = 30, face = "bold"),
          axis.title.x = element_text(size = 28, face = 'bold'),
          axis.title.y = element_text(size = 28, face = 'bold', vjust = 2.0),
          axis.text.x = element_text(size = 22, color = 'black', face='plain'),
          axis.text.y = element_text(size = 22, color = 'black', face='plain'),
          strip.text = element_blank(),
          legend.text = element_text(size= 23,face='bold.italic'),
         # legend.background = element_rect(linetype='solid', color = 'black'),
          #legend.title = element_text(size=28, face='bold',color='black'),
          legend.title = element_blank(),
          legend.justification = legend_pos,
          legend.position = legend_pos,
         # strip.background=element_rect(fill='#bcbcbc',color='black'),
         # panel.border=element_rect(color='black',fill=NA),
          strip.text.x=element_blank(),
          plot.margin=margin(l=0.5,t=0.5,r=0.5,b=0.5,unit="cm")
    )
}



## function to extract legend from plot  ---- 
get_only_legend <- function(plot) {
  
  # get tabular interpretation of plot
  plot_table <- ggplot_gtable(ggplot_build(plot)) 
  
  #  Mark only legend in plot
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
  
  # extract legend
  legend <- plot_table$grobs[[legend_plot]]
  
  # return legend
  return(legend) 
}


# load the cleaned and joined dataset -------------------------------------

knox22 <- readRDS('data/knox22_joined.RDS') 


# calculate general summary stats ---------------------------------------------------

## percent of each species per site 

knox22_bysite <- sf::st_drop_geometry(knox22) %>%
  group_by(site_id) %>%
  summarize(total_aedes = sum(total_albo+total_tris+total_jap,na.rm=TRUE),
            perc_albo= sum(total_albo,na.rm=TRUE)/total_aedes*100,
            perc_tris= sum(total_tris,na.rm=TRUE)/total_aedes*100,
            perc_jap= sum(total_jap,na.rm=TRUE)/total_aedes*100) 


# export this and join with site coordinates to make map of proportion species in QGIS

## total eggs ----

(total_eggs <- sum(knox22$total_eggs, na.rm=TRUE)) # 89,400 eggs
(total_albo <- sum(knox22$total_albo, na.rm=TRUE)) # 17,019 albo
(total_tris <- sum(knox22$total_tris, na.rm=TRUE)) # 18,296 tris
(total_jap <- sum(knox22$total_jap, na.rm=TRUE)) # 270 jap

total_albo/(total_albo+total_tris+total_jap) # 47.8% albo
total_tris/(total_albo+total_tris+total_jap) # 51.4% tris
total_jap/(total_albo+total_tris+total_jap) # 0.76% jap

(total_albo+total_tris+total_jap)/total_eggs # 39.8% egg-to-adult percentage

## summary stats by dominant land cover ----

(summary_month_landcover250m <- knox22 %>%
  group_by(domland_250m, calendar_month) %>%
  summarise(mean_tris = mean(total_tris, na.rm=TRUE),
            se_tris = parameters::standard_error(total_tris, na.rm=TRUE),
            mean_albo = mean(total_albo, na.rm=TRUE),
            se_albo = parameters::standard_error(total_albo, na.rm=TRUE),
            mean_jap = mean(total_jap, na.rm=TRUE),
            se_jap = parameters::standard_error(total_jap, na.rm=TRUE))
)

## summary stats by month ----

(summary_month <- knox22 %>%
  group_by(calendar_month) %>%
  summarise(mean_tris = mean(total_tris, na.rm=TRUE),
            se_tris = parameters::standard_error(total_tris, na.rm=TRUE),
            mean_albo = mean(total_albo, na.rm=TRUE),
            se_albo = parameters::standard_error(total_albo, na.rm=TRUE),
            mean_jap = mean(total_jap, na.rm=TRUE),
            se_jap = parameters::standard_error(total_jap, na.rm=TRUE),
            mean_eggs = mean(total_eggs,na.rm=TRUE),
            se_eggs = parameters::standard_error(total_eggs,na.rm=TRUE))
)

## TABLE 2 basic summary stats at 250m and 1000m----

(summary_landcover <- knox22 %>%
  group_by(domland_250m) %>%
  summarise(sum_eggs=sum(total_eggs, na.rm=TRUE),
            mean_eggs = mean(total_eggs, na.rm=TRUE),
            sd_eggs = sd(total_eggs, na.rm=TRUE),
            sum_tris=sum(total_tris, na.rm=TRUE),
            median_tris = median(total_tris,na.rm=TRUE),
            iqr_tris = IQR(total_tris, na.rm=TRUE),
            mean_tris = mean(total_tris, na.rm=TRUE),
            sd_tris = sd(total_tris, na.rm=TRUE),
            se_tris = parameters::standard_error(total_tris, na.rm=TRUE),
            sum_albo=sum(total_albo, na.rm=TRUE),
            median_albo = median(total_albo,na.rm=TRUE),
            iqr_albo = IQR(total_albo, na.rm=TRUE),
            mean_albo = mean(total_albo, na.rm=TRUE),
            sd_albo = sd(total_albo, na.rm=TRUE),
            se_albo = parameters::standard_error(total_albo, na.rm=TRUE),
            sum_jap=sum(total_jap, na.rm=TRUE),
            median_jap = median(total_jap,na.rm=TRUE),
            iqr_jap = IQR(total_jap, na.rm=TRUE),
            mean_jap = mean(total_jap, na.rm=TRUE),
            sd_jap = sd(total_jap, na.rm=TRUE),
            se_jap = parameters::standard_error(total_jap, na.rm=TRUE),
            percent_tris = sum_tris/(sum_tris+sum_albo+sum_jap)*100,
            percent_albo = sum_albo/(sum_tris+sum_albo+sum_jap)*100,
            percent_jap = sum_jap/(sum_tris+sum_albo+sum_jap)*100)
 )

(summary_landcover <- knox22 %>%
    group_by(domland_1000m) %>%
    summarise(sum_eggs=sum(total_eggs, na.rm=TRUE),
              mean_eggs = mean(total_eggs, na.rm=TRUE),
              sd_eggs = sd(total_eggs, na.rm=TRUE),
              sum_tris=sum(total_tris, na.rm=TRUE),
              median_tris = median(total_tris,na.rm=TRUE),
              iqr_tris = IQR(total_tris, na.rm=TRUE),
              mean_tris = mean(total_tris, na.rm=TRUE),
              sd_tris = sd(total_tris, na.rm=TRUE),
              se_tris = parameters::standard_error(total_tris, na.rm=TRUE),
              sum_albo=sum(total_albo, na.rm=TRUE),
              median_albo = median(total_albo,na.rm=TRUE),
              iqr_albo = IQR(total_albo, na.rm=TRUE),
              mean_albo = mean(total_albo, na.rm=TRUE),
              sd_albo = sd(total_albo, na.rm=TRUE),
              se_albo = parameters::standard_error(total_albo, na.rm=TRUE),
              sum_jap=sum(total_jap, na.rm=TRUE),
              median_jap = median(total_jap,na.rm=TRUE),
              iqr_jap = IQR(total_jap, na.rm=TRUE),
              mean_jap = mean(total_jap, na.rm=TRUE),
              sd_jap = sd(total_jap, na.rm=TRUE),
              se_jap = parameters::standard_error(total_jap, na.rm=TRUE),
              percent_tris = sum_tris/(sum_tris+sum_albo+sum_jap)*100,
              percent_albo = sum_albo/(sum_tris+sum_albo+sum_jap)*100,
              percent_jap = sum_jap/(sum_tris+sum_albo+sum_jap)*100)
)


# plot species together by month ------------------------------------------

## create a long version of species by month ----

# gather by mean
summ_month_long1 <- summary_month %>%
  gather(species, mean, c('mean_tris','mean_albo','mean_jap')) %>%
  mutate(species = case_when(species=='mean_tris'~'Aedes triseriatus',
                             species=='mean_albo'~'Aedes albopictus',
                             species=='mean_jap'~'Aedes japonicus')) %>%
  dplyr::select(calendar_month, species, mean)

# gather by se 
summ_month_long2 <- summary_month %>%
  gather(species, se, c('se_tris','se_albo','se_jap')) %>%
  mutate(species = case_when(species=='se_tris'~'Aedes triseriatus',
                             species=='se_albo'~'Aedes albopictus',
                             species=='se_jap'~'Aedes japonicus')) %>%
  dplyr::select(calendar_month, species, se)

# join them 

summ_month_long <- left_join(summ_month_long1, summ_month_long2,
                             by=c('calendar_month',
                                  'species'))

pos=position_dodge(width=0.3)

summ_month_long$species <- factor(summ_month_long$species, 
                                  levels=c('Aedes albopictus',
                                           'Aedes triseriatus',
                                           'Aedes japonicus'))

(species_bymonth_plot <- ggplot(summ_month_long, aes(x=calendar_month,y=mean,
                            ymin=mean-se,
                            ymax=mean+se,
                            shape=species,
                            color=species,
                            fill=species,
                            group = species)) +
  geom_line(size=1, alpha=0.5, pos=pos) +
  geom_errorbar(size=1.2, width = 0, pos=pos) +
  geom_point(size=8, pos=pos) +
  scale_y_continuous(name="Mean Reared from Eggs") +
  scale_x_discrete(name="Month of Collection",
                   labels = c('Jun','Jul','Aug','Sep','Oct')) +
  scale_color_manual(values=c('#1b9e77', '#7570b3', '#d95f02')) + 
  scale_shape_manual(values=c(19,15,18))+
  # remove lines from legend
  guides(fill = guide_legend(override.aes = list(linetype = 0)),
         color = guide_legend(override.aes = list(linetype = 0))) +
  custom_theme(legend_pos='top') + 
  theme(axis.title.x = element_text(vjust = -0.5),
        plot.title.position="plot",
        plot.margin=unit(c(0,0.7,0.5,0.5),"cm"),
        legend.text=element_text(face='bold.italic',size=20)))


legend_only <- get_only_legend(species_bymonth_plot)

#legend_plot <- readRDS('data/plot_grid_legend.rds')


# Plots of abundance by dominant land cover  ----------------------------------

## 250m dominant land cover ----

### prepare longform data ----

lc_long_250m <- knox22 %>% group_by(domland_250m) %>%
  gather(species, total, c('total_tris','total_albo','total_jap')) %>%
  mutate(species = case_when(species=='total_tris'~'Aedes triseriatus',
                             species=='total_albo'~'Aedes albopictus',
                             species=='total_jap'~'Aedes japonicus')) %>%
  dplyr::select(calendar_month, species, total) %>%
  mutate(species=factor(species, levels=c('Aedes albopictus',
                                          'Aedes triseriatus',
                                          'Aedes japonicus')))

### plot 250m dom landcover data ---- 

(by_domland_250m <- ggplot(data=lc_long_250m, aes(x=domland_250m, y=total, color=species)) + 
  geom_boxplot(outlier.shape = NA, size=1) +
  coord_cartesian(ylim=c(0,310)) + # set axis limits
  scale_y_continuous(expand=c(0,0), # expand y axis to fill entire vertical space
                     name=element_blank())+ # no axis label (will add global label later)
  scale_x_discrete(name='Dominant Land Cover (250m)', 
                   labels=c("Forest","Open","Built"))+
  scale_color_manual(values=c('#d95f02','#7570b3','#1b9e77'),
                     labels=c("Aedes albopictus","Aedes triseriatus",
                              "Aedes japonicus")) +
  custom_theme(legend_pos='none')
)

## 1000m dominant land cover ----

### prepare 1000m longform data ----

lc_long_1000m <- knox22 %>% group_by(domland_1000m) %>%
  gather(species, total, c('total_tris','total_albo','total_jap')) %>%
  mutate(species = case_when(species=='total_tris'~'Aedes triseriatus',
                             species=='total_albo'~'Aedes albopictus',
                             species=='total_jap'~'Aedes japonicus')) %>%
  dplyr::select(calendar_month, species, total) %>%
  mutate(species=factor(species, levels=c('Aedes albopictus',
                                          'Aedes triseriatus',
                                          'Aedes japonicus')))

### plot 1000m dom landcover data ----

(by_domland_1000m <- ggplot(data=lc_long_1000m, aes(x=domland_1000m, y=total, color=species)) + 
  geom_boxplot(outlier.shape = NA, size=1) +
  coord_cartesian(ylim=c(0,300)) + # set limits of y axis
  scale_y_continuous(expand=c(0,0), # expand y axis to fill entire vertical space
                     name=element_blank()) + # no label, will add global label later
  scale_x_discrete(name='Dominant Land Cover (1,000m)', 
                   labels=c("Forest","Open","Built"))+
  scale_color_manual(values=c('#d95f02','#7570b3','#1b9e77'),
                     labels=c("Aedes albopictus","Aedes triseriatus",
                              "Aedes japonicus")) +
  custom_theme(legend_pos = 'none')
)

# Plot abundance by percent forest ----------------------------------------

# create vector for x axis labels 

forest_labs <- c('0–19','20–39','40–59','61–79', '80–99')
## Plot by % forest at 250m scale ----

### prep longform mosquitoes by percent forest 250m ----

forest_long_250m <- knox22 %>% group_by(percent_forest_250m) %>%
  gather(species, total, c('total_tris','total_albo','total_jap')) %>%
  mutate(species = case_when(species=='total_tris'~'Aedes triseriatus',
                             species=='total_albo'~'Aedes albopictus',
                             species=='total_jap'~'Aedes japonicus')) %>%
  dplyr::select(calendar_month, species, total) %>%
  mutate(species=factor(species, levels=c('Aedes albopictus',
                                          'Aedes triseriatus',
                                          'Aedes japonicus')),
         bin = cut(percent_forest_250m, breaks=seq(0, 100, by=20),right=FALSE))

### plot mosquitoes by percent forest 250m ----

(byforest_250m <- ggplot(forest_long_250m, 
       aes(x=bin, # x axis is the 10% bins for percent forest
           y=total, # y axis is total number of mosquitoes 
           color=species, # color by species
           group=interaction(species,bin))) + # group species by forest bin 
  geom_boxplot(size=1, width=1, outlier.shape=NA) + # define boxplot size and suppress outliers
  scale_x_discrete(name='% Forest (250m)', # name for x axis 
                   labels=forest_labs) + # axis tick labels (labels defined outside of ggplot)
  coord_cartesian(ylim=c(0,300)) +
  scale_y_continuous(expand=c(0,0), # fill entire vertical space
                     name=element_blank()) + # no axis label, will add global label later 
  scale_color_manual(values=c('#d95f02','#7570b3','#1b9e77'), # manually define species colors 
                     labels=c("Aedes albopictus","Aedes triseriatus","Aedes japonicus")) + # define species labels
  custom_theme(legend_pos = 'none') # use custom theme, defined at beginning of script
)

## Plot by % forest at 1000m scale ----

### prep longform mosquitoes by percent forest 1000m ----

forest_long_1000m <- knox22 %>% group_by(percent_forest_1000m) %>%
  gather(species, total, c('total_tris','total_albo','total_jap')) %>%
  mutate(species = case_when(species=='total_tris'~'Aedes triseriatus',
                             species=='total_albo'~'Aedes albopictus',
                             species=='total_jap'~'Aedes japonicus')) %>%
  dplyr::select(calendar_month, species, total) %>%
  mutate(species=factor(species, levels=c('Aedes albopictus',
                                          'Aedes triseriatus',
                                          'Aedes japonicus')),
         bin = cut(percent_forest_1000m, breaks=seq(0, 100, by=20),right=FALSE))

### plot mosquitoes by percent forest 1000m ----

(byforest_1000m <- ggplot(forest_long_1000m, 
       aes(x=bin, # x axis is the 10% bins for percent forest
           y=total, # y axis is total number of mosquitoes 
           color=species, # color by species
           group=interaction(species,bin))) + # group species by forest bin 
  geom_boxplot(size=1, width=1, outlier.shape=NA) + # define boxplot size and suppress outliers
  scale_x_discrete(name='% Forest (1,000m)', # name for x axis 
                   drop=FALSE, # do not drop empty bins in x axis 
                   labels=forest_labs) + # axis tick labels (labels defined outside of ggplot)
  coord_cartesian(ylim=c(0,410)) + # limits of y axis 
  scale_y_continuous(expand=c(0,0), # fill entire vertical space 
                     name=element_blank()) + # no axis label, will add global label later 
  scale_color_manual(values=c('#d95f02','#7570b3','#1b9e77'), # manually define species colors 
                     labels=c("Aedes albopictus","Aedes triseriatus","Aedes japonicus")) + # define species labels
  custom_theme(legend_pos = 'none') # use custom theme, defined at beginning of script
)



# combine the plots into one grid -----------------------------------------

## get legend for combined plots ----

# need to get an object that is the legend only for inclusion as global legend in combined plots

legend_only <- get_only_legend( # function defined at start of script
  
  ggplot(data=lc_long_250m, aes(x=domland_250m, y=total, color=species)) + 
     geom_boxplot(outlier.shape = NA, size=1) +
     scale_color_manual(values=c('#d95f02','#7570b3','#1b9e77'),
                        labels=c("Aedes albopictus","Aedes triseriatus",
                                 "Aedes japonicus")) +
     custom_theme(legend_pos='top')
  )

## create grid of combined plots ----

combined_plot <- grid.arrange(by_domland_250m, by_domland_1000m,
                        byforest_250m, byforest_1000m,
                        ncol=2, # 2 columns
                        left = text_grob("Total Reared From Eggs", # set global y axis label
                                         rot=90,vjust=1,size=30, face='bold', family='Century Gothic'))

#### join grid with global legend ---- 
grid.arrange(legend_only, # legend comes first 
             combined_plot, # combined plot grid comes next
             nrow=2, # two rows (legend first, then plot grid)
             heights=c(0.6,10)) # legend size:plot size ratio = 0.6:10

