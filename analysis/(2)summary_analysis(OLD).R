#### load packages ----

library(tidyverse)
library(glmmTMB)
library(extrafont)



# functions ---------------------------------------------------------------

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


# calculate percent species per site --------------------------------------

# calculate percent of each species per site 
knox22_bysite <- knox22 %>%
  group_by(site_id) %>%
  summarize(total_aedes = sum(total_albo+total_tris+total_jap,na.rm=TRUE),
            perc_albo= sum(total_albo,na.rm=TRUE)/total_aedes*100,
            perc_tris= sum(total_tris,na.rm=TRUE)/total_aedes*100,
            perc_jap= sum(total_jap,na.rm=TRUE)/total_aedes*100) 


# export this and join with site coordinates to make map of proportion species in QGIS


# general summary stats ---------------------------------------------------


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
            se_jap = parameters::standard_error(total_jap, na.rm=TRUE),
            mean_eggs = mean(total_eggs,na.rm=TRUE),
            se_eggs = parameters::standard_error(total_eggs,na.rm=TRUE))
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

## TABLE 1 basic summary stats at 250m ----

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


# create a long version of species by month -------------------------------

# gather by mean
summ_month_long1 <- summary_month %>%
  gather(species, mean, c('mean_tris','mean_albo','mean_jap')) %>%
  mutate(species = case_when(species=='mean_tris'~'Aedes triseriatus',
                             species=='mean_albo'~'Aedes albopictus',
                             species=='mean_jap'~'Aedes j. japonicus')) %>%
  select(calendar_month, species, mean)

# gather by se 
summ_month_long2 <- summary_month %>%
  gather(species, se, c('se_tris','se_albo','se_jap')) %>%
  mutate(species = case_when(species=='se_tris'~'Aedes triseriatus',
            species=='se_albo'~'Aedes albopictus',
            species=='se_jap'~'Aedes j. japonicus')) %>%
  select(calendar_month, species, se)

# join them 

summ_month_long <- left_join(summ_month_long1, summ_month_long2,
                             by=c('calendar_month',
                                  'species'))


# create a long version of species by landcover ---------------------------

# gather by mean
summ_landcover_long1 <- summary_landcover %>%
  gather(species, mean, c('mean_tris','mean_albo','mean_jap')) %>%
  mutate(species = case_when(species=='mean_tris'~'Aedes triseriatus',
                             species=='mean_albo'~'Aedes albopictus',
                             species=='mean_jap'~'Aedes j. japonicus')) %>%
  select(domland_250m, species, mean)

# gather by se 
summ_landcover_long2 <- summary_landcover %>%
  gather(species, se, c('se_tris','se_albo','se_jap')) %>%
  mutate(species = case_when(species=='se_tris'~'Aedes triseriatus',
                             species=='se_albo'~'Aedes albopictus',
                             species=='se_jap'~'Aedes j. japonicus')) %>%
  select(domland_250m, species, se)

# join them 

summ_landcover_long <- left_join(summ_landcover_long1, summ_landcover_long2,
                             by=c('domland_250m',
                                  'species'))


# plot species together by month ------------------------------------------

pos=position_dodge(width=0.3)

summ_month_long$species <- factor(summ_month_long$species, 
                                  levels=c('Aedes albopictus',
                                           'Aedes triseriatus',
                                           'Aedes j. japonicus'))

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
  scale_y_continuous(name="Mean Egg Abundance") +
  scale_x_discrete(name="Month of Collection",
                   labels = c('Jun','Jul','Aug','Sep','Oct')) +
  scale_color_manual(values=c('#1b9e77', '#7570b3', '#d95f02')) + 
  scale_shape_manual(values=c(19,15,18))+
  ggtitle("(A)") +
  # remove lines from legend
  guides(fill = guide_legend(override.aes = list(linetype = 0)),
         color = guide_legend(override.aes = list(linetype = 0))) +
  custom_theme(legend_pos=c(100,0)) + 
  theme(axis.title.x = element_text(vjust = -0.5),
        plot.title.position="plot",
        plot.margin=unit(c(0,0.7,0.5,0.5),"cm"),
        text = element_text(family="Century Gothic"),
        legend.text=element_text(face='italic',size=20)))



legend_plot <- get_only_legend(species_bymonth_plot)
#legend_plot <- readRDS('data/plot_grid_legend.rds')

#### plot species separately by land cover ----

# create a plot function for byspecies plots 

byspecies.plot <- function(data, xvar, yvar, se, aesvar){
  pos=position_dodge(width=0.3)
  ggplot(data, aes(x = {{ xvar }}, y = {{ yvar }},
                   ymin = {{ yvar }} - {{ se }},
                   ymax = {{ yvar }} + {{ se }},
                   color = {{ aesvar }},
                   shape = {{ aesvar }},
                   group = {{ aesvar }})) + 
    geom_point(size=8, pos=pos) +
    geom_errorbar(size=1.2, linetype=1, width=0, pos=pos) +
    #geom_line(size=1, pos=pos) +
    scale_y_continuous(name="Mean Egg Abundance",
                       lim=c(0,100),
                       breaks=seq(0,100,25),) +
    scale_x_discrete(name="Dominant Land Cover",
                     labels=c("Forest","Open",
                              "Built")) +
    xlab("Dominant Land Cover") +
    scale_color_manual(values=c('#1b9e77', '#7570b3', '#d95f02')) +
    scale_shape_manual(values=c(19,15,18),
                     labels=c("Aedes albopictus","Aedes triseriatus",
                              "Aedes j. japonicus"))+ 
    ggtitle("(B)") +
    custom_theme(legend_pos=c(100,0)) + 
    theme(axis.title.x=element_text(vjust=-0.5),
          plot.title.position="plot",
          plot.margin=unit(c(0,0.7,0.5,0.5),"cm"))
}

# plot by land cover and month

summ_landcover_long$species <- factor(summ_landcover_long$species, 
                                      levels=c('Aedes albopictus',
                                               'Aedes triseriatus',
                                               'Aedes j. japonicus'))
(allspecies_domlandcover_plot <- byspecies.plot(
  data=summ_landcover_long, 
  xvar=domland_250m, 
  yvar=mean, 
  se=se,
  aesvar=species))

lc_long_250m <- knox22 %>% group_by(domland_250m) %>%
  gather(species, total, c('total_tris','total_albo','total_jap')) %>%
  mutate(species = case_when(species=='total_tris'~'Aedes triseriatus',
                             species=='total_albo'~'Aedes albopictus',
                             species=='total_jap'~'Aedes j. japonicus')) %>%
  select(calendar_month, species, total)

lc_long_1000m <- knox22 %>% group_by(domland_1000m) %>%
  gather(species, total, c('total_tris','total_albo','total_jap')) %>%
  mutate(species = case_when(species=='total_tris'~'Aedes triseriatus',
                             species=='total_albo'~'Aedes albopictus',
                             species=='total_jap'~'Aedes j. japonicus')) %>%
  select(calendar_month, species, total)

ggplot(data=lc_long_250m, aes(x=domland_250m, y=total, color=species)) + 
  geom_boxplot() +
  theme_classic()

ggplot(data=lc_long_1000m, aes(x=domland_1000m, y=total, color=species)) + 
  geom_boxplot() +
  theme_classic()

(summary_built <- knox22 %>%
    group_by(site_id,percent_built_250m) %>%
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
              percent_jap = sum_jap/(sum_tris+sum_albo+sum_jap)*100))



# gather by mean
summ_built_long1 <- summary_built %>%
  gather(species, mean, c('mean_tris','mean_albo','mean_jap')) %>%
  mutate(species = case_when(species=='mean_tris'~'Aedes triseriatus',
                             species=='mean_albo'~'Aedes albopictus',
                             species=='mean_jap'~'Aedes j. japonicus')) %>%
  select(percent_built_250m, species, mean, site_id)

summ_built_long2 <- summary_built %>%
  gather(species, se, c('se_tris','se_albo','se_jap')) %>%
  mutate(species = case_when(species=='se_tris'~'Aedes triseriatus',
                             species=='se_albo'~'Aedes albopictus',
                             species=='se_jap'~'Aedes j. japonicus')) %>%
  select(percent_built_250m, species, se,site_id)

# join em 

summ_built_long <- left_join(summ_built_long1, summ_built_long2,
                                 by=c('percent_built_250m',
                                      'species','site_id'))

#### plot species together by month ----
pos=position_dodge(width=0.3)
summ_built_long$species <- factor(summ_built_long$species, 
                                  levels=c('Aedes albopictus',
                                           'Aedes triseriatus',
                                           'Aedes j. japonicus'))
(species_byurban_plot <- ggplot(summ_built_long, aes(x=percent_built_250m
                                                     ,y=mean,
                                                     ymin=mean-se,
                                                     ymax=mean+se,
                                                     shape=species,
                                                     color=species,
                                                     fill=species,
                                                     group = species)) +
   # geom_line(size=1, alpha=0.5) +
    geom_errorbar(size=1.2, width = 0) +
    geom_point(size=6) +
    scale_y_continuous(name="Mean Eggs") +
    scale_x_continuous(name="% Built (250m)") +
    scale_color_manual(values=c('#1b9e77', '#7570b3', '#d95f02')) + 
    scale_shape_manual(values=c(19,15,18))+
    ggtitle("(C)") +
    # remove lines from legend
    guides(fill = guide_legend(override.aes = list(linetype = 0)),
           color = guide_legend(override.aes = list(linetype = 0))) +
    custom_theme(legend_pos=c(100,0)) + 
    theme(axis.title.x = element_text(vjust = -0.5),
          plot.title.position="plot",
          plot.margin=unit(c(0,0.7,0.5,0.5),"cm"),
          text = element_text(family="Century Gothic"),
          legend.text=element_text(face='italic',size=20)))


(summary_forest <- knox22 %>%
    group_by(site_id,percent_forest_1000m) %>%
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
              percent_jap = sum_jap/(sum_tris+sum_albo+sum_jap)*100))



# gather by mean
summ_forest_long1 <- summary_forest %>%
  gather(species, mean, c('mean_tris','mean_albo','mean_jap')) %>%
  mutate(species = case_when(species=='mean_tris'~'Aedes triseriatus',
                             species=='mean_albo'~'Aedes albopictus',
                             species=='mean_jap'~'Aedes j. japonicus')) %>%
  select(percent_forest_1000m, species, mean, site_id)

summ_forest_long2 <- summary_forest %>%
  gather(species, se, c('se_tris','se_albo','se_jap')) %>%
  mutate(species = case_when(species=='se_tris'~'Aedes triseriatus',
                             species=='se_albo'~'Aedes albopictus',
                             species=='se_jap'~'Aedes j. japonicus')) %>%
  select(percent_forest_1000m, species, se,site_id)

# join em 

summ_forest_long <- left_join(summ_forest_long1, summ_forest_long2,
                             by=c('percent_forest_1000m',
                                  'species','site_id'))

#### plot species together by month ----
pos=position_dodge(width=0.3)
summ_forest_long$species <- factor(summ_forest_long$species, 
                                  levels=c('Aedes albopictus',
                                           'Aedes triseriatus',
                                           'Aedes j. japonicus'))
(species_byforest_plot <- ggplot(summ_forest_long, aes(x=percent_forest_1000m
                                                     ,y=mean,
                                                     ymin=mean-se,
                                                     ymax=mean+se,
                                                     shape=species,
                                                     color=species,
                                                     fill=species,
                                                     group = species)) +
    # geom_line(size=1, alpha=0.5) +
    geom_errorbar(size=1.2, width = 0) +
    geom_point(size=6) +
    scale_y_continuous(name="Mean Eggs") +
    scale_x_continuous(name="% Forest (1,000m)") +
    scale_color_manual(values=c('#1b9e77', '#7570b3', '#d95f02')) + 
    scale_shape_manual(values=c(19,15,18))+
    ggtitle("(D)") +
    # remove lines from legend
    guides(fill = guide_legend(override.aes = list(linetype = 0)),
           color = guide_legend(override.aes = list(linetype = 0))) +
    custom_theme(legend_pos=c(100,0)) + 
    theme(axis.title.x = element_text(vjust = -0.5),
          plot.title.position="plot",
          plot.margin=unit(c(0,0.7,0.5,0.5),"cm"),
          text = element_text(family="Century Gothic"),
          legend.text=element_blank()))


combined_plot <- gridExtra::grid.arrange(species_bymonth_plot, allspecies_domlandcover_plot,
                                         species_byurban_plot,  species_byforest_plot,
                        nrow=2, ncol=2)
gridExtra::grid.arrange(legend_plot,combined_plot, nrow=2, heights=c(1,10))


#### plot rainfall at different weather stations ----

ggplot(knox22, aes(x=calendar_week, y=total_rainfall,
                   color=rainfall_station)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values=c('#1b9e77', '#7570b3', '#d95f02')) +
custom_theme(legend_pos = 'top')
