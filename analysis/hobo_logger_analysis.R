#### load packages ----

library(tidyverse)

#### load data ----

loggers <- read_rds('data/Weather Data/cleaned_joined_HOBO.RDS')


#### load functions ----

custom_theme <- function(legpos = c(1,1)){
  theme_bw() + 
    theme(plot.title = element_text(size = 24, face = "bold"),
          axis.title.x = element_text(size = 18, face = 'bold'),
          axis.title.y = element_text(size = 18, face = 'bold', vjust = 2.0),
          axis.text.x = element_text(size = 16, color = 'black', face='bold'),
          axis.text.y = element_text(size = 16, color = 'black', face='bold'),
          strip.text = element_text(size = 16, face = 'bold'),
          legend.text = element_text(size= 16,face='bold'),
          legend.title=element_blank(),
          legend.background = element_rect(linetype='solid', color = 'black'),
          legend.justification = legpos,
          legend.position = legpos
    )
}


#### Summarize data ----
summarize.data <- function(df, group1, group2, group3, group4, filter){ 
  group1 <- enquo(group1)
  group2 <- enquo(group2)
  group3 <- enquo(group3)
  group4 <- enquo(group4)
  
  df1 <- df %>%
    group_by(!!group1, !!group2, !!group3, !!group4) %>%
    filter(site != filter) %>%
    summarize(           mean_temp = mean(temp),
                         max_temp=mean(max(temp)),
                         min_temp=mean(min(temp)),
                         mean_rh = mean(rh),
                         max_rh=mean(max(rh)),
                         min_rh=mean(min(rh))
    )
  
}

daily.plot <- function(df, xcol, ycol, ylab, facet, 
                       ylim, title,legpos) {
  
  facet <- enquo(facet)
  #graph.name <-  paste(df$year,title)
  pos <- position_dodge(width=0.5)
  
  plot <- ggplot2::ggplot(data = df, aes(x = {{ xcol }}, y = {{ ycol }}, 
                                         color={{ facet }}, 
                                         ))+
    geom_line(linetype='solid',size=1.5,alpha=0.8)+
    #geom_point(aes(y = {{ ycol }}, x = {{ xcol }} ), size = 3, position = pos) +
    scale_y_continuous(lim=ylim, name = ylab) +
    scale_color_manual(values=c('#1b9e77', '#7570b3', '#d95f02'))+
    labs(x="Date",color="")+
    ggtitle(title) +
    custom_theme(legpos=legpos)
  
  print(plot) 
  #ggsave(plot, file = paste(graph.name,sliced_or_full, ".png", sep = ''), device = "png", path = "Output/", dpi=300) # Save plots to output folder
}



#### 4. Summarize data ----

daily_summ <- summarize.data(loggers, site, dominant_landcover, month, date,
                             filter = 'MV')

daily_summ_nosite <- summarize.data(loggers, dominant_landcover, month, date,
                                    filter = 'MV')

#### plot data ----

mean_temp <- daily.plot(df=daily_summ_nosite, xcol=date, ycol=mean_temp, ylab = 'Daily Temp (°C)', 
           facet = dominant_landcover, 
           ylim=c(10,30), title = 'Mean daily temperature',
           legpos=c(1,0))

min_temp <- daily.plot(df=daily_summ_nosite, xcol=date, ycol=min_temp, ylab = 'Daily Temp (°C)', 
                        facet = dominant_landcover, 
                        ylim=c(0,25), title = 'Minimum daily temperature',
                       legpos=c(100,0))

max_temp <- daily.plot(df=daily_summ_nosite, xcol=date, ycol=max_temp, ylab = 'Daily Temp (°C)', 
                       facet = dominant_landcover, 
                       ylim=c(15,40), title = 'Maximum daily temperature',
                       legpos=c(1,1))

mean_rh <- daily.plot(df=daily_summ_nosite, xcol=date, ycol=mean_rh, ylab = 'Daily RH', 
                        facet = north_south, 
                        ylim=c(0,100), title = 'Mean daily relative humidity',
                        legpos=c(100,0))

min_rh <- daily.plot(df=daily_summ_nosite, xcol=date, ycol=min_rh, ylab = 'Daily RH', 
                       facet = north_south, 
                       ylim=c(0,100), title = 'Minimum daily relative humidity',
                       legpos=c(100,0))

max_rh <- daily.plot(df=daily_summ_nosite, xcol=date, ycol=max_rh, ylab = 'Daily RH', 
                       facet = north_south, 
                       ylim=c(0,100), title = 'Maximum daily relative humidity',
                       legpos=c(1,0))


cowplot::plot_grid(max_temp, max_rh, 
                   mean_temp, mean_rh,
                   min_temp, min_rh,
                   ncol=2)

mean_temp <- ggplot(daily_summ_nosite, aes(x=date,color=north_south,y=mean_temp))+
  geom_point(position=position_dodge(width=0.5)) +
  geom_line() +
  scale_y_continuous(lim=c(10,30), seq(10,30,2)) +
  theme_bw()

min_temp <- ggplot(daily_summ_nosite, aes(x=date,color=north_south,y=min))+
  geom_point(position=position_dodge(width=0.5)) +
  geom_line() +
  scale_y_continuous(lim=c(0,25), seq(0,25,2)) +
  theme_bw()

max_temp <- ggplot(daily_summ_nosite, aes(x=date,color=north_south,y=max))+
  geom_point(position=position_dodge(width=0.5)) +
  geom_line() +
  scale_y_continuous(lim=c(15,40), seq(15,40,2)) +
  theme_bw()



# RH ----
daily_summ_rh <- loggers %>%
  # remove MV because of stupid high max rhs
  filter(site!='MV') %>%
  group_by(dominant_landcover, site,month, date) %>%
  summarize(mean = mean(rh),
            mean_se=sd(rh/sqrt(n()),na.rm=TRUE), 
            max=mean(max(rh)),
            max_se=sd(max(rh)/sqrt(n()),na.rm=TRUE),
            min=mean(min(rh)),
            min_se=sd(min(rh)/sqrt(n()),na.rm=TRUE)
  )


daily_summ_nosite_rh <- loggers %>%
  # remove MV because of stupid high max rhs
  filter(site!='MV') %>%
  group_by(dominant_landcover, month, date) %>%
  summarize(mean = mean(rh),
            mean_se=sd(rh/sqrt(n()),na.rm=TRUE), 
            max=mean(max(rh)),
            max_se=sd(max(rh)/sqrt(n()),na.rm=TRUE),
            min=mean(min(rh)),
            min_se=sd(min(rh)/sqrt(n()),na.rm=TRUE)
  )

mean_rh <- ggplot(daily_summ_nosite_rh, aes(x=date,color=dominant_landcover,y=mean))+
  geom_point(position=position_dodge(width=0.5)) +
  geom_line() +
  #scale_y_continuous(lim=c(0,40), seq(0,40,2)) +
  theme_bw()

min_rh <- ggplot(daily_summ_nosite_rh, aes(x=date,color=dominant_landcover,y=min))+
  geom_point(position=position_dodge(width=0.5)) +
  geom_line() +
  #scale_y_continuous(lim=c(0,40), seq(0,40,2)) +
  theme_bw()

max_rh <- ggplot(daily_summ_nosite_rh, aes(x=date,color=dominant_landcover,y=max))+
  geom_point(position=position_dodge(width=0.5)) +
  geom_line() +
  #scale_y_continuous(lim=c(0,40), seq(0,40,2)) +
  theme_bw()

cowplot::plot_grid(mean_rh,min_rh,max_rh,ncol=1)

check <- loggers %>%
  group_by(site) %>%
  summarize(lc = unique(dominant_landcover))
## mean temp ----

# interaction is not significant 
# north south IS signifcant; south had lower mean daily temps 
mean_temp_mod <- lme4::lmer(mean ~ dominant_landcover + month + (1|site), 
                            data = daily_summ)
summary(mean_temp_mod)

mean_temp_mod_red <- lme4::lmer(mean ~  month  + (1|site), 
                                data = daily_summ)
anova(mean_temp_mod_red, mean_temp_mod)
sjPlot::tab_model(mean_temp_mod)

## min temp ----
# min temp is significantly lower at south sites 
# interaction of northsouth and month is not significant 

min_temp_mod <- lme4::lmer(min ~ dominant_landcover + month + (1|site), 
                           data = daily_summ)
summary(min_temp_mod)

min_temp_mod_red <- lme4::lmer(min ~  month (1|site), 
                               data = daily_summ)
anova(min_temp_mod_red, min_temp_mod)
  sjPlot::tab_model(min_temp_mod)

min_temp_mod <- lme4::lmer(min ~ percent_developed_250m + month + (1|site), 
                           data = daily_summ)
summary(min_temp_mod)

min_temp_mod_red <- lme4::lmer(min ~  month (1|site), 
                               data = daily_summ)
anova(min_temp_mod_red, min_temp_mod)
sjPlot::tab_model(min_temp_mod)


## max temp ----
# max temp not significantly different for north south 
# interaction between month and north south IS significant for max temp 
## but not if MV is removed....

max_temp_mod <- lme4::lmer(max ~ dominant_landcover + month +(1|site), 
                           data = daily_summ)
summary(max_temp_mod)

max_temp_mod_red <- lme4::lmer(max ~  month  + (1|site), 
                               data = daily_summ)
anova(max_temp_mod_red, max_temp_mod)
sjPlot::tab_model(max_temp_mod)


## mean rh ----

# interaction is not significant 
# north south IS signifcant; south had lower mean daily rhs 

# significant difference; south is more humid on average
mean_rh_mod <- lme4::lmer(mean ~ dominant_landcover + month + (1|site), 
                          data = daily_summ_rh)
summary(mean_rh_mod)

mean_rh_mod_red <- lme4::lmer(mean ~  month  + (1|site), 
                              data = daily_summ_rh)
anova(mean_rh_mod_red, mean_rh_mod)
sjPlot::tab_model(mean_rh_mod)

## min rh ----

# significant difference; min rh is significantly higher in the south 
min_rh_mod <- lme4::lmer(min ~ dominant_landcover + month + (1|site), 
                         data = daily_summ_rh)
summary(min_rh_mod)

min_rh_mod_red <- lme4::lmer(min ~  month + (1|site), 
                             data = daily_summ_rh)
anova(min_rh_mod_red, min_rh_mod)
sjPlot::tab_model(min_rh_mod)

## max rh ----
# significant difference; rh is higher in the south

max_rh_mod <- lme4::lmer(max ~ dominant_landcover + month +(1|site), 
                         data = daily_summ_rh)
summary(max_rh_mod)

max_rh_mod_red <- lme4::lmer(max ~  month  + (1|site), 
                             data = daily_summ_rh)
anova(max_rh_mod_red, max_rh_mod)
sjPlot::tab_model(max_rh_mod)