#### load packages ----
library('tidyverse')
library('glmmTMB')
library('buildmer')
library('DHARMa')
library('ggeffects')

#### load data ----

knox22 <- readRDS('data/Mosquito Data/cleaned_joined_knox22.RDS') %>%
  # standardize popdens to be per 1,000 people
  mutate(mean_popdens = mean_popdens/1000,
         total_aedes = total_albo+total_tris) %>%
  filter(Site_ID !='FS')
head(knox22)


#### check correlations ----

corcols <- c('mean_popdens','mean_income','mean_education','mean_yearstructure',
             'impervious','canopy','ndvi','percent_developed_250m')

round(cor(knox22[,corcols],method='pearson'),2)
# education and income are too correlated
# impervious, canopy, ndvi, and prop_developed are too correlated 

#### final multivariabel models ----

albo_drop4 <- glmmTMB(total_albo ~  total_tris + 
                        percent_developed_250m + calendar_month +
                        (1|Site_ID),
                      data=knox22, family='nbinom1')
summary(albo_drop4)
plot(simulateResiduals(fittedModel=albo_drop4, plot=T))

albo_drop4_gg <- ggeffects::ggpredict(albo_drop4,terms=c("percent_developed_250m"))
ggplot(albo_drop4_gg,aes(x,predicted)) + 
  geom_line() +
  geom_ribbon(aes(ymin=conf.low,ymax=conf.high,alpha=0.5)) +
  theme_bw()

tris_drop4 <- glmmTMB(total_tris ~
                        calendar_month + percent_developed_250m + (1|Site_ID),
                      data=knox22, family='nbinom2')
tris_null <- glmmTMB(total_tris ~ 1 + (1|Site_ID),
data=knox22, family='nbinom1')
anova(tris_drop4,tris_null) # deviance = 2673.1

sjPlot::tab_model(tris_drop4)


summary(tris_drop4)

simulateResiduals(fittedModel=tris_drop4, plot=T)


# test overdispersion 
df <- df.residual(tris_drop4)
residual_deviance <- 2673.1
pchisq(residual_deviance, df)

simulateResiduals(fittedModel=tris_drop4, plot=T)

tris_drop4_gg <- ggeffects::ggpredict(tris_drop4,terms=c("percent_developed_250m"))
ggplot(tris_drop4_gg,aes(x,predicted)) + 
  geom_line() +
  geom_ribbon(aes(ymin=conf.low,ymax=conf.high,alpha=0.5)) +
  theme_bw()

egg_drop3 <- glmmTMB(total_eggs ~ lagged_avgtemp_1week + 
                       calendar_month +
                       (1|Site_ID), data=knox22, family='nbinom1')
summary(egg_drop3) # final model, laggedtemp_1week and calendar month are significant  


simulateResiduals(fittedModel=egg_drop3, plot=T)

summary(albo_drop4) # final, percent developed and yearstructure are significant 

percent_albo_drop3 <- glmmTMB(total_albo ~ calendar_month + 
                                percent_developed_250m + 
                                total_eggs +offset(log(1+total_aedes)) +
                                (1|Site_ID),
                              data=knox22, family='nbinom1')
summary(percent_albo_drop3) # final, percent developed, and total eggs are significant

simulateResiduals(fittedModel=percent_albo_drop3, plot=T)

# univariable models ----

#### total eggs 

eggs_northsouth <- glmmTMB(total_eggs ~ north_south+ (1|Site_ID),
                        data=knox22, family='nbinom2')
summary(eggs_northsouth ) # sig

eggs_percent_developed <- glmmTMB(total_eggs ~ percent_developed_250m,
                           data=knox22, family='nbinom2')
summary(eggs_percent_developed) # sig

eggs_popdens <- glmmTMB(total_eggs ~ mean_popdens + (1|Site_ID),
                        data=knox22, family='nbinom2')
summary(eggs_popdens) # sig

eggs_income <- glmmTMB(total_eggs ~ mean_income,
                       data=knox22, family='nbinom2')
summary(eggs_income) #sig

eggs_yearstructure <- glmmTMB(total_eggs ~ mean_yearstructure,
                              data=knox22, family='nbinom2')
summary(eggs_yearstructure) # not sig 

eggs_ndvi <- glmmTMB(total_eggs ~ ndvi,
                     data=knox22, family='nbinom2')
summary(eggs_ndvi) # sig

eggs_avg_temp<- glmmTMB(total_eggs ~ mean_avg_temp,
                        data=knox22, family='nbinom2')
summary(eggs_avg_temp) # sig

eggs_avg_temp_lag1 <- glmmTMB(total_eggs ~ lagged_avgtemp_1week,
                              data=knox22, family='nbinom2')
summary(eggs_avg_temp_lag1) # sig - better than week of

eggs_avg_temp_lag2 <- glmmTMB(total_eggs ~ lagged_avgtemp_2week,
                              data=knox22, family='nbinom2')
summary(eggs_avg_temp_lag2) # not sig

eggs_avg_temp_lag3 <- glmmTMB(total_eggs ~ lagged_avgtemp_3week,
                              data=knox22, family='nbinom2')
summary(eggs_avg_temp_lag3) # not sig

eggs_precip <- glmmTMB(total_eggs ~ total_precip,
                            data=knox22, family='nbinom2')
summary(eggs_precip) # not sig

eggs_precip_lag1 <- glmmTMB(total_eggs ~ lagged_precip_1week,
                            data=knox22, family='nbinom2')
summary(eggs_precip_lag1) # not sig

eggs_precip_lag2 <- glmmTMB(total_eggs ~ lagged_precip_2week,
                            data=knox22, family='nbinom2')
summary(eggs_precip_lag2) # not sig

eggs_precip_lag3 <- glmmTMB(total_eggs ~ lagged_precip_3week,
                            data=knox22, family='nbinom2')
summary(eggs_precip_lag3) # not sig

# percent_developed, popdens, income, ndvi, avgtemp_1weeklag 


#### total eggs ----
# income, ndvi, avgtemp_1weeklag 
egg_fullmod <- glmmTMB(total_eggs ~ mean_popdens + mean_income +lagged_avgtemp_1week + percent_developed_250m +
                         calendar_month +
                         (1|Site_ID), data=knox22, family=nbinom2)
summary(egg_fullmod)

egg_drop1 <-  glmmTMB(total_eggs ~ mean_income +lagged_avgtemp_1week + percent_developed_250m +
                        calendar_month +
                        (1|Site_ID), data=knox22, family=nbinom2)
summary(egg_drop1)

egg_drop2 <- glmmTMB(total_eggs ~ mean_income +lagged_avgtemp_1week + 
                       calendar_month +
                       (1|Site_ID), data=knox22, family=nbinom2)
summary(egg_drop2)

egg_drop3 <- glmmTMB(total_eggs ~ lagged_avgtemp_1week + 
                       calendar_month +
                       (1|Site_ID), data=knox22, family=nbinom2)
summary(egg_drop3) # final model, laggedtemp_1week and calendar month are significant  

sjPlot::tab_model(egg_drop4)


#### albopictus ----

albo_tris <- glmmTMB(total_albo ~ total_tris,
                        data=knox22, family='nbinom2')
summary(albo_tris) # sig

albo_northsouth <- glmmTMB(total_albo ~ north_south,
                     data=knox22, family='nbinom2')
summary(albo_northsouth) # sig

albo_popdens <- glmmTMB(total_albo ~ mean_popdens,
                        data=knox22, family='nbinom2')
summary(albo_popdens) # sig

albo_income <- glmmTMB(total_albo ~ mean_income,
                        data=knox22, family='nbinom2')
summary(albo_income) # sig

albo_yearstructure <- glmmTMB(total_albo ~ mean_yearstructure,
                       data=knox22, family='nbinom2')
summary(albo_yearstructure) # sig 

albo_percentdeveloped <- glmmTMB(total_albo ~ percent_developed_250m,
                       data=knox22, family='nbinom2')
summary(albo_percentdeveloped) # sig

albo_avg_temp<- glmmTMB(total_albo ~ mean_avg_temp,
                       data=knox22, family='nbinom2')
summary(albo_avg_temp) # not sig

albo_avg_temp_lag1 <- glmmTMB(total_albo ~ lagged_avgtemp_1week,
                        data=knox22, family='nbinom2')
summary(albo_avg_temp_lag1) # not sig

albo_avg_temp_lag2 <- glmmTMB(total_albo ~ lagged_avgtemp_2week,
                              data=knox22, family='nbinom2')
summary(albo_avg_temp_lag2) # not sig

albo_avg_temp_lag3 <- glmmTMB(total_albo ~ lagged_avgtemp_3week,
                              data=knox22, family='nbinom2')
summary(albo_avg_temp_lag3) # not sig

albo_precip_lag1 <- glmmTMB(total_albo ~ lagged_precip_1week,
                              data=knox22, family='nbinom2')
summary(albo_precip_lag1) # not sig

albo_precip_lag2 <- glmmTMB(total_albo ~ lagged_precip_2week,
                              data=knox22, family='nbinom2')
summary(albo_precip_lag2) # sig

albo_precip_lag3 <- glmmTMB(total_albo ~ lagged_precip_3week,
                              data=knox22, family='nbinom2')
summary(albo_precip_lag3) # sig

# popdens, northsouth, total tris, precip_lag3, year structure, ndvi significant
#### albopictus ----

albo_fullmod <- glmmTMB(total_albo ~ mean_popdens + lagged_precip_2week + mean_yearstructure + lagged_avgtemp_3week +
                           percent_developed_250m + calendar_month +
                          (1|Site_ID),
                        data=knox22, family='nbinom2')
summary(albo_fullmod)

buildmer::buildglmmTMB(total_albo ~ mean_popdens + total_tris + lagged_precip_2week + mean_yearstructure + lagged_avgtemp_3week +
                         percent_developed_250m + calendar_month +
                         (1|Site_ID),
                       data=knox22, family='nbinom2',
                       buildmerControl = buildmerControl(direction='order',
                                                         crit='LRT',
                                                         elim='LRT',
                                                         ))


albo_drop1 <- glmmTMB(total_albo ~ lagged_precip_2week + mean_yearstructure + lagged_avgtemp_3week +
                        percent_developed_250m + calendar_month +
                        (1|Site_ID),
                      data=knox22, family='nbinom2')
summary(albo_drop1)


albo_drop2 <- glmmTMB(total_albo ~ mean_yearstructure + lagged_avgtemp_3week +
                        percent_developed_250m + calendar_month +
                        (1|Site_ID),
                      data=knox22, family='nbinom2')
summary(albo_drop2)

albo_drop3 <- glmmTMB(total_albo ~ mean_yearstructure +
                        percent_developed_250m + calendar_month +
                        (1|Site_ID),
                      data=knox22, family='nbinom2')
summary(albo_drop3) 

albo_drop4 <- glmmTMB(total_albo ~  total_tris +
                        percent_developed_250m + calendar_month +
                        (1|Site_ID),
                      data=knox22, family='nbinom2')
summary(albo_drop4) # final, percent developed and yearstructure are significant 


#### triseriatus 

tris_albo <- glmmTMB(total_tris ~ total_albo,
                        data=knox22, family='nbinom2')
summary(tris_albo) # sig

tris_popdens <- glmmTMB(total_tris ~ mean_popdens,
                        data=knox22, family='nbinom2')
summary(tris_popdens) # sig

tris_income <- glmmTMB(total_tris ~ mean_income,
                       data=knox22, family='nbinom2')
summary(tris_income) # sig

tris_yearstructure <- glmmTMB(total_tris ~ mean_yearstructure,
                              data=knox22, family='nbinom2')
summary(tris_yearstructure) # sig 

tris_ndvi <- glmmTMB(total_tris ~ ndvi,
                     data=knox22, family='nbinom2')
summary(tris_ndvi) # sig

tris_avg_temp<- glmmTMB(total_tris ~ mean_avg_temp,
                        data=knox22, family='nbinom2')
summary(tris_avg_temp) # sig

tris_avg_temp_lag1 <- glmmTMB(total_tris ~ lagged_avgtemp_1week,
                              data=knox22, family='nbinom2')
summary(tris_avg_temp_lag1) # sig

tris_avg_temp_lag2 <- glmmTMB(total_tris ~ lagged_avgtemp_2week,
                              data=knox22, family='nbinom2')
summary(tris_avg_temp_lag2) # not sig

tris_avg_temp_lag3 <- glmmTMB(total_tris ~ lagged_avgtemp_3week,
                              data=knox22, family='nbinom2')
summary(tris_avg_temp_lag3) # not sig

tris_precip_lag1 <- glmmTMB(total_tris ~ lagged_precip_1week,
                            data=knox22, family='nbinom2')
summary(tris_precip_lag1) # not sig

tris_precip_lag2 <- glmmTMB(total_tris ~ lagged_precip_2week,
                            data=knox22, family='nbinom2')
summary(tris_precip_lag2) # not sig

tris_precip_lag3 <- glmmTMB(total_tris ~ lagged_precip_3week,
                            data=knox22, family='nbinom2')
summary(tris_precip_lag3) # not sig

### big model

tris_fullmod <- glmmTMB(total_tris ~ mean_popdens + mean_income + mean_yearstructure +
                          mean_avg_temp + calendar_month + percent_developed_250m +
                          (1|Site_ID),
                        data=knox22, family='nbinom1')

summary(tris_fullmod)
buildmer::buildglmmTMB(total_tris ~ mean_popdens + mean_income + mean_yearstructure +
                                 mean_avg_temp + lagged_precip_1week + calendar_month + prop_developed +
                                 (1|Site_ID),
                               data=knox22, family='nbinom2',
                       buildmerControl = buildmerControl(direction='order',
                                                         crit='LRT',
                                                         elim='LRT',
                       ))



tris_drop1<- glmmTMB(total_tris ~ mean_popdens +  mean_yearstructure +
                       mean_avg_temp + calendar_month + percent_developed_250m +
                       (1|Site_ID),
                     data=knox22, family='nbinom1')
summary(tris_drop1)

tris_drop2<- glmmTMB(total_tris ~ mean_popdens +  mean_yearstructure +
                       calendar_month + percent_developed_250m +
                       (1|Site_ID),
                     data=knox22, family='nbinom2')
summary(tris_drop2)

tris_drop3 <- glmmTMB(total_tris ~  mean_yearstructure +
                        calendar_month + percent_developed_250m +
                        (1|Site_ID),
                      data=knox22, family='nbinom2')

summary(tris_drop3) 

tris_drop4 <- glmmTMB(total_tris ~ 
                        calendar_month + percent_developed_250m +
                        (1|Site_ID),
                      data=knox22, family='nbinom2')

summary(tris_drop4)

#### prop_dev_250m, and calendar month are significant 

anova(tris_drop6, tris_drop6_red)

summary(tris_drop6) # final, month and prop_developed are significant

sjPlot::tab_model(tris_drop6)

#### percent albo ----

percent_albo_northsouth <- glmmTMB(total_albo ~ north_south + offset(log(1+total_aedes)),
                           data=knox22, family='nbinom2')
summary(percent_albo_northsouth) # sig

albo_popdens <- glmmTMB(total_albo ~ mean_popdens + offset(log(1+total_aedes)),
                        data=knox22, family='nbinom2')
summary(albo_popdens) # sig

albo_income <- glmmTMB(total_albo ~ mean_income + offset(log(1+total_aedes)),
                       data=knox22, family='nbinom2')
summary(albo_income) # not sig

albo_yearstructure <- glmmTMB(total_albo ~ mean_yearstructure + offset(log(1+total_aedes)),
                              data=knox22, family='nbinom2')
summary(albo_yearstructure) # sig 

albo_ndvi <- glmmTMB(total_albo ~ ndvi +  offset(log(1+total_aedes)),
                     data=knox22, family='nbinom2')
summary(albo_ndvi) #  sig

albo_avg_temp<- glmmTMB(total_albo ~ mean_avg_temp + offset(log(1+total_aedes)),
                        data=knox22, family='nbinom2')
summary(albo_avg_temp) # not sig

albo_avg_temp_lag1 <- glmmTMB(total_albo ~ lagged_avgtemp_1week + offset(log(1+total_aedes)),
                              data=knox22, family='nbinom2')
summary(albo_avg_temp_lag1) # not sig

albo_avg_temp_lag2 <- glmmTMB(total_albo ~ lagged_avgtemp_2week + offset(log(1+total_aedes)),
                              data=knox22, family='nbinom2')
summary(albo_avg_temp_lag2) # not sig

albo_avg_temp_lag3 <- glmmTMB(total_albo ~ lagged_avgtemp_3week + offset(log(1+total_aedes)),
                              data=knox22, family='nbinom2')
summary(albo_avg_temp_lag3) # not sig

albo_precip_lag1 <- glmmTMB(total_albo ~ lagged_precip_1week + offset(log(1+total_aedes)),
                            data=knox22, family='nbinom2')
summary(albo_precip_lag1) # not sig

albo_precip_lag2 <- glmmTMB(total_albo ~ lagged_precip_2week + offset(log(1+total_aedes)),
                            data=knox22, family='nbinom2')
summary(albo_precip_lag2) # not sig

albo_precip_lag3 <- glmmTMB(total_albo ~ lagged_precip_3week + offset(log(1+total_aedes)),
                            data=knox22, family='nbinom2')
summary(albo_precip_lag3) # sig


#### percent albo big model ----

percent_albo_full <- glmmTMB(total_albo ~ mean_popdens + mean_yearstructure + calendar_month + 
                               lagged_precip_3week + percent_developed_250m + 
                               total_eggs +offset(log(1+total_aedes)) +
                               (1|Site_ID),
        data=knox22, family='nbinom2')
summary(percent_albo_full)

percent_albo_drop1 <- glmmTMB(total_albo ~ mean_popdens + mean_yearstructure + calendar_month + 
                                 percent_developed_250m + 
                                total_eggs +offset(log(1+total_aedes)) +
                                (1|Site_ID),
                              data=knox22, family='nbinom2')

summary(percent_albo_drop1)

percent_albo_drop2 <- glmmTMB(total_albo ~ mean_yearstructure + calendar_month + 
                                percent_developed_250m + 
                                total_eggs +offset(log(1+total_aedes)) +
                                (1|Site_ID),
                              data=knox22, family='nbinom2')
summary(percent_albo_drop2)

percent_albo_drop3 <- glmmTMB(total_albo ~ calendar_month + 
                                percent_developed_250m + 
                                total_eggs +offset(log(1+total_aedes)) +
                                (1|Site_ID),
                              data=knox22, family='nbinom2')
summary(percent_albo_drop3) # final, percent developed, and total eggs are significant



