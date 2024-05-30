# **************************************************************************** #
# Title: Variable Selection
# Project: Day and Trout Fryxell Land Cover Aedes Project 
# Author: Corey Day 
# Date Created: March 2024
# Questions? email coreyallenday96@gmail.com 
# **************************************************************************** #

# run renv::restore()

# load packages -----------------------------------------------------------

library(tidyverse)
library(glmmTMB)

# load data ---------------------------------------------------------------

knox22 <- readRDS('data/knox22_joined.RDS') %>%
  mutate(percent_hatch = ifelse(total_eggs>0,
                                (total_albo+total_tris+total_jap)/total_eggs*100,
                                100),
         total_aedes = total_jap+total_albo+total_tris,
         percent_built_250m_scaled = percent_built_250m/5,
         percent_forest_1000m_scaled = percent_forest_1000m/5,
         prop_tris = total_tris/total_aedes,
         prop_albo = total_albo/total_aedes)



# calculate Pearson correlations ------------------------------------------


corcols_250m <- c('percent_forest_250m','percent_open_250m', 'percent_built_250m','percent_water_250m',
                  'percent_crop_250m')

round(cor(knox22[,corcols_250m],method='pearson'),2)


corcols_1000m <- c('percent_forest_1000m','percent_open_1000m', 'percent_built_1000m','percent_water_1000m',
                   'percent_crop_1000m')

round(cor(knox22[,corcols_1000m],method='pearson'),2)



# land cover AIC ----------------------------------------------------------


## albopictus ----

albo_forest_250m <- glmmTMB(total_albo ~ percent_forest_250m + percent_open_250m + (1|site_id),
                             data=knox22, family='nbinom1')

AIC(albo_forest_250m) # 3434


albo_built_250m <- glmmTMB(total_albo ~ percent_built_250m + percent_open_250m + (1|site_id),
                           data=knox22, family='nbinom1')

AIC(albo_built_250m) # AIC = 3439


albo_forest_1000m <- glmmTMB(total_albo ~ percent_forest_1000m + percent_open_1000m + (1|site_id),
                            data=knox22, family='nbinom1')

AIC(albo_forest_1000m) # AIC = 3436

albo_built_1000m <- glmmTMB(total_albo ~ percent_built_1000m + percent_open_1000m + (1|site_id),
                             data=knox22, family='nbinom1')

AIC(albo_built_1000m) # AIC = 3437

## triseriatus ----

tris_forest_250m <- glmmTMB(total_tris ~ percent_forest_250m + percent_open_250m +  (1|site_id),
                            data=knox22, family='nbinom1')

AIC(tris_forest_250m) # AIC = 2751

tris_built_250m <- glmmTMB(total_tris ~ percent_built_250m + percent_open_250m +  (1|site_id),
                           data=knox22, family='nbinom1')

AIC(tris_built_250m) # AIC = 2752

tris_forest_1000m <- glmmTMB(total_tris ~ percent_forest_1000m +  percent_open_1000m +  (1|site_id),
                            data=knox22, family='nbinom1')

AIC(tris_forest_1000m) # AIC = 2744 (best fit)


tris_built_1000m <- glmmTMB(total_tris ~ percent_built_1000m +  percent_open_1000m +  (1|site_id),
                             data=knox22, family='nbinom1')

AIC(tris_built_1000m) # AIC = 2750


## japonicus ----

jap_forest_250m <- glmmTMB(ifelse(total_jap>0,1,0) ~ percent_forest_250m + percent_open_250m + (1|site_id),
                            data=knox22, family='binomial')

AIC(jap_forest_250m) # AIC = 218


jap_built_250m <- glmmTMB(ifelse(total_jap>0,1,0) ~ percent_built_250m + percent_open_250m + (1|site_id),
                           data=knox22, family='binomial')

AIC(jap_built_250m) # AIC = 220.2

                    
jap_forest_1000m <- glmmTMB(ifelse(total_jap>0,1,0) ~ percent_forest_1000m + percent_open_1000m + (1|site_id),
                           data=knox22, family='binomial')

AIC(jap_forest_1000m) # AIC = 211 (best fit)


jap_built_1000m <- glmmTMB(ifelse(total_jap>0,1,0) ~ percent_built_1000m + percent_open_1000m + (1|site_id),
                          data=knox22, family='binomial')

AIC(jap_built_1000m) # AIC = 217



# temperature -------------------------------------------------------------

## albopictus ----

albo_temp_0week <- glmmTMB(total_albo ~ mean_avg_temp + (1|site_id),
                           data=knox22, family='nbinom1')
AIC(albo_temp_0week) # 3429

albo_temp_1week <- glmmTMB(total_albo ~ lagged_avgtemp_1week + (1|site_id),
                            data=knox22, family='nbinom1')
AIC(albo_temp_1week) # 3431

albo_temp_2week <- glmmTMB(total_albo ~ lagged_avgtemp_2week + (1|site_id),
                           data=knox22, family='nbinom1')
AIC(albo_temp_2week) # 3410 (best fit)

albo_temp_3week <- glmmTMB(total_albo ~ lagged_avgtemp_3week +  (1|site_id),
                           data=knox22, family='nbinom1')
AIC(albo_temp_3week) # 3422 

##  triseriatus ----

tris_temp_0week <- glmmTMB(total_tris ~ mean_avg_temp + (1|site_id),
                           data=knox22, family='nbinom1')
AIC(tris_temp_0week) # 2742 (best fit)

tris_temp_1week <- glmmTMB(total_tris ~ lagged_avgtemp_1week + (1|site_id),
                           data=knox22, family='nbinom1')
AIC(tris_temp_1week) # 2752

tris_temp_2week <- glmmTMB(total_tris ~ lagged_avgtemp_2week + (1|site_id),
                           data=knox22, family='nbinom1')
AIC(tris_temp_2week) # 2762

tris_temp_3week <- glmmTMB(total_tris ~ lagged_avgtemp_3week +  (1|site_id),
                           data=knox22, family='nbinom1')
AIC(tris_temp_3week) # 2755

## japonicus ----

jap_temp_0week <- glmmTMB(ifelse(total_jap>0,1,0) ~ mean_avg_temp + (1|site_id),
                          data=knox22, family='binomial')
AIC(jap_temp_0week) # 224

jap_temp_1week <- glmmTMB(ifelse(total_jap>0,1,0) ~ lagged_avgtemp_1week + (1|site_id),
                           data=knox22, family='binomial')
AIC(jap_temp_1week) # 224

jap_temp_2week <- glmmTMB(ifelse(total_jap>0,1,0) ~ lagged_avgtemp_2week + (1|site_id),
                           data=knox22, family='binomial')
AIC(jap_temp_2week) # 222 (best fit)

jap_temp_3week <- glmmTMB(ifelse(total_jap>0,1,0) ~ lagged_avgtemp_3week + (1|site_id),
                           data=knox22, family='binomial')
AIC(jap_temp_3week) # 224 



# precipitation -----------------------------------------------------------

## albopictus ----

albo_rainfall_1week <- glmmTMB(total_albo ~ lagged_rainfall_1week + (1|site_id),
                           data=knox22, family='nbinom1')
AIC(albo_rainfall_1week) # 3436

albo_rainfall_2week <- glmmTMB(total_albo ~ lagged_rainfall_2week + (1|site_id),
                           data=knox22, family='nbinom1')
AIC(albo_rainfall_2week) # 3433 (very slightly best fit) 

albo_rainfall_3week <- glmmTMB(total_albo ~ lagged_rainfall_3week + (1|site_id),
                           data=knox22, family='nbinom1')
AIC(albo_rainfall_3week) # 3434 

## triseriatus ----

tris_rainfall_1week <- glmmTMB(total_tris ~ lagged_rainfall_1week + (1|site_id),
                             data=knox22, family='nbinom1')
AIC(tris_rainfall_1week) # 2763 

tris_rainfall_2week <- glmmTMB(total_tris ~ lagged_rainfall_2week + (1|site_id),
                             data=knox22, family='nbinom1')
AIC(tris_rainfall_2week) # 2763

tris_rainfall_3week <- glmmTMB(total_tris ~ lagged_rainfall_3week + (1|site_id),
                             data=knox22, family='nbinom1')
AIC(tris_rainfall_3week) # 2761 (best fit)


## japonicus ----

jap_rainfall_1week <- glmmTMB(ifelse(total_jap>0,1,0) ~ lagged_rainfall_1week + (1|site_id),
                          data=knox22, family='binomial')
AIC(jap_rainfall_1week) # 223

jap_rainfall_2week <- glmmTMB(ifelse(total_jap>0,1,0) ~ lagged_rainfall_2week + (1|site_id),
                          data=knox22, family='binomial')
AIC(jap_rainfall_2week) # 222

jap_rainfall_3week <- glmmTMB(ifelse(total_jap>0,1,0) ~ lagged_rainfall_3week + (1|site_id),
                          data=knox22, family='binomial')
AIC(jap_rainfall_3week) # 221

