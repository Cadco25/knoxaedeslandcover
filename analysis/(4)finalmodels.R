# **************************************************************************** #
# Title: Final Models and Diagnostics 
# Project: Day and Trout Fryxell Land Cover Aedes Project 
# Author: Corey Day 
# Date Created: March 2024
# Questions? email coreyallenday96@gmail.com 
# **************************************************************************** #

# run renv::restore() 

# load packages -----------------------------------------------------------


library(tidyverse) # to manipulate data 
library(glmmTMB) # to fit models 
library(sjPlot) # for nice model output 


# load data ---------------------------------------------------------------

knox22 <- readRDS('data/knox22_joined.RDS') %>%
  mutate(percent_hatch = ifelse(total_eggs>0,
                                (total_albo+total_tris+total_jap)/total_eggs*100,
                                100),
         total_aedes = total_jap+total_albo+total_tris,
         percent_built_250m_scaled = percent_built_250m/5,
         percent_forest_250m_scaled = percent_forest_250m/5,
         percent_forest_1000m_scaled = percent_forest_1000m/5,
         percent_open_250m_scaled = percent_open_250m/5,
         percent_open_1000m_scaled = percent_open_1000m/5,
         prop_tris = total_tris/total_aedes)
knox22$pos <- numFactor(scale(knox22$lon), scale(knox22$lat))
knox22$ID <- factor(rep(1,nrow(knox22)))


# get unique lat/long for each site ---------------------------------------

knox22_coords <- knox22 %>%
  dplyr::distinct(site_id, lon, lat) 
  


# fit albopictus full model -----------------------------------------------

##  fit full model ----
albo_fullmod <- glmmTMB(total_albo ~ percent_forest_250m_scaled + percent_open_250m_scaled +
                                     lagged_avgtemp_2week +
                                     lagged_rainfall_2week +
                                     calendar_month + 
                                    (1|site_id), 
                        data=knox22, family='nbinom1')

# get tabulated model summary with expontentiated coefficients 
tab_model(albo_fullmod)


### fit model without calendar month ----
albo_fullmod_red <- glmmTMB(total_albo ~ percent_forest_250m_scaled + percent_open_250m_scaled +
                              lagged_avgtemp_2week +
                              lagged_rainfall_2week +
                              (1|site_id), 
                            data=knox22, family='nbinom1')

### LRT for calendar month ----

anova(albo_fullmod, albo_fullmod_red) # likelihood ratio test for calendar month

## residual diagnostics ----

plot(DHARMa::simulateResiduals(albo_fullmod)) # diagnostic plots 
 
albo_fullmod_simoutput <-  DHARMa::recalculateResiduals(
                              DHARMa::simulateResiduals(albo_fullmod), 
                              group = knox22$site_id) # recalculate residuals with aggregation by site

plot(albo_fullmod_simoutput) # diagnostic plots of residuals with observations aggregated by site

DHARMa::testSpatialAutocorrelation(albo_fullmod_simoutput, # simmed residuals with observations aggregated by site
                                   knox22_coords$lon, # specify site longitude
                                   knox22_coords$lat) # specify site latitude 

# p = 0.23, indicates no problem of spatial autocorrelation 


# triseriatus full model --------------------------------------------------

## fit full model ----

tris_fullmod <- glmmTMB(total_tris ~ percent_forest_1000m_scaled + percent_open_1000m_scaled+
                          mean_avg_temp + 
                          lagged_rainfall_3week+
                          calendar_month + 
                          (1|site_id),
                        data=knox22, family='nbinom1')


tris_fullmod <- glmmTMB(total_tris ~ 
                          calendar_month + 
                          (1|site_id),
                        data=knox22, family='nbinom1')

# tabulated model output with exponentiated coefficients 
tab_model(tris_fullmod)

### fit model without calendar month for LRT ----

tris_fullmod_red <- glmmTMB(total_tris ~ percent_forest_1000m_scaled + percent_open_1000m_scaled +
                              mean_avg_temp + 
                              lagged_rainfall_3week + 
                              (1|site_id),
                            data=knox22, family='nbinom1')

### LRT to test calendar month ----

anova(tris_fullmod, tris_fullmod_red) # likelihood ratio test for calendar month

## residual diagnostics ----

plot(DHARMa::simulateResiduals(tris_fullmod)) # diagnostic plots 


tris_fullmod_simoutput <-  DHARMa::recalculateResiduals(
  DHARMa::simulateResiduals(tris_fullmod), group = knox22$site_id) # aggregate by site and resample residuals


DHARMa::testSpatialAutocorrelation(tris_fullmod_simoutput,# simmed residuals with observations aggregated by site
                                   knox22_coords$lon, # specify site longitude
                                   knox22_coords$lat) # specify site latitude 

# p = 0.91, no evidence of spatial autocorrelation



# japonicus full model ----------------------------------------------------

## fit full model ----

jap_fullmod <- glmmTMB(ifelse(total_jap>0,1,0) ~ percent_forest_1000m_scaled + percent_open_1000m_scaled + 
                         calendar_month + 
                         lagged_avgtemp_2week +
                         lagged_rainfall_3week +
                         (1|site_id),
                      data=knox22, family='binomial')

# tabulated model output with exponentiated coefficients
tab_model(jap_fullmod) 

### fit model without calendar month for LRT ----

jap_fullmod_red <- glmmTMB(ifelse(total_jap>0,1,0) ~ percent_forest_1000m_scaled + percent_open_1000m_scaled + 
                             lagged_avgtemp_2week + 
                             lagged_rainfall_3week +
                             (1|site_id),
                           data=knox22, family='binomial')

### LRT to test calendar month ----

anova(jap_fullmod, jap_fullmod_red) # likelihood ratio test to analyze calendar month 


## residual diagnostics ----

plot(DHARMa::simulateResiduals(jap_fullmod)) # diagnostic plots 

jap_fullmod_simoutput <-  DHARMa::recalculateResiduals(
  DHARMa::simulateResiduals(jap_fullmod), group = knox22$site_id) # recalculate residuals aggregated by site


DHARMa::testSpatialAutocorrelation(jap_fullmod_simoutput,
                                   knox22_coords$lon, knox22_coords$lat)
