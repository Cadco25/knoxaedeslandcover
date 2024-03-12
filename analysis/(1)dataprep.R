# **************************************************************************** #
# Title: Data Preparation 
# Project: Day and Trout Fryxell Land Cover Aedes Project 
# Author: Corey Day 
# Date Created: March 2024
# Questions? email coreyallenday96@gmail.com 
# **************************************************************************** #

# load packages -----------------------------------------------------------

library('tidyverse') # for data manipulation
library('sf') # for working with spatial (vector) data
library('raster') # for working with raster data (e.g., land cover grids)


# define a projection ----------------------------------------------------


albersproj4string <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" 


# load data ---------------------------------------------------------------

## site coordinates ---- 

sites_22 <- read.csv('data/sites22.csv') %>% # load sites
  st_as_sf(coords = c('lon', 'lat')) %>%  # convert to simple features object
  st_set_crs(4269) %>% # define the original coordinate system
  st_transform(crs=st_crs(albersproj4string))  # transform to albers equal area 

plot(st_geometry(sites_22)) # plot the coordinates to make sure everything worked


## mosquito collection data ---- 
  
aedes22 <- read.csv('data/aedes22_clean.csv')
         

## land cover data ----      


LULC <- raster('data/NLCD_2019_landcover_clippedtoKnox.tif') %>%
  raster::projectRaster(crs=albersproj4string) # reproject the raster to albers conic

## temperature data ----

temperature <- read.csv('data/knox22_temperature.csv') %>%
  janitor::clean_names() # clean the column names for R 

## rainfall data ----

rainfall <- read.csv('data/knox22_rainfall.csv') %>% 
  janitor::clean_names() # clean the column names for R 


# calculate land cover percentages in 250m and 1000m radii -----------------------------------------


## create site buffers ----

sites_250m <- st_buffer(sites_22, 250) # create 250m buffer around each site 
sites_1000m <- st_buffer(sites_22, 1000) # create 1000m buffer around each site 

site_list <- sites_22$`site_code` # create a vector of site codes 

## create function for calculating zonal statistics ----

nlcd.zonalhistogram <- function(df, buffer){ # define function 

nlcd_extracted <- raster::extract(df, buffer) # extract the circular areas from the larger land cover dataset with extract()

nlcd_legend <- c('11','12','21','22','23','24','31','41',
                 '42','43','51','52','71','72','73','74',
                 '81','82','90','95') # define the numerical categories in the NLCD land cover data 

land_cover_counts <- lapply(nlcd_extracted, function(x) table(factor(x, levels=nlcd_legend))) # create a table of land cover within each site buffer

percentage_values <- lapply(land_cover_counts, function(x) 100*x/sum(x)) %>% # calculate the percentage of each land cover category
  setNames(site_list) # name each site based on the site names vector we specified outside of the function

unique_nlcd_values <- unique(unlist(lapply(percentage_values, names))) # make sure we only have one unique value for each land cover cat and site pair

percentage_values_df <- data.frame( # create a final clean data frame with the percentage land cover categories per site 
  site=names(percentage_values),
  t(sapply(percentage_values, function(x) x[match(unique_nlcd_values, names(x))]))
  ) %>%
  mutate(percent_forest = X41+X42+X43, # aggregate "forest" categories
         percent_open = X21, # aggregate "open" categories
         percent_built = X22+X23+X24, # aggregate "built" categories
         percent_water = X11, # rename "water" 
         percent_crop = X51+X52+X71+X72+X73+X74+X81+X82) %>% # aggregate crop/shrubs/etc. 
  dplyr::select(site, percent_forest,  percent_open, percent_built, percent_water, percent_crop) # reduce to only the key variables
}

## run function on every buffer level ----
LULC_250m <- nlcd.zonalhistogram(LULC,sites_250m) %>% # apply the function to the land cover data and 250m buffers 
  {colnames(.) <- c('site','percent_forest_250m',
                    'percent_open_250m','percent_built_250m',
                    'percent_water_250m', 'percent_crop_250m'); .} # specifies the column names for the output dataset 

LULC_1000m <- nlcd.zonalhistogram(LULC,sites_1000m) %>% # apply the function to the land cover data and 1000m buffers 
  {colnames(.) <- c('site','percent_forest_1000m',
                    'percent_open_1000m','percent_built_1000m',
                    'percent_water_1000m', 'percent_crop_1000m'); .} # specifies the column names for the output dataset 


# merge the 250m and 1000m data -------------------------------------------

LULC_merged <- merge(LULC_250m, LULC_1000m, by='site') 

# calculate dominant land cover at 250m and 1000m  ------------------------


## calculate dominant land cover at 250m ----

domland_250m <- LULC_merged %>%
    group_by(site) %>%
    summarize(max_forest = max(percent_forest_250m),
              max_open = max(percent_open_250m),
              max_developed = max(percent_built_250m),
              max_water = max(percent_water_250m),
              max_crop = max(percent_crop_250m))

print(domland_250m,n=34) # present land cover percentages in 250m radius

## calculate dominant land cover at 1000m ----

domland_1000m <- LULC_merged %>% 
    group_by(site) %>%
    summarize(max_forest = max(percent_forest_1000m),
              max_open = max(percent_open_1000m),
              max_developed = max(percent_built_1000m),
              max_water = max(percent_water_1000m),
              max_crop = max(percent_crop_1000m))

print(domland_1000m,n=34)# present land cover percentages in 1000m radius


# join land cover and mosquito data ---------------------------------------

knox22_lulc <- left_join(aedes22, LULC_merged, by=c('site_id'='site'))

# climate data prep -------------------------------------------------------

# format data columns as dates
temperature$Date <- as.Date(temperature$date,format=c("%m/%d/%y"))
rainfall$Date <- as.Date(rainfall$date,format=c("%m/%d/%y"))


# create a calendar week column
temperature$calendar_week <- strftime(temperature$Date, format = "%V")
rainfall$calendar_week <- strftime(rainfall$Date, format = "%V")

# change rainfall to long format

rainfall_long <- gather(rainfall, station, rainfall_inches, 
                        `city_building`:`noaa_airport`)

# convert rainfall to mm 

rainfall_long$rainfall_mm <- as.numeric(as.character(rainfall_long$rainfall_inches))*25.4

## summarize the temperature data by week and time lags ----

temperature_weekly <- temperature %>% 
  group_by(calendar_week) %>%  # group by week
  # calculate weekly mean temps or total precip 
  summarize(mean_avg_temp = mean(`tavg_degrees_fahrenheit`,na.rm=TRUE)) %>% 
  
  # create time lags 
  mutate(across(c(calendar_week),as.numeric),
        
          # convert avg weekly temps to Celsius 
         mean_avg_temp = (mean_avg_temp-32)*5/9,
         
        # specify time lags (e.g., 1 week prior to collecton, 2 weeks prior, etc.)
         lagged_avgtemp_1week = lag(mean_avg_temp,n=1,default=NA), # mean temp 1 week prior
         lagged_avgtemp_2week = lag(mean_avg_temp,n=2,default=NA), # mean temp 2 weeks prior
         lagged_avgtemp_3week = lag(mean_avg_temp,n=3,default=NA))  # mean temp 3 weeks prior 


rainfall_weekly <- rainfall_long %>% 
  # group by week and station
  group_by(calendar_week, station) %>%
  # calculatew weekly mean temps or total precip 
  summarize(total_rainfall = sum(rainfall_mm,na.rm=TRUE)) %>% 
  group_by(station) %>%
  arrange(calendar_week) %>%
  mutate(lagged_rainfall_1week = lag(total_rainfall, default = NA), # cumulative precip 1 week prior 
         lagged_rainfall_2week = lag(total_rainfall,n=2,default=NA)+lagged_rainfall_1week, # cumulative precip 2 weeks prior 
         lagged_rainfall_3week = lag(total_rainfall,n=3,default=NA)+lagged_rainfall_2week) # cumulative precip 3 weeks prior 



# change calendar week to numeric in rainfall data
rainfall_weekly$calendar_week <- as.numeric(rainfall_weekly$calendar_week)
         

# join temperature and mosquito data  -----------------------------------------


knox22_joined <- left_join(knox22_lulc,temperature_weekly,
                           by=c('calendar_week' = 'calendar_week'),
                           multiple='all') %>% # bc there are mulitple matches
  left_join(rainfall_weekly,
            by=c('calendar_week' = 'calendar_week','rainfall_station'='station')) %>%
  
         mutate(calendar_month = ifelse(calendar_week %in% c(22:26),
                                 'June',
                                 ifelse(calendar_week %in% c(27:30),
                                        'July',
                                        ifelse(calendar_week %in% c(31:34),
                                               'August',
                                               ifelse(calendar_week %in% c(35:38),
                                                      'September',
                                                      'October'))))
                   )

# lowercase column names
colnames(knox22_joined) <- tolower(colnames(knox22_joined))



# final data cleaning steps -----------------------------------------------

knox22_joined <- knox22_joined %>%
  
  # create column with dominant landcover in 250m radius (calculated earlier, specified in dataset here)
  mutate(domland_250m = ifelse(
    site_id %in% c('CP','FR','HG','IC','MC','MV','WT','HR'),
    'forest',
    ifelse(site_id %in% c('BC','LP','SP','SQ','ST','WC','WW'),
           'open',
           'built')),
    
    # create column with dominant landcover in 1000m radius (calculated earlier, specified in dataset here)
    domland_1000m = case_when(site_id %in% c('CP','FR','HG','HR','IC','MC','MV','ST','WC','WT', 'SQ') ~ 'forest',
                              site_id %in% c('AB','LP') ~ 'open',
                              site_id %in% c('BC','CC', 'CP','DH','HM','SP','TM','TY','UT','WW') ~ 'built'
    ),
    
    # create calendar month column 
    calendar_month = case_when(calendar_week %in% c('22','23','24','25') ~ 'June',
                               calendar_week %in% c('26','27','28','29','30') ~ 'July',
                               calendar_week %in% c('31','32','33','34') ~ 'August',
                               calendar_week %in% c('35','36','37','38') ~'September',
                               calendar_week %in% c('39','40','41','42') ~ 'October'),
    
    # calculate column for percent of eggs that were successfully reared to adulthood
    percent_hatch = ifelse(total_eggs>0,
                                  (total_albo+total_tris+total_jap)/total_eggs*100,
                                  100),
    
    # calculate column that shows the total number of mosquitoes reared to adulthood among all three species 
    total_aedes=total_tris+total_jap+total_albo
  )

## convert columns to factor and specify order of levels ----

knox22_joined$calendar_month <- factor(knox22_joined$calendar_month,
                                       levels=c('June','July','August',
                                                'September','October'))

knox22_joined$domland_250m <- factor(knox22_joined$domland_250m,
                                       levels=c('forest','open','built'))

knox22_joined$domland_1000m <- factor(knox22_joined$domland_1000m,
                                     levels=c('forest','open','built'))




# join the climate and mosquito data with sites ---------------------------

knox22_joined <- knox22_joined %>% 
  left_join(sites_22, by=c('site_id' = 'site_code'))


# save the final dataset  -------------------------------------------------

write_rds(knox22_joined, 'data//knox22_joined.RDS') # saves an R dataset that maintains factors and numeric specifications


write.csv(knox22_joined, 'data/2022-Knox-Collections-Landcover-Joined.csv') # save as csv too 


