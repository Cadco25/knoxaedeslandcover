#### load packages ----
library('tidyverse')
library('sf')
library('raster')

# albers conic projection 
albersproj4string <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" 


#### load data ----
sites_22 <- readxl::read_excel('data/2022 Mosquito Data/2022 Knox County Sites.xlsx',
                               sheet='2022 Knox County Sites') %>% # load sites
  # convert to simple features object
  st_as_sf(coords = c('lon', 'lat')) %>% 
  # define the original coordinate system
  st_set_crs(4269) %>%
  # transform to albers equal area 
  st_transform(crs=st_crs(albersproj4string)) %>%
  mutate(`Site Code` = ifelse(`Site Code` %in% c('FS-1','HR-1','TM-1'),
                          case_when(`Site Code`=='FS-1' ~ 'FS',
                                    `Site Code`=='HR-1' ~ 'HR',
                                    `Site Code`=='TM-1' ~ 'TM'),
                          `Site Code`)) %>%
  filter(`Site Code` %in% c('UT','HG','ST','MV','WC','CP','MC','FR','WT','IC','LP','TY','AB','DH','SP','CC','HM','WW',
                            'BC','SQ','HR','TM'))
plot(st_geometry(sites_22))

site_list <- sites_22$`Site Code`

#### create site buffers ----

sites_250m <- st_buffer(sites_22, 250)
sites_1000m <- st_buffer(sites_22, 1000)

#### mosquito data ----
  
  knox22 <- read.csv('data/2022 Mosquito Data/knox22_clean.csv') %>%
  filter(Collection_Method == 'ovicup') %>%
  dplyr::select(Site_ID, Trap_Number, Calendar_Week, hatched_eggs, embryonating_eggs,
         total_eggs, albopictus_female, albopictus_male, triseriatus_female,
         triseriatus_male, japonicus_female, japonicus_male) %>%
  mutate(across(c(total_eggs, albopictus_female, albopictus_male, triseriatus_female,
                  triseriatus_male, japonicus_female, japonicus_male),as.numeric),
         total_albo = albopictus_female+albopictus_male,
         total_tris = triseriatus_female+triseriatus_male,
         total_jap = japonicus_female + japonicus_male,
         total_albo = ifelse(total_eggs == 'NA','NA',total_albo),
         total_tris = ifelse(total_eggs == 'NA','NA', total_tris),
         total_jap = ifelse(total_eggs == 'NA','NA', total_jap),
         # create column for rainfall station joining
         rainfall_station = case_when(Site_ID %in% c('AB','BC','DH','HM',
                                                   'LP','SQ','TM','WW') ~ 'Walden Drive',
                                      Site_ID %in% c('CP','HG','MV',
                                                     'ST','TY','UT','FS') ~ 'Tyson Park',
                                      Site_ID %in% c('CC','FR','HR','IC','MC',
                                                     'WT','WC') ~ 'Williams Creek',
                                      Site_ID == 'SP' ~ 'Love Creek')
                                                     ) %>%
  filter(Site_ID %in% site_list, Trap_Number=='1' & Site_ID !='FS') %>%
  janitor::clean_names()
         
write.csv(knox22,'data/2022 Mosquito Data/knox22_clean.csv')
                                    
        
# load land cover categories
LULC <- raster('data/Raster data/NLCD LULC/NLCD_2019_landcover_clippedtoKnox.tif') %>%
  raster::projectRaster(crs=albersproj4string) # reproject the raster to albers conic


#### calculate percentage land cover categories ----

### create function for calculating zonal statistics 
nlcd.zonalhistogram <- function(df, buffer){

nlcd_extracted <- raster::extract(df, buffer)

nlcd_legend <- c('11','12','21','22','23','24','31','41',
                 '42','43','51','52','71','72','73','74',
                 '81','82','90','95') 

land_cover_counts <- lapply(nlcd_extracted, function(x) table(factor(x, levels=nlcd_legend)))

percentage_values <- lapply(land_cover_counts, function(x) 100*x/sum(x)) %>%
  setNames(site_list) 

unique_nlcd_values <- unique(unlist(lapply(percentage_values, names)))

percentage_values_df <- data.frame(
  site=names(percentage_values),
  t(sapply(percentage_values, function(x) x[match(unique_nlcd_values, names(x))]))
  ) %>%
  mutate(percent_forest = X41+X42+X43,
         percent_developed_total = X21+X22+X23+X24,
         percent_open = X21,
         percent_developed_constructed = X22+X23+X24,
         percent_water = X11,
         percent_crop = X51+X52+X71+X72+X73+X74+X81+X82) %>%
  select(site, percent_forest, percent_developed_total, percent_open,
         percent_developed_constructed, percent_water, percent_crop)
}

### run function on every buffer level
LULC_250m <- nlcd.zonalhistogram(LULC,sites_250m) %>%
  {colnames(.) <- c('site','percent_forest_250m','percent_developed_total_250m',
                    'percent_open_250m','percent_built_250m',
                    'percent_water_250m', 'percent_crop_250m'); .}

LULC_1000m <- nlcd.zonalhistogram(LULC,sites_1000m)%>%
  {colnames(.) <- c('site','percent_forest_1000m','percent_developed_total_1000m',
                    'percent_open_1000m','percent_built_1000m',
                    'percent_water_1000m', 'percent_crop_1000m'); .}

#### merge the LULC data ---- 
LULC_merged <- merge(LULC_250m, LULC_1000m, by='site') 

# calculate dominant land cover at 250m

domland_250m <- LULC_merged %>%
    group_by(site) %>%
    summarize(max_forest = max(percent_forest_250m),
              max_open = max(percent_open_250m),
              max_developed = max(percent_built_250m),
              max_water = max(percent_water_250m),
              max_crop = max(percent_crop_250m))

print(domland_250m,n=34) # present land cover percentages in 250m radius

# calculate dominant land cover at 1000m

domland_1000m <- LULC_merged %>% 
    group_by(site) %>%
    summarize(max_forest = max(percent_forest_1000m),
              max_open = max(percent_open_1000m),
              max_developed = max(percent_built_1000m),
              max_water = max(percent_water_1000m),
              max_crop = max(percent_crop_1000m))

print(domland_1000m,n=34)# present land cover percentages in 1000m radius

# join with knox22 

knox22_lulc <- left_join(knox22, LULC_merged, by=c('Site_ID'='site'))
view(knox22_lulc)

#### Climate data prep ----


climate <- readxl::read_excel('data/Weather data/2022 knoxville weather data.xlsx',
                              sheet='data') 

rainfall <- readxl::read_excel('data/Weather data/2022 knoxville watershed data (NPDES).xlsx',
                               sheet='rainfall') 


# format data column as date
climate$Date <- as.Date(climate$Date)
rainfall$Date <- as.Date(rainfall$Date)

# create a calendar week column
climate$calendar_week <- strftime(climate$Date, format = "%V")
rainfall$calendar_week <- strftime(rainfall$Date, format = "%V")

# change rainfall to long format

rainfall_long <- gather(rainfall, station, rainfall_inches, 
                        `City Building`:`NOAA Airport`)

# convert rainfall to mm 

rainfall_long$rainfall_mm <- as.numeric(rainfall_long$rainfall_inches)*25.4

climate_weekly <- climate %>% 
  # group by week
  group_by(calendar_week) %>%
  # calculatew weekly mean temps or total precip 
  summarize(mean_min_temp = mean(`TMIN (Degrees Fahrenheit)`,na.rm=TRUE),
            mean_max_temp = mean(`TMAX (Degrees Fahrenheit)`,na.rm=TRUE),
            mean_avg_temp = mean(`TAVG (Degrees Fahrenheit)`,na.rm=TRUE),
            total_precip = sum(`PRCP (Inches)`,na.rm=TRUE))%>%
  # create time lags 
  mutate(across(c(calendar_week),as.numeric),
         # convert temp to celsius 
         mean_min_temp = (mean_min_temp-32)*5/9,
         mean_max_temp = (mean_max_temp-32)*5/9,
         mean_avg_temp = (mean_avg_temp-32)*5/9,
         # convert precip to centimeters 
         total_precip = total_precip*2.54,
         lagged_precip_1week = lag(total_precip,n=1,default=NA),
         lagged_precip_2week = lag(total_precip,n=2,default=NA),
         lagged_precip_3week = lag(total_precip,n=3,default=NA),
         lagged_maxtemp_1week = lag(mean_max_temp,n=1,default=NA),
         lagged_maxtemp_2week = lag(mean_max_temp,n=2,default=NA),
         lagged_maxtemp_3week = lag(mean_max_temp,n=3,default=NA),
         lagged_mintemp_1week = lag(mean_min_temp,n=1,default=NA),
         lagged_mintemp_2week = lag(mean_min_temp,n=2,default=NA),
         lagged_mintemp_3week = lag(mean_min_temp,n=3,default=NA),
         lagged_avgtemp_1week = lag(mean_avg_temp,n=1,default=NA),
         lagged_avgtemp_2week = lag(mean_avg_temp,n=2,default=NA),
         lagged_avgtemp_3week = lag(mean_avg_temp,n=3,default=NA)) 

rainfall_weekly <- rainfall_long %>% 
  # group by week and station
  group_by(calendar_week, station) %>%
  # calculatew weekly mean temps or total precip 
  summarize(total_rainfall = sum(rainfall_mm,na.rm=TRUE)) %>%
  # create time lags 
    mutate(
         lagged_rainfall_1week = lag(total_rainfall,n=1,default=NA),
         lagged_rainfall_2week = lag(total_rainfall,n=2,default=NA)+lagged_rainfall_1week,
         lagged_rainfall_3week = lag(total_rainfall,n=3,default=NA)+lagged_rainfall_2week)

# change calendar week to numeric in rainfall data
rainfall_weekly$calendar_week <- as.numeric(rainfall_weekly$calendar_week)
         
#### join climate data with mosquito data ----
knox22_joined <- left_join(knox22_lulc,climate_weekly,
                           by=c('Calendar_Week' = 'calendar_week'),
                           multiple='all') %>% # bc there are mulitple matches
  left_join(rainfall_weekly,
            by=c('Calendar_Week' = 'calendar_week','rainfall_station'='station')) %>%
  
         mutate(calendar_month = ifelse(Calendar_Week %in% c(22:26),
                                 'June',
                                 ifelse(Calendar_Week %in% c(27:30),
                                        'July',
                                        ifelse(Calendar_Week %in% c(31:34),
                                               'August',
                                               ifelse(Calendar_Week %in% c(35:38),
                                                      'September',
                                                      'October'))))
                   )



# lowercase column names
colnames(knox22_joined) <- tolower(colnames(knox22_joined))

knox22_joined <- knox22_joined %>%
  filter(site_id != 'FS') %>% # remove Fort Sanders site 
  # classify the dominant landcover 
  mutate(domland_250m = ifelse(
    site_id %in% c('CP','FR','HG','IC','MC','MV','WT','HR'),
    'forest',
    ifelse(site_id %in% c('BC','LP','SP','SQ','ST','WC','WW'),
           'open',
           'built')),
    domland_1000m = case_when(site_id %in% c('CP','FR','HG','HR','IC','MC','MV','ST','WC','WT') ~ 'forest',
                              site_id %in% c('AB','LP') ~ 'open',
                              site_id %in% c('BC','CP','DH','HM','SP','TM','TP','UT','WW') ~ 'built'
    ),
    # month = if last day of week was in that month 
    calendar_month = case_when(calendar_week %in% c('22','23','24','25') ~ 'June',
                               calendar_week %in% c('26','27','28','29','30') ~ 'July',
                               calendar_week %in% c('31','32','33','34') ~ 'August',
                               calendar_week %in% c('35','36','37','38') ~'September',
                               calendar_week %in% c('39','40','41','42') ~ 'October'),
    percent_hatch = ifelse(total_eggs>0,
                                  (total_albo+total_tris+total_jap)/total_eggs*100,
                                  100),
    total_aedes=total_tris+total_jap+total_albo
  )

knox22_joined$calendar_month <- factor(knox22_joined$calendar_month,
                                       levels=c('June','July','August',
                                                'September','October'))

knox22_joined$domland_250m <- factor(knox22_joined$domland_250m,
                                       levels=c('forest','open','developed'))

knox22_joined$domland_1000m <- factor(knox22_joined$domland_1000m,
                                     levels=c('forest','open','developed'))

sites_22 <- readxl::read_excel('data/2022 Mosquito Data/2022 Knox County Sites.xlsx',
                               sheet='2022 Knox County Sites') %>%
  rename(site_id=`Site Code`) %>%
  select(site_id, lon, lat) %>%
  mutate(site_id = ifelse(site_id %in% c('HR-1','TM-1'),
                          case_when(site_id=='HR-1' ~ 'HR',
                                    site_id=='TM-1' ~ 'TM'),
                          site_id))

knox22_joined <- knox22_joined %>% 
  left_join(sites_22, by='site_id')
write_rds(knox22_joined, 'data/clean_data/knox22.RDS')
write.csv(knox22_joined, 'data/clean_data/2022-Knox-Collections-Landcover-Joined.csv')


