#### Load Packages ----
library('tidyverse')
library('sf')
library('raster')

#### Load projections ----

# * 1. USA Contiguous Albers Equal Area Conic Proj4string ----
albersproj4string <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" 


#### Load Data ----

# * 1. load Knox county boundary ----
knox_geom <- tigris::counties(state=c('47'),year='2020') %>%
  # filter to Knox county
  filter(NAME=='Knox') %>%
  # transform to albers equal area 
  st_transform(crs=st_crs(albersproj4string))

plot(st_geometry(knox_geom))

# * 2. Mosquito sites ----

sites <- readxl::read_excel('data/Mosquito Data/2022 Knox County Sites.xlsx') %>%
  rename(site_id=`Site Code`)


# Create points from raw coordinates 
sites_sf <- st_as_sf(sites, coords=c("lon","lat"))%>% 
  # define the original coordinate system
  st_set_crs(4269) %>%
  # transform to albers equal area 
  st_transform(st_crs(albersproj4string))

plot(st_geometry(sites_sf),add=TRUE)

# buffers
buffer_250m <- st_buffer(sites_sf,250) 
buffer_500m <- st_buffer(sites_sf,500) 

plot(st_geometry(buffer_250m),add=TRUE)

# * 3. mosquito data ----

knox22 <- readxl::read_excel('data/Mosquito Data/2022-Knox-Collections-Pools.xlsx',
                             sheet='2022_AllData') %>%
  filter(Collection_Method == 'ovicup') %>%
  select(Site_ID, Trap_Number, Calendar_Week, hatched_eggs, embryonating_eggs,
         total_eggs, albopictus_female, albopictus_male, triseriatus_female,
         triseriatus_male, japonicus_female, japonicus_male) %>%
  mutate(across(c(total_eggs, albopictus_female, albopictus_male, triseriatus_female,
                 triseriatus_male, japonicus_female, japonicus_male),as.numeric),
         total_albo = albopictus_female+albopictus_male,
         total_tris = triseriatus_female+triseriatus_male,
         total_jap = japonicus_female + japonicus_male,
         total_albo = ifelse(total_eggs == 'NA','NA',total_albo),
         total_tris = ifelse(total_eggs == 'NA','NA', total_tris),
         total_jap = ifelse(total_eggs == 'NA','NA', total_jap))


# 4. Land cover rasters ---- 

# load rasters with raster::raster() function 

# ndvi
ndvi <- raster('data/Raster data/NDVI/NDVI_June222022_AlbersUSGS_ClippedtoKnox.tif')
plot(ndvi,add=TRUE)



canopy <- raster('data/Raster data/NLCD LULC/canopy_NLCD2019_AlbersUSGS.tif')
plot(canopy)

# impervious surface cover

impervious <- raster('data/Raster data/NLCD LULC/impervious_NLCD2019_AlbersUSGS.tif')
plot(impervious)


lulc <- raster('data/Raster data/NLCD LULC/NLCD_2019_landcover_clippedtoKnox.tif')
plot(lulc)
# * 5. Census data ---

# load the tract boundaries 

tracts <- st_read('data/Census Files/2021 tract boundaries/US_tract_2021.shp') %>%
  # transform to albers equal area 
  st_transform(crs=st_crs(albersproj4string))

# load the census data

census_data <- read.csv('data/Census Files/2021 tract demographic data/nhgis0040_ds254_20215_tract.csv') %>%
  filter(STATE=='Tennessee',COUNTY=='Knox County') %>%
  # change col names to something legible
  rename(total_population = AON4E001,
         median_income = AOQIE001,
         median_year_structure = AOTIE001) %>%
  # calculate percent under high school 
  mutate(percent_under_highschool = 100*(select(.,AOP8E002:AOP8E016) %>% 
                                           rowSums(na.rm=TRUE)/AOP8E001)) %>%
  select(GISJOIN,total_population,median_income,median_year_structure,percent_under_highschool) %>%
  # join with geometry
  left_join(tracts,by="GISJOIN") %>%
  # calculate pop density in sqkm
  mutate(population_density_sqm = total_population/(Shape_Area/1000000)) %>%
  st_as_sf()

# * 5. Environmental data ---

climate <- readxl::read_excel('data/Weather data/2022 knoxville weather data.xlsx',
                              sheet='data') 
# remove time from the date time column
climate$Date <- as.Date(climate$Date)
# create a calendar week column
climate$calendar_week <- strftime(climate$Date, format = "%V")

#### Calculate zonal stats ----

# * 1. Rasters ----

# create list of raster layers 
rasters <- as.list(ndvi,canopy,impervious)

# for loop to calculate zonal statistics for every raster 
for (i in 1:length(rasters)) {
  # calculate mean in 250m buffer 
  dat <- raster::extract(x=rasters[[i]],y=buffer_250m,fun=mean,df=TRUE)
  new <- rep(i, nrow(sites))  # Create new column in sites df
  sites[ , ncol(sites) + 1] <- dat[,2] # Append new column with the zonal stats
}

# rename columns in order 

names(sites)

colnames(sites) <- c('site_id','Common Name','lon','lat','dominant_lulc_250m','percent_developed_250m','dominant_lulc_500m','percent_developed_500m',
'ndvi','canopy','impervious')
names(sites)


# * 2. Census ----

#### Calculate zonal census stats and join with sites list ----
sites_joined <- census_data %>%
  st_intersection(buffer_250m) %>%
  group_by(site_id) %>%
  summarise(mean_popdens =mean(population_density_sqm,na.rm=TRUE),
            mean_income = mean(median_income,na.rm=TRUE),
            mean_education = mean(percent_under_highschool,na.rm=TRUE),
            mean_yearstructure = mean(median_year_structure,na.rm=TRUE)) %>%
  st_drop_geometry() %>%
  left_join(sites,by='site_id')%>%
  # remove extra trash cleanup sites 
  filter(!site_id %in% c('FS-2','FS-3','FS-4',
                         'HR-2','HR-3','HR-4','HR-5',
                         'TM-2','TM-3','TM-4','TM-5')) %>%
  # rename cleanup sites to match simple site id column in mosquito data
  mutate(site_id = ifelse(site_id=='HR-1','HR',site_id),
         site_id = ifelse(site_id=='TM-1','TM',site_id),
         site_id = ifelse(site_id=='FS-1','FS',site_id))



#### Join site data with mosquito data ----

knox22_joined <- left_join(knox22,sites_joined,by=c('Site_ID'="site_id")) %>%
  # remove extra trash cleanup sites
  filter(Trap_Number < 2)


#### Summarize climate data ----

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

#### Join climate data with knox 22 mosquito data ----

knox22_joined <- left_join(knox22_joined,climate_weekly,
                           by=c('Calendar_Week' = 'calendar_week'),
                           multiple='all') %>% # bc there are mulitple matches
                mutate(north_south = ifelse(Site_ID %in% c('BC','HM','TM','DH','AB',
                                                           'LP','UT','CC','SP','HR',
                                                           'TY','WW','FS'),
                                            'north',
                                            'south'),
                       calendar_month = ifelse(Calendar_Week %in% c(22:26),
                                               'June',
                                               ifelse(Calendar_Week %in% c(27:30),
                                                      'July',
                                                      ifelse(Calendar_Week %in% c(31:34),
                                                             'August',
                                                             ifelse(Calendar_Week %in% c(35:38),
                                                                    'September',
                                                                    'October')))),
                       developed_intensity = ifelse(percent_developed_250m >0 & percent_developed_250m < 25,
                                                    'low',
                                                    ifelse(percent_developed_250m >25.01 & percent_developed_250m < 59,
                                                           'medium',
                                                           'high')),
                       across(c(percent_developed_250m,percent_developed_500m),as.numeric))
    
#### Data cleaning cheack ----

str(knox22_joined) # columns look right

psych::describe(knox22_joined) # no obvious problems

janitor::tabyl(knox22_joined$Site_ID) # looks right

# everything looks pretty good, so let's save it as an RDS

write_rds(knox22_joined,file='data/Mosquito Data/cleaned_joined_knox22.RDS')
