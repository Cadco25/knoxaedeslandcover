library('tidyverse')
library('lubridate')

####  1. Load csv files as a list ----
filenames <- list.files(path="data/Weather data/HOBO_Logs/",pattern="*.csv") # list of csv filenames
setwd("data/Weather data/HOBO_Logs/") # change working directory for loop below

logger_list <- lapply(filenames, function(i){
  read.csv(i,skip=1) # read each file into a list
})

setwd("../../../") # reset working directory


#### 2. Clean files and prepare for stat summary ----

for(i in 1:length(logger_list)) {
  colnames(logger_list[[i]]) <- c("X","Date_Time_GMT","Site", "Temp","RH") # clean column names
  keepcols <- c("Site","Date_Time_GMT","Temp","RH") 
  logger_list[[i]] <- logger_list[[i]][,keepcols] # drop unneeded columns
  logger_list[[i]]$Date_Time_GMT <- mdy_hm(logger_list[[i]]$Date_Time_GMT,tz="GMT") # format datetime column
  logger_list[[i]]$Date <- as.Date(logger_list[[i]]$Date_Time_GMT) # create date-only column
  logger_list[[i]]$Time <- format(logger_list[[i]]$Date_Time_GMT, format = "%H:%M") # create time-only column
  keepcols <- c("Site","Date_Time_GMT",'Date','Time',"Temp","RH") 
  logger_list[[i]] <- logger_list[[i]][,keepcols] # drop unneeded column 
  logger_list[[i]]$calendar_week <- format(logger_list[[i]]$Date, format = "%W") # create calendar week column
  logger_list[[i]]$month <- month(logger_list[[i]]$Date) # create calendar week column
  logger_list[[i]] <- logger_list[[i]] %>% filter(Date > "2022-06-08" & Date < "2022-10-10") # remove deployment and retrieval dates
  colnames(logger_list[[i]]) <- tolower(colnames(logger_list[[i]])) # lowercase column names
  
}

head(logger_list[[1]])

#### 3. Concatenate the files ----

loggers <- logger_list %>% bind_rows()
loggers <- loggers %>%
  mutate(north_south = ifelse(site %in% c('UT','CC','SP','DH','AB','LP','SQ','BC','HM','WW'),
                              'north',
                              'south'),
         dominant_landcover = ifelse(
           site %in% c('CP','FR','HG','IC','MC','MV','WT','HR'),
           'forest',
           ifelse(site %in% c('BC','LP','SP','SQ','ST','WC','WW'),
                  'open',
                  'developed')))
loggers$month <- as.factor(as.character(loggers$month))

urb_22 <- readxl::read_excel('data/Mosquito Data/2022 Knox County Sites.xlsx') %>%
  filter(`Site Code` != ' FS' & `Site Code` !='HR' & `Site Code` != 'TM') %>%
  rename(site_id=`Site Code`) 


loggers_joined <- left_join(loggers,urb_22, by=c('site'='site_id'))
head(loggers_joined)

#### 4. Save formatted data as RDS ----

write_rds(loggers_joined,'data/Weather Data/cleaned_joined_HOBO.RDS')

