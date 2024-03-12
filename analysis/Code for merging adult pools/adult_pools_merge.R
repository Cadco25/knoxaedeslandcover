library('tidyverse')


#### Prepare Adult Pool Spreadsheet for Merging ----

#### Do not need to run this again - useful code for converting adult pools
#### and merging into a wide-form dataset 



knox22 <- readxl::read_excel('data/Mosquito Data/2022-Knox-Collections-Pools.xlsx',
                             sheet='2022_AllData') %>%
  filter(Collection_Method == 'ovicup')
knox22 <- knox22[,1:12]

knox22_RNA <- readxl::read_excel('data/Mosquito Data/2022-Knox-Collections-Pools.xlsx',
                                 sheet='RNA_Pools_LACV')%>%
  tidyr::separate(NumberSex, 
                  c("adults", "Sex"), 
                  sep = "(?<=[0-9])(?=[A-Za-z])") %>%
  filter(Trap_Type == 'ovicup')


knox22_DNA <- readxl::read_excel('data/Mosquito Data/2022-Knox-Collections-Pools.xlsx',
                                 sheet = "DNA_Pools_LACV")%>%
  tidyr::separate(NumberSex, 
                  c("adults", "Sex"), 
                  sep = "(?<=[0-9])(?=[A-Za-z])") %>%
  filter(Trap_Type == 'ovicup')



#?<= is "before cursor" and ?= is after cursor. Before cursor, there are numbers, after cursor, there are letters;
## so separate at the cursor. 


knoxadults <- rbind(knox22_RNA, knox22_DNA)



knoxadults$trap_id <- paste(knoxadults$Year,knoxadults$Site,
                            knoxadults$Trap,knoxadults$Calendarwk, sep="-")

knoxadults$adults <- as.numeric(knoxadults$adults)
knoxadults$trap_id <- as.factor(knoxadults$trap_id)
knoxadults$trap_id <- as.factor(knoxadults$trap_id)

knoxadults <- knoxadults %>%
  mutate(male_tris = case_when(
    grepl("tris", GenusSpecies)&grepl("M", Sex) ~ adults),
    female_tris = case_when(
      grepl("tris", GenusSpecies)&grepl("F", Sex) ~ adults),
    male_albo = case_when(
      grepl("albo", GenusSpecies)&grepl("M", Sex) ~ adults),
    female_albo = case_when(
      grepl("albo", GenusSpecies)&grepl("F", Sex) ~ adults),
    male_jap = case_when(
      grepl("jap", GenusSpecies)&grepl("M", Sex) ~ adults),
    female_jap = case_when(
      grepl("jap", GenusSpecies)&grepl("F", Sex) ~ adults)
  )







knoxadults <- knoxadults %>%
  group_by(Calendarwk, Site, Trap, trap_id) %>%
  summarise(female_albopictus = sum(female_albo, na.rm=TRUE),
            male_albopictus = sum(male_albo, na.rm=TRUE),
            total_albopictus = sum(male_albopictus, female_albopictus),
            female_triseriatus=sum(female_tris, na.rm=TRUE),
            male_triseriatus=sum(male_tris, na.rm = TRUE),
            total_triseriatus=sum(male_triseriatus,female_triseriatus),
            female_japonicus=sum(female_jap, na.rm=TRUE),
            male_japonicus=sum(male_jap, na.rm=TRUE),
            total_japonicus=sum(male_japonicus,female_japonicus),
            total_adults = sum(total_albopictus,total_triseriatus,total_japonicus))

#### Merge 2022 adult pool sheet with 2022 data sheet ----
knox22 <- knox22 %>%
  dplyr::left_join(knoxadults, 
                   by = c("Calendar_Week" = "Calendarwk","Site_ID" = "Site",
                          "Trap_Number"="Trap"))

view(knox22)

# convert NA values in total adults to zero when > 0 eggs were collected
knox22$female_albopictus[which(is.na(knox22$female_albopictus) & knox22$total_eggs > 0)] <- 0
knox22$male_albopictus[which(is.na(knox22$male_albopictus) & knox22$total_eggs > 0)] <- 0
knox22$total_albopictus[which(is.na(knox22$total_albopictus) & knox22$total_eggs > 0)] <- 0
knox22$female_triseriatus[which(is.na(knox22$female_triseriatus) & knox22$total_eggs > 0)] <- 0
knox22$male_triseriatus[which(is.na(knox22$male_triseriatus) & knox22$total_eggs > 0)] <- 0
knox22$total_triseriatus[which(is.na(knox22$total_triseriatus) & knox22$total_eggs > 0)] <- 0
knox22$female_japonicus[which(is.na(knox22$female_japonicus) & knox22$total_eggs > 0)] <- 0
knox22$male_japonicus[which(is.na(knox22$male_japonicus) & knox22$total_eggs > 0)] <- 0
knox22$total_japonicus[which(is.na(knox22$total_japonicus) & knox22$total_eggs > 0)] <- 0
knox22$total_adults[which(is.na(knox22$total_adults) & knox22$total_eggs > 0)] <- 0

# replace 0 adults with NA if the egg total was also NA 
knox22 <- knox22 %>% mutate(total_adults = ifelse(Collection_Method=='ovicup'&grepl("NA", total_eggs) == TRUE,
                                                  "NA",
                                                  total_adults),
                            total_albopictus = ifelse(Collection_Method=='ovicup'&grepl("NA", total_eggs) == TRUE,
                                                      "NA",
                                                      total_albopictus),
                            total_triseriatus = ifelse(Collection_Method=='ovicup'&grepl("NA", total_eggs) == TRUE,
                                                       "NA",
                                                       total_triseriatus),
                            total_japonicus = ifelse(Collection_Method=='ovicup'&grepl("NA", total_eggs) == TRUE,
                                                     "NA",
                                                     total_japonicus))

write_csv(knox22, "data/knox22_adults_merged.csv")
