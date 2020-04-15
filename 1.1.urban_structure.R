#### Libraries ####

library(tidyverse)
library(knitr)
library(sf)
library(sp)
library(tidyr)
library(viridis)
library(gtable)
library(rgdal)
library(rgeos)
library(grid)
library(raster)
library(data.table)
library(spdep)
library(scales)
library('microdadosBrasil')
library(expss)
library(ineq)
library(DescTools)
library(mipfp)
library(ipfp)
library(questionr)
library(seg)
library(ggsn)
library(splitstackshape)
library(BAMMtools)
library(fs)
library(readr)
library(matrixStats)
library(SDMTools)
library(dineq)
library(reldist)
library(OasisR)
library(spatialreg)
library(magrittr)
library(lwgeom)

mutate <- dplyr::mutate
select <- dplyr::select
filter <- dplyr::filter
rename <- dplyr::rename
distinct <- dplyr::distinct
pull <- dplyr::pull
summarize <- dplyr::summarize
nest <- tidyr::nest

setwd("D:/Drive - Taina/Doc/Analysis/")

#### Replication codes ####

states <- c("PR","RJ","SP","CE")

st_codes <- c("41","33","35","23")

# Metropolitan regions' official municipalities codes
MR_codes <- c("4128633","4127882","4127601","4125506","4122305","4122206","4121208","4120804","4119509","4119152","4119103","4114302","4113205","4111258","4107652","4106902","4106209","4105805","4105201","4104253","4104204","4104105","4104006","4103107","4102307","4101804","4100400","4100301","4100202","4106902","3522505","3523107","3525003","3526209","3528502","3529401","3530607","3534401","3539103","3539806","3543303","3544103","3545001","3546801","3547304","3547809","3548708","3548807","3549953","3550308","3552502","3552809","3556453","3503901","3505708","3506607","3509007","3509205","3510609","3513009","3513801","3515004","3515103","3515707","3516309","3516408","3518305","3518800","3522208","3300456","3300803","3301702","3301850","3301900","3302007","3302270","3302502","3302700","3302858","3303203","3303302","3303500","3303609","3303906","3304144","3304300","3304557","3304904","3305109","3305554","3305752", "2301000", "2303501", "2303709", "2303956", "2304285", "2304400", "2304954", "2305233", "2306256", "2307650", "2307700", "2309607", "2309706", "2310852", "2312403", "2312601", "2310258", "2310209", "2313500")

cd_metro <- c("4106902","3304557","3550308","2304400")

crs_utm <- c("+proj=utm +zone=22 +datum=WGS84 +units=m +ellps=WGS84 +towgs84=0,0,0",
             "+proj=utm +zone=23 +datum=WGS84 +units=m +ellps=WGS84 +towgs84=0,0,0",
             "+proj=utm +zone=23 +datum=WGS84 +units=m +ellps=WGS84 +towgs84=0,0,0",
             "+proj=utm +zone=23 +datum=WGS84 +units=m +ellps=WGS84 +towgs84=0,0,0")

#MR_new_codes <- c("4100400","4101804","4104006","4104204","4104253","4105805","4106902","4107652","4111258","4119152","4119509","4120804","4125506",
#                  "3300456","3301702","3301850","3301900","3302007","3302270","3302502","3302700","3302858","3303203","3303500","3304144","3304557","3304904","3305109","3305554","3305752",
#                  "3503901","3505708","3506607","3509007","3509205","3510609","3513009","3513801","3515004","3515103","3515707","3516309","3516408","3518800","3522208","3522505","3523107","3525003","3528502","3529401","3530607","3534401","3539103","3539806","3543303","3544103","3546801","3547304","3547809","3548708","3548807","3549953","3550308","3552502","3552809","3556453",
#                  "2303709","2304285","2304400","2306256","2307650","2307700","2309706","2301000")

#### Urban area ####

# 1. Run coverage map classification for 2017 (PR, SP and RJ) from MapBiomas using Google Earth Engine
# link: http://mapbiomas.org/pages/scripts

# 2. Reclass pixel values in QGIS using raster calculator
# (value  >  0  AND value  <= 23 ) * 1 +
# (value =  24 ) * 3 +
# (value =  25 ) * 1 +
# (value =  26 ) * 2 +
# (value  >= 28  AND value  <= 30 ) * 1 +
# (value  >= 31  AND value <= 33 ) * 2 +
# (value =  0) * 1

# 3. Convert raster to vector in Qgis

# 4. Cut vector based on official metropolitan regions in Qgis

# 5. Extract only urban area using filter

#### Education and occupation for each individual ####

educ_occup <- function(state){
  
  read_CENSO('pessoas',2010, UF = state, root_path = "Inputs/Census_Sample") %>% 
    mutate(CD_GEOCODI = paste0(as.character(V0001),as.character(V0002))) %>%
    filter(CD_GEOCODI %in% MR_codes) %>%
    mutate(transitions = case_when(V0628 %in% c("1","2") & V0629 %in% c("1","2","3","4","5","6") ~ "T1a",
                                   V0628 %in% c("1","2") & V0629 %in% c("7","8") ~ "T3a",
                                   V0628 %in% c("1","2") & V0629 == "9" ~ "T5a",
                                   V0628 %in% c("1","2") & V0629 %in% c("10","11","12") ~ "T6a",
                                   V0628 == "3" & V0633 %in% c("1","2","3","4","5","6") ~ "T1b",
                                   V0628 == "3" & V0633 %in% c("7","8") & V0634 == "2" ~ "T1b1",
                                   V0628 == "3" & V0633 %in% c("7","8") & V0634 == "1" ~ "T2b",
                                   V0628 == "3" & V0633 %in% c("9","10") & V0634 == "2" ~ "T3b",
                                   V0628 == "3" & V0633 %in% c("9","10") & V0634 == "1" ~ "T4b",
                                   V0628 == "3" & V0633 == "11" & V0634 == "2" ~ "T5b",
                                   V0628 == "3" & V0633 == "11" & V0634 == "1" ~ "T6b1",
                                   V0632 == "1" | V0628 == "3" & V0633 %in% c("12","13","14") ~ "T6b2",
                                   V0627 == "2" | V0628 == "4" ~ "T0",
                                   TRUE ~ V6400),
           cod_education = case_when(transitions == "T0" ~ "0", # illiterate
                                     transitions %in% c("T1a","T1b","T1b1","1") ~ "1", # primary incomplete
                                     transitions %in% c("T2b","2") ~ "2", # primary complete
                                     transitions %in% c("T3a","T3b") ~ "3", # high school incomplete
                                     transitions %in% c("T4b","3") ~ "4", # high school complete
                                     transitions %in% c("T5a","T5b") ~ "5", # superior incomplete
                                     transitions %in% c("T6a","T6b1","T6b2","4") ~ "6"), # superior incomplete 
           years_study = case_when(V0633 == "01" ~ 0, # nursery
                                   V0633 == "02" ~ 2, # literacy for young people and adults
                                   V0633 %in% c("03","05") & V0634 == "1" ~ 3, # primary complete (1-3)
                                   V0633 %in% c("03","05") & V0634 != "1" ~ 1, # primary incomplete (1-3)
                                   V0633 == "06" & V0634 == "1" ~ 4, # 4th grade complete
                                   V0633 == "06" & V0634 != "1" ~ 3, # 4th grade incomplete
                                   V0633 %in% c("04","07","08") & V0634 == "1" ~ 8, # secondary complete (5-8)
                                   V0633 %in% c("04","07","08") & V0634 != "1" ~ 4, # secondary incomplete (5-8)
                                   V0633 %in% c("09","10") & V0634 == "1" ~ 11, # high school complete (1-3)
                                   V0633 %in% c("09","10") & V0634 != "1" ~ 8, # high school incomplete (1-3)
                                   V0633 == "11" & V0634 == "1" ~ 16, # superior complete
                                   V0633 == "11" & V0634 != "1" ~ 11, # superior incomplete
                                   V0633 == "12" & V0634 == "1" ~ 17, # specialization course complete
                                   V0633 == "12" & V0634 != "1" ~ 16, # specialization course incomplete
                                   V0633 == "13" & V0634 == "1" ~ 18, # master complete
                                   V0633 == "13" & V0634 != "1" ~ 16, # master incomplete
                                   V0635 == "1" ~ 16, # superior complete
                                   V0635 == "2" ~ 18, # master complete
                                   V0635 == "3" ~ 22), # doctorate
           V6036 = as.numeric(V6036),
           occup = case_when(V6036 > 10 & (V0641 == "1" | V0642 == "1" | V0643 == "1" | V0644 == "1") ~ "employed",
                             V6036 > 10 & V0654 == "1" & V0655 == "1" & V0641 != "1"  & V0642 != "1" & V0643 != "1" & V0644 != "1" ~ "unemployed"),
           cod_pos_wcup = case_when(V0648 == "7" ~ "0", # unpaid
                                    V0648 %in% c("1","2","3","4") ~ "1", # employee
                                    V0648 == "6" ~ "2", # employer
                                    V0648 == "5" ~ "3")) %>% # autonomous
    left_join(read.csv("Inputs/EGP_Classification/cod_ISIC.csv", sep=";") %>%
                mutate(V6471=as.character(V6471)) %>%
                select(V6471,cod_ISIC,ISIC),by="V6471") %>%
    left_join(read.csv("Inputs/EGP_Classification/cod_ISCO_EGP.csv", sep=";") %>%
                select(V6461,cod_ISCO,cod_EGP) %>%
                mutate(V6461=as.character(V6461)), by="V6461") %>%
    mutate(cod_ISCO = as.character(cod_ISCO),
           cod_ISCO = case_when(cod_ISCO %in% c("6121","6129","6122","6123","6113","6111") & cod_pos_wcup =="2" ~ "1311", # Rural employers
                                cod_ISCO == "2131" & cod_education %in% c("0","1","2","3","4") ~ "3121", # analists and programers
                                cod_ISCO %in% c("9211","6113") & cod_ISIC %in% c("114","120") ~ "9141", # gardeners
                                TRUE ~ cod_ISCO),
           cod_EGP = as.character(cod_EGP),
           cod_EGP = case_when(cod_pos_wcup == "2" & cod_ISIC == "10" ~ "6",
                               cod_EGP == "11" & cod_pos_wcup == "2" ~ "6",
                               cod_EGP == "11" & cod_pos_wcup %in% c("0","3") ~ "7",
                               cod_EGP == "11" & V6513 == "0" ~ "7",
                               V6461 %in% c("0000","9629") & cod_ISIC == "10" &  transitions %in% c("5","6") & cod_pos_wcup == "1"  ~ "8",
                               V6461 %in% c("0000","9629") & cod_ISIC == "10" &  transitions %in% c("0","1","2","3","4") & cod_pos_wcup == "1"  ~ "11",
                               V6461 %in% c("0000","9629") & cod_ISIC == "10" & cod_pos_wcup %in% c("0","3")  ~ "7",
                               V6461 %in% c("0000","9629") & cod_ISIC != "10" & cod_pos_wcup == "2"  ~ "5",
                               is.na(cod_EGP) & cod_pos_wcup == "2"  ~ "5", 
                               TRUE ~ cod_EGP),
           EGP = case_when(cod_EGP == "1" ~ "I. Higher professionals",
                           cod_EGP == "2" ~ "II. Lower professionals",
                           cod_EGP == "3" ~ "IIIa. Routine non-manuals - higher degree",
                           cod_EGP == "4" ~ "IIIb. Routine non-manuals - lower degree",
                           cod_EGP == "5" ~ "IVa2. Proprietors and employers",
                           cod_EGP == "6" ~ "IVc1. Rural employers",
                           cod_EGP == "7" ~ "IVc2. Self-employed farmers and subsistence agriculture workers",
                           cod_EGP == "8" ~ "V. Technicians and Supervisors of manual workers",
                           cod_EGP == "9" ~ "VI. Skilled workers",
                           cod_EGP == "10" ~ "VIIa. Semi and unskilled workers",
                           cod_EGP == "11" ~ "VIIb. Agricultural workers"),
           color = case_when(V0606 %in% c("1","3") ~ "white",
                             V0606 %in% c("2","4","5") ~ "black")) %>%
    saveRDS(paste0("Temporary/",tolower(state),"_egp_wa_MR.rds"))}

states %>% map(educ_occup)

#### Metropolitan region ####

metro_region <- function(state,st_code){
  
  # Start from the official metropolitan regions
  code <- MR_codes[substr(MR_codes,1,2) == st_code]
  
  ## Check for urban contiguity criteria
  
  # Multipart to singleparts
  urban <- st_read(paste0("Inputs/Land_Cover/Shapefiles/",tolower(state),"_urban_MR.shp")) %>%
    st_cast("POLYGON") %>%
    st_join(st_read(paste0("Inputs/Census_Shapefiles/",st_code,"MUE250GC_SIR.shp")) %>%
              st_transform(4326) %>%
              filter(CD_GEOCODM %in% MR_codes),by = "st_within")
  
  # Dissolve polygons inside the metropolis
  main <- urban %>% st_buffer(0) %>%
    filter(CD_GEOCODM == cd_metro) %>%
    summarise() %>%
    mutate(CD_GEOCODM = cd_metro)
  
  # Merge with other polygons and create temporary id
  urban <- main %>% rbind(urban %>%
                            filter(CD_GEOCODM != cd_metro) %>%
                            select(CD_GEOCODM,geometry)) %>%
    mutate(id = row_number())
  
  # Look for metropolis neighbors
  neighbors <- urban %>% as('Spatial') %>% poly2nb(queen=T)
  neighbors <- c(n.comp.nb(neighbors)$comp.id)
  neighbors <- c(which(neighbors == 1))
  
  # Filter municipalities with urban contiguity
  urban <- urban %>% filter(id %in% neighbors) %>%
    rename("mun" = CD_GEOCODM) %>%
    select(mun) %>%
    st_join(st_read(paste0("Inputs/Census_Shapefiles/",st_code,"MUE250GC_SIR.shp")) %>%
              st_transform(4326),
            by = "st_overlaps")
  
  urban_filter <- as.character(unique(urban$CD_GEOCODM))
  
  ## Check for urban workers and trips criteria 
  
  # Prepare data
  data <- st_read(paste0("Inputs/Census_Shapefiles/",st_code,"MUE250GC_SIR.shp")) %>%
    st_transform(4674) %>%
    mutate(area = as.numeric(st_area(.))/1000000) %>%
    st_transform(4326) %>%
    filter(CD_GEOCODM %in% code) %>%
    left_join(readRDS(paste0("Temporary/",tolower(state),"_egp_wa_MR.rds")) %>%
                mutate(CD_GEOCODAP = as.character(V0011)) %>%
                rename("weight" = V0010, "work_mun" = V6604, "study_mun" = V6364) %>%
                select(CD_GEOCODAP,weight,work_mun,study_mun,occup,cod_EGP) %>%
                mutate(work_mun = ifelse(substr(work_mun,1,7) %in% code,weight,0),
                       study_mun = ifelse(substr(study_mun,1,7) %in% code,weight,0),
                       occup = ifelse(occup == "employed",weight,0),
                       urban = ifelse(!cod_EGP %in% c("6","7","11"), weight,0),
                       CD_GEOCODM = substr(CD_GEOCODAP,1,7)) %>%
                group_by(CD_GEOCODM) %>%
                summarise_at(c("work_mun","study_mun","occup","urban","weight"),~(p=sum(.,na.rm = T))),
              by = "CD_GEOCODM") %>%
    rename("pop" = weight) %>%
    mutate(density = pop/area,
           trips = (work_mun + study_mun)/pop,
           urban_work = urban/occup) %>%
    mutate(trips = ifelse(CD_GEOCODM == cd_metro,1,trips))
  
  # Filter muniicpalities based on trips, density and urban workers
  data_filter <- data %>% filter(trips >= 0.10) %>% filter(density >= 60) %>% filter(urban_work >= 0.7)
  data_filter <- as.character(unique(data_filter$CD_GEOCODM))
  
  ## Define new metropolitan regions 
  
  # Merge urban and data filters
  code <- unique(c(data_filter,urban_filter))
  
  # Check for municipality contiguity
  data_filter <- data %>%
    filter(CD_GEOCODM %in% code) %>%
    mutate(id = row_number())
  neighbors <- data_filter %>% as('Spatial') %>% poly2nb(queen=T)
  neighbors <- c(n.comp.nb(neighbors)$comp.id)
  neighbors <- c(which(neighbors==1))
  data_filter <- data_filter %>% filter(id %in% neighbors)
  data_filter <- as.character(unique(data_filter$CD_GEOCODM))
  
  # Final code
  code <- unique(c(data_filter,urban_filter))
  MR_new_codes <- c(MR_new_codes,code)}

tibble(state = states,st_code = st_codes) %>% pmap(metro_region)

# Retain only infomation of redifined metro region
metro_cut <- function(state,st_code){
  
  # Define final metropolitan region
  st_read(paste0("Inputs/Census_Shapefiles/",st_code,"MUE250GC_SIR.shp")) %>%
    filter(CD_GEOCODM %in% MR_new_codes) %>%
    st_transform(4326) %>%
    saveRDS(paste0("Temporary/",state,"_mun_MR.rds"))
  
  # Update weighting area database
  readRDS(paste0("Temporary/",tolower(state),"_egp_wa_MR.rds")) %>%
    mutate(CD_GEOCODM = substr(as.character(V0011),1,7)) %>%
    filter(CD_GEOCODM %in% MR_new_codes) %>%
    saveRDS(paste0("Temporary/",tolower(state),"_egp_wa_MR_cut.rds"))
  }

tibble(state = states,st_code = st_codes) %>% pmap(metro_cut)

#### Social classes - clusters ####

# Individual data from all cities

egp <- paste0("Temporary/",tolower(states),"_egp_wa_MR_cut.rds") %>% map_df(readRDS)

##### Summary of each EGP ####

# EGP groups by race
aux1 <- egp %>%
  group_by(V0001,EGP,color) %>%
  summarise(people = sum(V0010),
            income_work = round(weightedMedian(V6513,V0010,na.rm = TRUE),0),
            income_pc = round(weightedMedian(V6531,V0010,na.rm = TRUE),0),
            literacy = round(weightedMedian(years_study,V0010,na.rm = TRUE),0)) 

# EGP groups
aux2 <- egp %>%
  group_by(V0001,EGP) %>%
  summarise(people = sum(V0010),
            income_work = round(weightedMedian(V6513,V0010,na.rm = TRUE),0),
            income_pc = round(weightedMedian(V6531,V0010,na.rm = TRUE),0),
            literacy = round(weightedMedian(years_study,V0010,na.rm = TRUE),0)) %>%
  mutate(color = "all")

aux3 <- egp %>%
  group_by(EGP) %>%
  summarise(people = sum(V0010),
            income_work = round(weightedMedian(V6513,V0010,na.rm = TRUE),0),
            income_pc = round(weightedMedian(V6531,V0010,na.rm = TRUE),0),
            literacy = round(weightedMedian(years_study,V0010,na.rm = TRUE),0)) %>%
  mutate(color = "all")

# Merge data to produce just one table
rbind(aux1,aux2) %>% write.csv("Outputs/CSV/summary_egp.csv")

## 3 group reclassification 

occup_clusters <- function(state,st_code){
  
  # Data preparation
  data <- aux2 %>%
    filter(V0001 == st_code) %>%
    filter(EGP %in% c("I. Higher professionals",
                      "II. Lower professionals",
                      "IIIa. Routine non-manuals - higher degree",
                      "IIIb. Routine non-manuals - lower degree",
                      "IVa2. Proprietors and employers",
                      "V. Technicians and Supervisors of manual workers",
                      "VI. Skilled workers",
                      "VIIa. Semi and unskilled workers")) %>%
    ungroup() %>%
    select(-V0001,-color,-people) %>%
    mutate_at(c("income_work","income_pc","literacy"),as.numeric) %>%
    mutate_at(c("income_work","income_pc","literacy"),scale) %>%
    column_to_rownames(var = "EGP")
  
  # Clusters based on work income, household income and education
  cluster <- dist(data, method = "euclidean") %>%
    hclust(method="ward") %>%
    as.dendrogram()
  par(mar = c(3,1,1,20))
  graphics::plot(cluster,horiz = T)
}

tibble(state = states, st_code = st_codes) %>% pmap(occup_clusters)

# Reclassification
reclass <- function(state){
  
  readRDS(paste0("Temporary/",tolower(state),"_egp_wa_MR_cut.rds")) %>%
    mutate(group=case_when(EGP %in% c("I. Higher professionals","II. Lower professionals","IVa2. Proprietors and employers") ~ "high",
                           EGP %in% c("IIIa. Routine non-manuals - higher degree","IIIb. Routine non-manuals - lower degree","V. Technicians and Supervisors of manual workers") ~ "medium",
                           EGP %in% c("VI. Skilled workers","VIIa. Semi and unskilled workers","VIIb. Agricultural workers") ~ "low",
                           EGP %in% c("IVa2. Proprietors and employers","IVc1. Rural employers","IVc2. Self-employed farmers and subsistence agriculture workers") ~ "rural")) %>%
    saveRDS(paste0("Temporary/",tolower(state),"_egp_wa_MR_cut.rds"))}

states %>% map(reclass)

## Summary of each cluster group 

egp <- paste0("Temporary/",tolower(states),"_egp_wa_MR_cut.rds") %>% map_df(readRDS)

# Cluster groups by race
aux1 <- egp %>%
  group_by(V0001,group,color) %>%
  summarise(people = sum(V0010),
            income_work = round(weightedMedian(V6513,V0010,na.rm = TRUE),0),
            sd_inc_work = wt.sd(V6513,V0010)/wt.mean(V6513,V0010),
            income_pc = round(weightedMedian(V6531,V0010,na.rm = TRUE),0),
            sd_inc_pc = wt.sd(V6531,V0010)/wt.mean(V6531,V0010),
            literacy = round(weightedMedian(years_study,V0010,na.rm = TRUE),0),
            sd_study = wt.sd(years_study,V0010)/wt.mean(years_study,V0010))  

# Cluster groups
aux2 <- egp %>%
  group_by(V0001,group) %>%
  summarise(people = sum(V0010),
            income_work = round(weightedMedian(V6513,V0010,na.rm = TRUE),0),
            sd_inc_work = wt.sd(V6513,V0010)/wt.mean(V6513,V0010),
            income_pc = round(weightedMedian(V6531,V0010,na.rm = TRUE),0),
            sd_inc_pc = wt.sd(V6531,V0010)/wt.mean(V6531,V0010),
            literacy = round(weightedMedian(years_study,V0010,na.rm = TRUE),0),
            sd_study = wt.sd(years_study,V0010)/wt.mean(years_study,V0010))   %>%
  mutate(color = "all")

# Merge data to produce just one table
rbind(aux1,aux2) %>% write.csv("Outputs/CSV/summary_clusters.csv")

#### Population synthesis - occupational groups considering race and income ####

int_trs <- function(x){
  xv <- as.vector(x)
  xint <- floor(xv)
  r <- xv - xint
  def <- round(sum(r))
  topup <- sample(length(x), size = def, prob = r)
  xint[topup] <- xint[topup] + 1
  dim(xint) <- dim(x)
  dimnames(xint) <- dimnames(x)
  xint
}

  state <- "CE"

# Aggregated data
ct <- read.csv(paste0("Inputs/Census_Universe/",state,"/CSV/PessoaRenda_",state,".csv"), sep=";") %>%
  filter(substr(Cod_setor,1,7) %in% MR_new_codes) %>%
  mutate_all(~na_if(.,"X")) %>%
  mutate_all(as.character) %>%
  mutate_all(as.numeric) %>%
  mutate(CD_GEOCODI = as.character(Cod_setor)) %>%
  select(CD_GEOCODI,V001,V002,V003,V004,V005,V006,V007,V008,V009,V010) %>%
  left_join(read.csv(paste0("Inputs/Census_Universe/",state,"/CSV/Pessoa03_",state,".csv"),sep=";") %>%
              filter(substr(Cod_setor,1,7) %in% MR_new_codes) %>%
              mutate_all(~na_if(.,"X")) %>%
              mutate_all(as.character) %>%
              mutate_all(as.numeric) %>%
              mutate(CD_GEOCODI = as.character(Cod_setor),
                     white = V002 + V004 - V007 - V009 - V012 - V014,
                     black = V003 + V005 + V006 - V008 - V010 - V011 - V013 - V015 - V016) %>%
              select(CD_GEOCODI,white,black), by = "CD_GEOCODI") %>%
  mutate(income = V001 + V002 + V003 + V004 + V005 + V006 + V007 + V008 + V009 + V010,
         people = white + black,
         conf = income == people) %>%
  mutate(V010 = ifelse(conf == "FALSE",V010 - (income - people),V010)) %>%
  select(-conf,-income,people) %>%
  rename("inc1"=V001,"inc2"=V002,"inc3"=V003,"inc4"=V004,"inc5"=V005,
         "inc6"=V006,"inc7"=V007,"inc8"=V008,"inc9"=V009,"inc0"=V010) %>%
  left_join(read.csv("Inputs/Census_Sample/sc_ap_2010.csv",sep=";",colClasses = "character") %>%
              filter(substr(CD_GEOCODAP,1,7) %in% MR_new_codes),by="CD_GEOCODI") %>%
  filter(!is.na(white)) %>%
  filter(people != 0)

# Individual data
wa <- readRDS(paste0("Temporary/",tolower(state),"_egp_wa_MR_cut.rds")) %>%
  mutate(V0011 = as.character(V0011)) %>%
  rename("age" = V6036,"income_MW" = V6526,"CD_GEOCODAP" = V0011,"weight"=V0010) %>%
  filter(age >= 10) %>%
  mutate(income = case_when(income_MW > 0 & income_MW <= 0.5 ~ "inc1",
                            income_MW > 0.5 & income_MW <= 1 ~ "inc2",
                            income_MW > 1 & income_MW <= 2 ~ "inc3",
                            income_MW > 2 & income_MW <= 3 ~ "inc4",
                            income_MW > 3 & income_MW <= 5 ~ "inc5",
                            income_MW > 5 & income_MW <= 10 ~ "inc6",
                            income_MW > 10 & income_MW <= 15 ~ "inc7",
                            income_MW > 15 & income_MW <= 20 ~ "inc8",
                            income_MW >= 20 ~ "inc9",
                            income_MW == 0 | is.na(income_MW) ~ "inc0")) %>%
  select(CD_GEOCODAP,income,color,weight,occup,group) %>%
  filter(!is.na(color)) %>%
  mutate(id = row_number()) 

# Get all unique ap codes
cod_ap <- unique(wa$CD_GEOCODAP)

# Create fictional database to ensure at least one individual of each color and income
aux <- as.data.frame(cbind(rep(cod_ap,each=20),
                           seq(nrow(wa)+1,nrow(wa)+1+20*length(cod_ap),1),
                           rep(rep(c("inc1","inc2","inc3","inc4","inc5","inc6","inc7","inc8","inc9","inc0"),each=2),length(cod_ap)),
                           rep(c("white","black"),10*length(cod_ap)),
                           rep(1,20*length(cod_ap)),
                           rep(NA,20*length(cod_ap)),
                           rep(NA,20*length(cod_ap))))
colnames(aux) <- c("CD_GEOCODAP","id","income","color","weight","occup","group")
wa <- rbind(wa,aux) %>%
  mutate(weight = as.numeric(weight))

pop_sint <- function(i){
  
  print(i)
  
  # Filter data from only selected weighting area code
  wa_cor <- wa %>% filter(CD_GEOCODAP == i) 
  ct_cor <- ct %>% filter(CD_GEOCODAP == i) 
  
  # Population retification based on census tracts
  aux <- wa_cor %>% filter(color == "white") 
  cor <- sum(ct_cor$white,na.rm=T)/sum(aux$weight,na.rm=T)
  wa_wh <- aux %>% mutate(weight = weight * cor)
  aux <- wa_cor %>% filter(color == "black")
  cor <- sum(ct_cor$black,na.rm=T)/sum(aux$weight,na.rm=T)
  wa_bl <- aux %>% mutate(weight = weight * cor)
  wa_cor <- rbind(wa_wh,wa_bl)
  
  # Prepare individual data (ap)
  ind_orig <- wa_cor
  ind_orig$weight <- int_trs(ind_orig$weight)
  ind <- expandRows(ind_orig,"weight") %>% mutate(id_2 = paste0(id,"-",row.names(.))) %>%
    select(-CD_GEOCODAP,-group,-occup)
  row.names(ind) <- ind$id_2
  ind <- ind %>% select(-id,-id_2)
  
  # Prepare aggregated data (sc)
  con_inc <- ct_cor %>% select(CD_GEOCODI,inc0,inc1,inc2,inc3,inc4,inc5,inc6,inc7,inc8,inc9)
  row.names(con_inc) <- con_inc$CD_GEOCODI
  con_inc <- con_inc %>% select(-CD_GEOCODI)
  con_col <- ct_cor %>% select(CD_GEOCODI,white,black)
  row.names(con_col) <- con_col$CD_GEOCODI
  con_col <- con_col %>% select(-CD_GEOCODI)
  cons <- cbind(con_inc,con_col)
  
  # Constraints categories
  cat_inc <- model.matrix(~ ind$income - 1)
  cat_col <- model.matrix(~ ind$color - 1)[, c(2, 1)]
  ind_cat <- cbind(cat_inc, cat_col)
  ind_catt <- t(ind_cat)
  cons <- apply(cons, 2, as.numeric)
  
  # Individual categories
  n_ind <- nrow(ind)
  x0 <- rep(1, n_ind)
  
  # IPF algorithm
  weights <- apply(cons, MARGIN = 1, FUN = function(x) ipfp(x, ind_catt, x0, maxit = 20))
  
  # Nominate results
  colnames(weights) <- row.names(con_inc)
  row.names(weights) <- row.names(ind)
  weights <- as.data.frame(weights)
  
  # Get occupational groups based on income and race data
  weights <- weights %>%
    mutate(id = str_extract(row.names(.),"\\d{1,10}-"),
           id = substr(id,1,nchar(id)-1)) %>%
    left_join(ind_orig, by = "id") %>%
    select(-CD_GEOCODAP,-income,-id) %>%
    group_by(color,occup,group) %>%
    summarise_all(.,~sum(.,na.rm=T)) %>%
    mutate(key = paste0(group,"_",color)) %>%
    group_by(key) %>%
    select(-group,-color,-occup) %>%
    summarise_all(.,~sum(.,na.rm=T)) %>%
    t()
  colnames(weights) <- weights[1,]
  weights <- weights[-1,]
  weights <- weights %>% as.data.frame() %>%
    mutate(CD_GEOCODI = row.names(weights),CD_GEOCODAP = i) %>%
    select(CD_GEOCODI,CD_GEOCODAP,high_white,high_black,medium_white,medium_black,low_white,low_black) %>%
    mutate_all(as.character) %>%
    mutate_at(c("high_white","high_black","medium_white","medium_black","low_white","low_black"),as.numeric)}

aux1 <- cod_ap %>% map_df(pop_sint)

# Get cluster groups
aux1 %>%
  left_join(read.csv(paste0("Inputs/Census_Universe/",state,"/CSV/Basico_",state,".csv"), sep=";", dec=",") %>%
              select(Cod_municipio,Cod_setor, V002, V009) %>%
              rename("pop"=V002,"income"=V009) %>%
              left_join(read.csv(paste0("Inputs/Census_Universe/",state,"/CSV/Pessoa03_",state,".csv"), sep=";") %>%
                          mutate_at(c("V001","V002","V003","V004","V005","V006"),~na_if(.,"X")), by="Cod_setor") %>%
              mutate_at(c("V001","V002","V003","V004","V005","V006"),~as.character(.)) %>%
              mutate_at(c("V001","V002","V003","V004","V005","V006"),~as.numeric(.)) %>%
              filter(Cod_municipio %in% MR_new_codes) %>%
              mutate(white=V002+V004,black=V003+V005+V006,CD_GEOCODI=as.character(Cod_setor)) %>%
              select(CD_GEOCODI,pop,income,white,black),by="CD_GEOCODI") %>%
  rename("hi_wh" = high_white,
         "hi_bl" = high_black,
         "md_wh" = medium_white,
         "md_bl" = medium_black,
         "lo_wh" = low_white,
         "lo_bl" = low_black) %>%
  saveRDS(paste0("Temporary/",tolower(state),"_egp_ct_MR.rds"))

##### Create hexagonal grid #####

HexGrid <- function(cellsize,state,utm_proj) { 
  
  mun <- readRDS(paste0("Temporary/",state,"_mun_MR.rds"))
  
  # Projections
  CRSlatlong <- "+proj=longlat +datum=WGS84 +no_defs"
  
  # Bounding Box
  mun <- as(mun, 'Spatial') %>% spTransform(utm_proj)
  boundingbox <- as(raster::extent(bbox(mun)), "SpatialPolygons")
  
  # Create hexagonal grid
  HexPts <- spsample(boundingbox, type="hexagonal", offset=c(0.5, 0.5), cellsize=cellsize)
  HexPols <- HexPoints2SpatialPolygons(HexPts)
  
  # CRS Transformations
  crs(HexPols) <- utm_proj
  HexPols <- spTransform(HexPols, CRS(utm_proj))
  
  # Get only polygons inside municipality borders
  hex_poly <- raster::intersect(mun,HexPols)
  
  # Create ID
  hex_poly@data$ID <- 1:nrow(hex_poly@data) %>% as.numeric()
  row.names(hex_poly@data) <- 1:nrow(hex_poly@data) %>% as.character()
  hex_poly@data <- hex_poly@data[1]
  
  # Transformations
  hex_poly <- spTransform(hex_poly,CRS(CRSlatlong))
  hex_poly <- st_as_sf(hex_poly)
  
  hex_poly %>%
    rename("GEO_ID" = ID) %>%
    st_write(.,paste0("Inputs/Grids/",tolower(state),"_hex",cellsize,"_MR.shp"),delete_layer=T)
  }

tibble(cellsize = c(500,1000,2500,5000),state = rep(tolower(states[4]),4),utm_proj = rep(crs_utm[4],4)) %>% pmap(HexGrid)

#### Reapportion to urban grid ####

urban_grid <- function(state,utm_proj,size){
  
  # For memory issues, we've done this part using ArcGis:
  # 1. Filter to DN = 3 (urban cover)
  # 2. "Clip" census tracts based on urban layer (GEE)
  # 3. "Multipart to singleparts"
  
  ct <- st_read(paste0("Temporary/urban_clip/",tolower(state),"_sc_urban_clip.shp")) %>%
    filter(CD_GEOCODM %in% MR_new_codes) %>%
    select(CD_GEOCODI) %>%
    st_transform(4326) %>%
    as("Spatial")
  
  hex <- st_read(paste0("Inputs/Grids/",tolower(state),"_hex",size,"_MR.shp")) %>% as("Spatial")
  
  #urban_hex <- rgeos::gIntersection(ct,hex, byid = T, drop_lower_td = T) %>% st_as_sf() %>% mutate(id_temp = row_number())
  urban_hex <- st_read(paste0("Temporary/urban_clip/",tolower(state),"_urban_hex",size,"_MR.shp")) %>%
    st_transform(4326) %>% select(geometry) %>% mutate(id_temp = row_number())
  
  
  # Get information from original databases, calculate area and reapportion
  urban_hex <- urban_hex %>% left_join(urban_hex %>% st_centroid() %>%
                                         st_join(ct %>% st_as_sf(), by = "st_within") %>%
                                         st_join(hex %>% st_as_sf(), by = "st_within") %>%
                                         st_drop_geometry(), by = "id_temp") %>%
    st_transform(utm_proj) %>%
    mutate(area = as.numeric(st_area(.)/1000000)) %>%
    st_drop_geometry() %>%
    left_join(t <- readRDS(paste0("Temporary/",tolower(state),"_egp_ct_MR.rds")) %>%
                mutate(CD_GEOCODI = as.factor(CD_GEOCODI)),by="CD_GEOCODI") %>%
    group_by(CD_GEOCODI) %>%
    mutate(area_ct = sum(area)) %>%
    ungroup() %>%
    mutate(prop_area = area/area_ct) %>%
    mutate_at(c("pop","white","black","hi_wh","hi_bl","md_wh","md_bl","lo_wh","lo_bl"),
              ~(p=.*prop_area)) %>%
    select(-c(id_temp,CD_GEOCODI,CD_GEOCODAP,area_ct,prop_area,area)) %>%
    mutate(income = as.numeric(as.character(income))) %>%
    group_by(GEO_ID) %>%
    mutate(income = income*pop) %>%
    summarise_all(.,~(p=sum(.,na.rm = T))) %>%
    mutate(income = income/pop) %>%
    mutate_at(.,c("hi_wh","hi_bl","md_wh","md_bl","lo_wh","lo_bl"),
              funs(w = ./(hi_wh+hi_bl+md_wh+md_bl+lo_wh+lo_bl)))
  
  # Merge with geographic information
  hex %>% st_as_sf() %>%
    left_join(urban_hex,by="GEO_ID") %>%
    filter(!is.na(pop)) %>%
    st_write(paste0("Outputs/Shapefiles/",tolower(state),"_hex",size,"_data_MR.shp"),delete_layer=T)
}

tibble(state = rep(states,each=4),utm_proj = rep(crs_utm,each=4),size = rep(c(500,1000,2500,5000),4)) %>% pmap(urban_grid)

#### Get CD_GECODM ####

cod_mun <- function(state,cd_state,size){
  
  hex <- st_read(paste0("Outputs/Shapefiles/",tolower(state),"_hex",size,"_data_MR.shp")) %>% distinct(GEO_ID, .keep_all = T)
  cent <- st_centroid(hex) %>% st_join(st_read(paste0("Inputs/Census_Shapefiles/",cd_state,"MUE250GC_SIR.shp")) %>%
                                         st_transform(4326),by="st_intersects") %>%
    st_drop_geometry() %>%
    select(GEO_ID,CD_GEOCODM)
  hex <- hex %>% left_join(cent, by="GEO_ID") %>% as("Spatial") %>% st_as_sf()
  st_write(hex,paste0("Outputs/Shapefiles/",tolower(state),"_hex",size,"_data_MR.shp"),delete_layer=T)
}

tibble(state = rep(states,each=4),cd_state = rep(st_codes,each=4),size = rep(c(500,1000,2500,5000),4)) %>% pmap(cod_mun)

#### Dominant groups of every cell ####

lisa_f <- function(variable){
  
  local <- as.numeric((variable*W%*%variable))
  y_s <- matrix(variable[ID_sample], nrow = nrow(W), ncol = 2000)
  y_s <- (y_s - apply(y_s, 1, mean))/apply(y_s, 1, sd)
  local_sims  <- (variable*W%*%y_s)
  intervals <- t(apply(local_sims, 1, function(x) quantile(x, probs=probs <- c(0.05/2, 1-0.05/2))))
  sig <- ( local < intervals[,1] )  | ( local > intervals[,2] )
  patterns <- as.character(interaction(variable > 0, W%*%variable > 0))
  patterns <- str_replace_all(patterns,"TRUE","High")
  patterns <- str_replace_all(patterns,"FALSE","Low")
  patterns <- case_when(sig==FALSE~"Not significant",TRUE~patterns)
  patterns <- factor(patterns,levels=c("High.High","High.Low","Low.High", "Low.Low", "Not significant"),labels=c("HH","HL","LH", "LL", "Not signif."))
  return(patterns)}

dom_group <- function(state){
  
  hex <- st_read(paste0("Outputs/Shapefiles/",tolower(state),"_hex250_data_MR.shp")) %>%
    select(GEO_ID,hi_wh_w,hi_bl_w,md_wh_w,md_bl_w,lo_wh_w,lo_bl_w) %>%
    mutate_at(c("hi_wh_w","hi_bl_w","md_wh_w","md_bl_w","lo_wh_w","lo_bl_w"),scale)
  
  # Using LISA clusters
  
  d2 <- hex %>% as("Spatial")
  nb <- poly2nb(d2)
  nb <- nb2listw(nb, style = "B", zero.policy = T)
  W  <- as(nb, "symmetricMatrix")
  W  <- as.matrix(W/rowSums(W))
  W[which(is.na(W))] <- 0
  ID_sample <- sample(1:nrow(W), size = nrow(W)*2000, replace = T)
  
  hi_wh <- lisa_f(d2$hi_wh_w) %>% filter(patterns == "HH") %>% mutate(dom_lisa = 0)
  hi_bl <- lisa_f(d2$hi_bl_w) %>% filter(patterns == "HH") %>% mutate(dom_lisa = 1)
  md_wh <- lisa_f(d2$md_wh_w) %>% filter(patterns == "HH") %>% mutate(dom_lisa = 2)
  md_bl <- lisa_f(d2$md_bl_w) %>% filter(patterns == "HH") %>% mutate(dom_lisa = 3)
  lo_wh <- lisa_f(d2$lo_wh_w) %>% filter(patterns == "HH") %>% mutate(dom_lisa = 4)
  lo_bl <- lisa_f(d2$lo_bl_w) %>% filter(patterns == "HH") %>% mutate(dom_lisa = 5)
  
  lisa <- rbind(hi_wh,hi_bl,md_wh,md_bl,lo_wh,lo_bl) %>%
    st_drop_geometry() %>%
    group_by(GEO_ID) %>%
    summarise(dom_lisa = mean(dom_lisa,na.rm=T)) %>%
    mutate(dom_lisa = case_when(dom_lisa == 0 ~ "High white",
                                dom_lisa == 1 ~ "High Black",
                                dom_lisa == 2 ~ "Medium white",
                                dom_lisa == 3 ~ "Medium Black",
                                dom_lisa == 4 ~ "Low white",
                                dom_lisa == 5 ~ "Low Black",
                                TRUE ~ "Not signif.")) %>%
    select(GEO_ID,dom_lisa)
  
  hex <- hex %>% left_join(lisa) %>%
    mutate(dom = ifelse(!is.na(pop) & is.na(dom_lisa),"Not signif.",dom_lisa))
  
  # Using simple majority
  groups <- hex %>% select(GEO_ID,hi_wh_w,hi_bl_w,md_wh_w,md_bl_w,lo_wh_w,lo_bl_w) %>%
    st_drop_geometry()
  groups$MAX <- apply(groups[,-1],1,max,na.rm=TRUE)
  groups <- groups %>% mutate(dom_max = case_when(MAX == hi_wh_w ~ "hi_wh",
                                                  MAX == hi_bl_w ~ "hi_bl",
                                                  MAX == md_wh_w ~ "md_wh",
                                                  MAX == md_bl_w ~ "md_bl",
                                                  MAX == lo_wh_w ~ "lo_wh",
                                                  MAX == lo_bl_w ~ "lo_bl"))
  
  hex %>% left_join(groups %>% select(GEO_ID,dom_max), by = "GEO_ID") %>%
    st_write(paste0("Outputs/Shapefiles/",tolower(state),"_hex250_data_MR.shp"),delete_layer = T)
  
}

states[1:3] %>% map(dom_group)

#### Income inequality indexes ####

paste0("Temporary/",tolower(states),"_egp_wa_MR_cut.rds") %>%
  map_df(readRDS) %>%
  select(V0001,V0002,V0010,V0011,V6531) %>%
  group_by(V0001) %>%
  summarise(Obs=n(),
            mean_income_mun=weighted.mean(V6531,V0010,na.rm=TRUE),
            median_income_mun=weightedMedian(V6531,V0010,na.rm=TRUE),
            p10=wtd.quantile(V6531,q=0.1,na.rm=T,weight=V0010),
            p90=wtd.quantile(V6531,q=0.9,na.rm=T,weight=V0010),
            p90_10=p90/p10,
            Gini=gini.wtd(V6531,V0010)) %>%
  write.csv("Outputs/CSV/ineq_MR.csv")

#### Spatial segregation indexes ####

seg <- function(size,state,metro,cd_state){
  
  global_aux <- NULL
  group_aux <- NULL
  
  hex <- st_read(paste0("Outputs/Shapefiles/",tolower(state),"_hex",size,"_data_MR.shp")) %>%
    filter(hi_wh+hi_bl+md_wh+md_bl+lo_wh+lo_bl > 0) %>%
    filter_all(.,all_vars(!is.na(.)))# %>%
  #filter(CD_GEOCODM == metro)
  
  groups <- c("hi_wh","hi_bl","md_wh","md_bl","lo_wh","lo_bl")
  
  data <- hex %>% st_drop_geometry() %>%
    select(groups)
  
  shp <- hex %>% select(groups) %>% as("Spatial")
  
  #dist <- distance(shp)
  #area <- area(shp)
  #cont <- contig(shp)
  
  # Global indexes multi-group
  aux <- data.frame(state = state) %>%
    mutate(DMulti = DMulti(x=data),
           #GiniMulti = GiniMulti(x=data),
           HMulti = HMulti(x=data),
           NShannon = NShannon(x=data),
           PMulti = PMulti(x=data),
           RelDivers = RelDivers(x=data))
  seg_global <- rbind(global_aux,aux)
  
  # Global indexes per group
  aux <- data.frame(state = state) %>%
    mutate(Eta2 = list(Eta2(x=data)),
           #Gini = list(OasisR::Gini(x=data)),
           #Gorard = list(Gorard(x=data)),
           HTheil = list(HTheil(x=data)),
           #xPx = list(xPx(x=data)),
           #ACL = list(ACL(x=data,c=cont,d=dist,spatobj=shp)),
           #ACO = list(ACO(x=data,a=area,spatobj=shp)),
           #ISMorrill = list(ISMorrill(x=data,c=cont,spatobj=shp)),
           #Delta = list(Delta(x=data,a=area,spatobj=shp)),
           ISDuncan = list(ISDuncan(x=data))) %>%
    unnest() %>%
    gather(variable,value,-(state)) %>%
    mutate(type=rep_len(groups,n())) %>%
    spread(type,value)
  seg_group <- rbind(group_aux,aux)
  
  # Moran
  neigh_dist <- hex %>% as('Spatial') %>%
    coordinates() %>% knearneigh(6) %>% knn2nb()
  neigh_w_dist <- nb2listw(neigh_dist,zero.policy=TRUE)
  groups_w <- paste0(groups,"_w")
  aux <- NULL
  for(i in groups_w){
    moran <- hex %>% pull(i) %>% moran.test(neigh_w_dist)
    moran <- round(moran[["estimate"]][["Moran I statistic"]],3)
    aux <- cbind(aux,moran)}
  aux <- cbind(state,"moran",aux)
  colnames(aux) <- c("state","variable",groups)
  seg_group <- rbind(seg_group,aux)
  
  # Spatial exposure indexes
  data <- data %>% as.matrix()
  coords <- st_centroid(hex) %>% select()
  coords <- do.call(rbind, st_geometry(coords))
  aux <- as.list(spatseg(env = SegLocal(coords,data,data), method = "all", useC = TRUE, negative.rm = FALSE, tol = .Machine$double.eps))
  aux <- cbind(state,paste0("exp_",groups),aux$p)
  colnames(aux) <- c("state","variable",groups)
  
  aux1 <- c(aux[1,3],aux[2,4],aux[3,5],aux[4,6],aux[5,7],aux[6,8]) %>% as.character() %>% as.numeric()
  pop <- as.data.frame(data) %>% filter_all(.,all_vars(!is.na(.))) %>% summarise_all(sum)
  all_pop <- pop$hi_wh + pop$hi_bl + pop$md_wh + pop$md_bl + pop$lo_wh + pop$lo_bl
  aux2 <- all_pop*aux1/pop[1,]
  aux2 <- aux2 %>% mutate(state = state,variable = "isol_w")
  aux1 <- c(state,"isol",aux1) %>% rbind()
  colnames(aux1) <- c("state","variable",groups)
  
  seg_group <- rbind(seg_group,aux,aux1,aux2)
  
  # Global indexes interactions between groups        
  #aux3 <- data.frame(state = state) %>%
  #mutate(DIDuncan = list(DIDuncan(x=data)),
  #DIMorrill = list(DIMorrill(x=data,c=cont,spatobj=shp)),
  #RCO = list(RCO(x=data,a=area,spatobj=shp)),
  #spatinteract = list(spatinteract(x=data,spatobj=shp)))
  
  seg_global <- seg_global %>% mutate(cell_size = size)
  seg_group <- seg_group %>% mutate_all(as.character) %>% mutate_at(-c(1,2),as.numeric) %>% mutate(cell_size = size)
  
  seg_list <- list("seg_global" = seg_global,"seg_group" = seg_group)
  
  return(seg_list)
  
}

seg_list <- tibble(size = rep(c("500","1000","2500","5000"),4), state = rep(states,each=4),metro = rep(cd_metro,each=4),cd_state = rep(st_codes,each=4)) %>% pmap(seg)

seg_global <- sapply(seg_list,function(x) x[1]) %>% do.call("rbind",.) %>% as_tibble() %>% mutate_all(as.character) %>% mutate_at(-c(1),as.numeric)# %>% mutate_at(-c(1,7),scale)
seg_group <- sapply(seg_list,function(x) x[2]) %>% do.call("rbind",.) %>% as_tibble() %>%
  gather(key="group",value="value",-c(variable,state,cell_size)) %>%
  mutate(value = as.numeric(value)) %>% group_by(variable) %>% #mutate(value = scale(value)) %>%
  spread(key = "group",value = "value")
seg_group <- seg_group[,c(3,1,2,5,4,9,8,7,6)]

write.csv(seg_group,"Outputs/CSV/seg_group_MR.csv")
write.csv(seg_global,"Outputs/CSV/seg_global_MR.csv")

#### Maps ####

# Base theme map
theme_map <- function(...){
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(color = "#ebebe5", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    legend.title = element_text(size = 12, color = "#4e4d47"),
    legend.text = element_text(size = 10, hjust = 0, color = "#4e4d47",family="A"),
    legend.background = element_rect(fill = alpha('white', 0.0)),
    ...)}

# Map zoom
zoom_x <- function(state){
  zoom_x <- case_when(state == "pr" ~ c(-49.55,-49.05),
                      state == "sp" ~ c(-47.0,-46.17),
                      state == "rj" ~ c(-43.8,-42.85),
                      state == "ce" ~ c(-38.77,-38.3))}
zoom_y <- function(state){
  zoom_y <- case_when(state == "pr" ~ c(-25.68,-25.25),
                      state == "sp" ~ c(-23.87,-23.27),
                      state == "rj" ~ c(-23.1,-22.6),
                      state == "ce" ~ c(-4.05,-3.65))}

# Scale bar zoom
bar_scale <- function(state){
  bar_scale <- case_when(state == "pr" ~ 5,
                         state == "sp" ~ 10,
                         state == "rj" ~ 7,
                         state == "ce" ~ 5)}

# Subtitle position
pos_legend <- function(state){
  pos_legend <- case_when(state == "pr" ~ c(0.73, 0.13),
                          state == "sp" ~ c(0.73, 0.13),
                          state == "rj" ~ c(0.73, 0.15),
                          state == "ce" ~ c(0.73,0.13))}

# Map for numeric variables
PlotMap <- function(base,state,variable,name_variable,population) {
  
  # Apply map definitions
  zoom_x <- zoom_x(state)
  zoom_y <- zoom_y(state)
  bar_scale <- bar_scale(state)
  pos_legend <- pos_legend(state)
  
  # Round for each type of variable (percentage or absolute)
  decimals <- case_when(mean(variable,na.rm=T) > 1 ~ -1,
                        mean(variable,na.rm=T) < 1 ~ 4)
  
  # Quantiles
  #variable_cor <- variable[!variable %in% 0]
  #breaks <- quantile(variable_cor,probs = seq(0, 1, length.out = 6), na.rm = TRUE) %>% round(decimals)
  
  # Equal intervals (considering all cities)
  #max <- max(population, na.rm = T) %>% round(decimals)
  #diff <- round(max/5,decimals)
  #breaks <- c(0,diff,diff*2,diff*3,diff*4,max)
  
  # Natural breaks (considering all cities)
  #breaks <- getJenksBreaks(population, 8, subset = NULL)
  
  # Pretty breaks - percentage of the group in each cell
  #breaks <- c(population,max(variable,na.rm = T))
  #windowsFonts(A=windowsFont("Arial Unicode MS"))
  #pm <- number_format(accuracy = 0.5,scale = 10000,suffix = '\u2031')
  #base$breaks <- cut(variable,breaks = breaks,include.lowest = TRUE,labels = breaks[2:6])
  #breaks_scale <- levels(base$breaks)
  #labels <- pm(rev(as.numeric(breaks_scale)))
  
  breaks <- c(0,0.05,0.1,0.2,0.3,0.4,0.5,0.85)
  labels <- rev(c("5%","10%","20%","30%","40%","50%","85%"))
  
  # Cut variable based on breaks
  base$breaks <- cut(variable,breaks = breaks,include.lowest = TRUE,labels = breaks[2:8])
  breaks_scale <- levels(base$breaks) %>% as.numeric()
  #labels <- case_when(mean(variable,na.rm=T) > 1 ~ as.character(rev(breaks_scale)),mean(variable,na.rm=T) < 1 ~ scales::percent(rev(breaks_scale),accuracy = 1))
  
  # Plot map
  map <- ggplot() +
    geom_sf(data = back %>% filter(DN == "0"), mapping = aes(), fill = "#f0f0f0", size = 0.3, color = "white") +
    theme_map() +
    theme(legend.position = pos_legend) + 
    geom_sf(data = base, aes(fill = breaks), color = NA)  +
    scale_fill_manual(
      values = (magma(10)[3:9]),
      breaks = rev(breaks_scale),
      name = name_variable,
      drop = FALSE,
      labels = labels,
      guide = guide_legend(
        direction = "horizontal",
        keyheight = unit(3, units = "mm"),
        keywidth = unit(11, units = "mm"),
        title.position = 'top',
        title.hjust = 0.5,
        label.hjust = 1,
        nrow = 1,
        byrow = T,
        reverse = T,
        label.position = "bottom")) + 
    geom_sf(data = mask %>% filter(DN == "1"), mapping = aes(), fill = "#f0f0f0", color = NA) +
    geom_sf(data = mask %>% filter(DN == "2"), mapping = aes(), fill = "#9ecae1", color = NA) +
    geom_sf(data = back %>% filter(DN == "2"), mapping = aes(), fill = "#9ecae1", color = NA) +
    geom_sf(data = mun, mapping = aes(), fill = "transparent", color = "white", size = 0.3) +
    geom_sf(data = mun %>% filter(CD_GEOCODM == metro), mapping = aes(), fill = "transparent", color = "white", size = 1.5) +
    geom_sf(data = transport, mapping = aes(), color = "#252525", size = 0.7) +
    coord_sf(xlim=zoom_x,ylim=zoom_y) +
    ggsn::scalebar(data = mun_city, dist = bar_scale, st.color = "#4e4d47", st.size=3, box.fill = c("#4e4d47","#f0f0f0"), box.color = "#4e4d47", border.size = 0.2, height=0.015, dist_unit = "km", location = "bottomright", model = 'WGS84',transform = TRUE, anchor = c(x=zoom_x[2], y=zoom_y[1])) +
    ggsn::north(data = mun_city, location = "topright", scale = 0.1, symbol = 4, anchor = c(x=zoom_x[2], y=zoom_y[2]))
  
  return(map)
}

#### Plot Maps ####

all <- c(paste0("Outputs/Shapefiles/",tolower(states),"_hex500_data_MR.shp")) %>%
  map_df(st_read) %>%
  filter(!is.na(pop)) %>%
  group_by()

# Choose the method of categorization inside the function (quantiles, natural breaks, equal intervals)

# General maps
for(state in states){
  
  # Data
  hex <- st_read(paste0("Outputs/Shapefiles/",tolower(state),"_hex500_data_MR.shp")) %>%
    filter(!is.na(pop))
  
  # Map elements
  mun <- st_read(paste0("Inputs/Map_Elements/",tolower(state),"_mun_MR.shp"))
  mun_city <- st_read(paste0("Inputs/Map_Elements/",tolower(state),"_mun.shp"))
  transport <- st_read(paste0("Inputs/Map_Elements/",tolower(state),"_transport_axes.shp"))
  mask <- st_read(paste0("Inputs/Land_Cover/Shapefiles/",tolower(state),"_cover_MR.shp"))
  back <- st_read("Inputs/Map_Elements/br_mun.shp")
  
  # Population map
  plot <- PlotMap(hex,tolower(state),hex$pop,"Population",all$pop)
  ggsave(paste0(tolower(state),"_pop.jpeg"),device = "jpeg",
         path = "Outputs/Plots/hex500_MR_natural_breaks_all/",dpi = 600)
}

# Groups maps
for(state in states){
  
  # Data
  hex <- st_read(paste0("Outputs/Shapefiles/",tolower(state),"_hex500_data_MR.shp"))
  
  # Map elements
  mun <- st_read(paste0("Inputs/Map_Elements/",tolower(state),"_mun_MR.shp"))
  mun_city <- st_read(paste0("Inputs/Map_Elements/",tolower(state),"_mun.shp"))
  transport <- st_read(paste0("Inputs/Map_Elements/",tolower(state),"_transport_axes.shp"))
  mask <- st_read(paste0("Inputs/Land_Cover/Shapefiles/",tolower(state),"_cover_MR.shp"))
  back <- st_read("Inputs/Map_Elements/br_mun.shp")

  metro <- case_when(state == "RJ" ~ "3304557",
                     state == "SP" ~ "3550308",
                     state == "PR" ~ "4106902",
                     state == "CE" ~ "2304400")
  
  
  # White upper class map
  plot <- PlotMap(hex,tolower(state),hex$hi_wh_w,"Upper-class white",all$hi_wh_w)
  ggsave(paste0(tolower(state),"_hi_wh_mun.jpeg"),plot,device = "jpeg",
         path = "Outputs/Plots/hex500/",dpi = 600)
  
  # Black upper class map
  #plot <- PlotMap(hex,tolower(state),hex$hi_bl_w,"Upper-class black",all$hi_bl_w)
  #ggsave(paste0(tolower(state),"_hi_bl.jpeg"),plot,device = "jpeg",
  #       path = "Outputs/Plots/hex500_MR_equal_breaks_all/",dpi = 600)
  
  # White middle class map
  #plot <- PlotMap(hex,tolower(state),hex$md_wh_w,"Middle-class white",all$md_wh_w)
  #ggsave(paste0(tolower(state),"_md_wh.jpeg"),plot,device = "jpeg",
  #       path = "Outputs/Plots/hex500_MR_equal_breaks_all/",dpi = 600)
  
  # Black middle class map
  #plot <- PlotMap(hex,tolower(state),hex$md_bl_w,"Middle-class black",all$md_bl_w)
  #ggsave(paste0(tolower(state),"_md_bl.jpeg"),plot,device = "jpeg",
  #       path = "Outputs/Plots/hex500_MR_equal_breaks_all/",dpi = 600)
  
  # White lower class map
  #plot <- PlotMap(hex,tolower(state),hex$lo_wh_w,"Lower-class white",all$lo_wh_w)
  #ggsave(paste0(tolower(state),"_lo_wh.jpeg"),plot,device = "jpeg",
  #       path = "Outputs/Plots/hex500_MR_equal_breaks_all/",dpi = 600)
  
  # Black lower class map
  plot <- PlotMap(hex,tolower(state),hex$lo_bl_w,"Lower-class black",all$lo_bl_w)
  ggsave(paste0(tolower(state),"_lo_bl_mun.jpeg"),plot,device = "jpeg",
         path = "Outputs/Plots/hex500/",dpi = 600)
  
}



