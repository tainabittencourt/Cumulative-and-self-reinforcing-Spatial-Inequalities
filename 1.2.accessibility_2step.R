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
library(tidytransit)
library(chron)

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

states <- c("PR","RJ","SP","BA","CE")[-4]

st_codes <- c("41","33","35","29","23")[-4]

cd_metro <- c("4106902","3304557","3550308","2927408","2304400")[-4]

crs_utm <- c("+proj=utm +zone=22 +datum=WGS84 +units=m +ellps=WGS84 +towgs84=0,0,0",
             "+proj=utm +zone=23 +datum=WGS84 +units=m +ellps=WGS84 +towgs84=0,0,0",
             "+proj=utm +zone=23 +datum=WGS84 +units=m +ellps=WGS84 +towgs84=0,0,0",
             "+proj=utm +zone=24 +datum=WGS84 +units=m +ellps=WGS84 +towgs84=0,0,0",
             "+proj=utm +zone=23 +datum=WGS84 +units=m +ellps=WGS84 +towgs84=0,0,0")[-4]
setwd("D:/Drive - Taina/Doc/Analysis/")

#### Employement statistics ####

employment <- NULL

for(city in cities){
  
  #### Summary of employment data ####
  
  # Auxiliary codes
  st_name <- nm_state(city)
  code <- cd_metro(city)
  
  data <- read.delim(paste0("Inputs/RAIS/",state,"2017.txt"), sep = ";",colClasses = "character") %>%
    filter(Município == code) %>% mutate_all(as.numeric) %>%
    filter(Vínculo.Ativo.31.12 == 1) %>%
    mutate(sex = case_when(Sexo.Trabalhador == 1 ~ "men",
                           Sexo.Trabalhador == 2 ~ "women"),
           color = case_when(Raça.Cor %in% c(2,6) ~ "white",
                             Raça.Cor %in% c(1,4,8) ~ "black"),
           income = Faixa.Remun.Média..SM.) %>%
    group_by(income) %>%
    summarise(people = n()) %>%
    mutate(prop = people/sum(people)) %>%
    mutate(mun = city)
  
  employment <- rbind(employment,data)
}

employment


#### Jobs ####

## Curitiba
state <- states[1]
metro <- cd_metro[1]
cd_state <- st_codes[1]

cep <- read.csv2(paste0("Inputs/RAIS/",state,"_cep.csv")) %>%
  mutate(CEP.Estab = paste0(substr(CEP,1,5),substr(CEP,7,9))) %>%
  mutate_at(c("LAT","LON"),as.character) %>% mutate_at(c("LAT","LON"),as.numeric)
coordinates(cep)<-~LON+LAT
crs(cep) <- "+proj=longlat +datum=WGS84 +no_defs"
cep <- cep %>% st_as_sf() %>% st_join(st_read(paste0("Inputs/Census_Shapefiles/",cd_state,"MUE250GC_SIR.shp")) %>%
  filter(CD_GEOCODM==metro) %>% st_transform(4326),by="st_intersects")

emp <- read.delim("Inputs/RAIS/ESTB2017.txt", sep = ";",colClasses = "character") %>%
  filter(Município==substr(metro,1,6))

jobs <- cep %>%
  left_join(emp, by = "CEP.Estab") %>%
  mutate(Qtd.Vínculos.Ativos = as.numeric(Qtd.Vínculos.Ativos)) %>%
  filter(Qtd.Vínculos.Ativos != 0)

# Corrections (first 3 removed from database)
# 1. Secretaria do Estado da Educacao: 86.231 vinculos
# 2. Centro Civico/R. Dep. Mario de Andrade: 31.432 vinculos
# 3. Centro Civico/Av. Candido de Abreu: 29.793 vinculos
# Demais < 10.000 vinculos
# Remove outliers

jobs <- jobs %>% arrange(desc(Qtd.Vínculos.Ativos)) %>% tail(-3) %>%
  group_by(CEP) %>%
  summarise(jobs = sum(Qtd.Vínculos.Ativos))

# Detected outliers
jobs <- jobs %>% filter(CEP != "81945-020")

sum(jobs$jobs)
sum(as.numeric(emp$Qtd.Vínculos.Ativos))

st_write(jobs,paste0("Inputs/RAIS/",tolower(state),"_jobs.shp"),delete_layer = T)

## Rio de Janeiro
state <- states[2]
metro <- cd_metro[2]
cd_state <- st_codes[2]

cep <- read.csv2(paste0("Inputs/RAIS/",state,"_cep.csv")) %>%
  mutate(CEP.Estab = paste0(substr(CEP,1,5),substr(CEP,7,9))) %>%
  mutate_at(c("LAT","LON"),as.character) %>% mutate_at(c("LAT","LON"),as.numeric)
coordinates(cep)<-~LON+LAT
crs(cep) <- "+proj=longlat +datum=WGS84 +no_defs"
cep <- cep %>% st_as_sf() %>% st_join(st_read(paste0("Inputs/Census_Shapefiles/",cd_state,"MUE250GC_SIR.shp")) %>%
                                        filter(CD_GEOCODM==metro) %>% st_transform(4326),by="st_intersects")
emp <- read.delim("Inputs/RAIS/ESTB2017.txt", sep = ";",colClasses = "character") %>%
  filter(Município==substr(metro,1,6))

jobs <- cep %>%
  left_join(emp, by = "CEP.Estab") %>%
  mutate(Qtd.Vínculos.Ativos = as.numeric(Qtd.Vínculos.Ativos)) %>%
  filter(Qtd.Vínculos.Ativos != 0)

# Corrections (first 5 removed from database)
# 1. Prefeitura Municipal do Rio de Janeiro: 85.506 vinculos
# 2. Secretaria do Estado de Educacao: 77.972 vinculos
# 3. Quartel General da Policia Militar: 43.932 vinculos
# 4. IBGE/R. Roosevelt: 23.080 vinculos
# 5. Companhia Municipal de Limpeza Urbana: 19.688 vinculos
# 6. R. Mexico/MPF: 17.606 vinculos
# 7. Praca XV de Novembro/Ibama,Receita Federal, Assembleia Legislativa: 14.984 vinculos
# 8. Praca da Republica/Tribunal de Contas: 13.717 vinculos
# 9. R. Mexico/MPF: 11.288 vinculos
# 10. UFRJ: 10.485 vinculos
# Demais < 10.000 vinculos

jobs <- jobs %>% arrange(desc(Qtd.Vínculos.Ativos)) %>% tail(-5) %>%
  group_by(CEP) %>%
  summarise(jobs = sum(Qtd.Vínculos.Ativos))

# Detected outliers
jobs <- jobs %>% filter(!CEP %in% c("23565-200","22775-001","23020-255","23565-160","23565-235","23565-190"))

sum(jobs$jobs)
sum(as.numeric(emp$Qtd.Vínculos.Ativos))

st_write(jobs,paste0("Inputs/RAIS/",tolower(state),"_jobs.shp"),delete_layer=T)

## Sao Paulo
state <- states[3]
metro <- cd_metro[3]
cd_state <- st_codes[3]

cep <- read.csv2(paste0("Inputs/RAIS/",state,"_cep.csv")) %>%
  mutate(CEP.Estab = paste0(substr(CEP,1,5),substr(CEP,7,9))) %>%
  mutate_at(c("LAT","LON"),as.character) %>% mutate_at(c("LAT","LON"),as.numeric)
coordinates(cep)<-~LON+LAT
crs(cep) <- "+proj=longlat +datum=WGS84 +no_defs"
cep <- cep %>% st_as_sf() %>% st_join(st_read(paste0("Inputs/Census_Shapefiles/",cd_state,"MUE250GC_SIR.shp")) %>%
                                        filter(CD_GEOCODM==metro) %>% st_transform(4326),by="st_intersects") %>%
  filter(!is.na(CD_GEOCODM))

emp <- read.delim("Inputs/RAIS/ESTB2017.txt", sep = ";",colClasses = "character") %>%
  filter(Município==substr(metro,1,6))

jobs <- cep %>%
  mutate(CEP.Estab = as.numeric(CEP.Estab)) %>%
  left_join(emp %>% mutate(CEP.Estab = as.numeric(CEP.Estab)), by = "CEP.Estab") %>%
  mutate(Qtd.Vínculos.Ativos = as.numeric(Qtd.Vínculos.Ativos)) %>%
  filter(Qtd.Vínculos.Ativos != 0)

# Corrections (first 5 removed from database)
# 1. Rodoviária Tiete: 86.062 vinculos
# 2. R. Boa Vista/Defensoria Publica/CDHU: 81.809 vinculos
# 3. OAB/Forum: 45.173 vinculos
# 4. Av. Dr. Arnaldo/Clinicas: 39.122 vinculos
# 5. R. Boa Vista/Defensoria Publica/CDHU: 21.922 vinculos
# 6. R. dos Andradas: 18.831 vinculos
# 7. Av. Pres. Juscelino Kubitschek: 14.437 vinculos
# 8. Avenida Albert Einstein: 13.464 vinculos
# 9. Av. Tiradentes: 	10.571 vinculos
# 10. Av. Paulista: 10.285 vinculos
# Demais < 10.000 vinculos

jobs <- jobs %>% arrange(desc(Qtd.Vínculos.Ativos)) %>% tail(-5) %>%
  group_by(CEP) %>%
  summarise(jobs = sum(Qtd.Vínculos.Ativos))

sum(jobs$jobs)
sum(as.numeric(emp$Qtd.Vínculos.Ativos))

jobs_t <- jobs %>% as("Spatial") %>% st_as_sf()

st_write(jobs_t,paste0("Inputs/RAIS/",tolower(state),"_jobs.shp"))

## Fortaleza
state <- states[4]
metro <- cd_metro[4]
cd_state <- st_codes[4]

cep <- read.csv(paste0("Inputs/RAIS/",state,"_cep.csv")) %>%
  mutate(CEP.Estab = paste0(substr(CEP,1,5),substr(CEP,7,9))) %>%
  mutate_at(c("LAT","LON"),as.character) %>% mutate_at(c("LAT","LON"),as.numeric) %>%
  filter(!is.na(LAT))
coordinates(cep)<-~LON+LAT
crs(cep) <- "+proj=longlat +datum=WGS84 +no_defs"
cep <- cep %>% st_as_sf() %>% st_join(st_read(paste0("Inputs/Census_Shapefiles/",cd_state,"MUE250GC_SIR.shp")) %>%
                                        filter(CD_GEOCODM==metro) %>% st_transform(4326),by="st_intersects") %>%
  filter(!is.na(CD_GEOCODM))

emp <- read.delim("Inputs/RAIS/ESTB2017.txt", sep = ";",colClasses = "character") %>%
  filter(Município==substr(metro,1,6))

jobs <- cep %>%
  left_join(emp, by = "CEP.Estab") %>%
  mutate(Qtd.Vínculos.Ativos = as.numeric(Qtd.Vínculos.Ativos)) %>%
  filter(Qtd.Vínculos.Ativos != 0) %>%
  filter(CEP != "60000-000")

# Corrections
# All < 14.000 vinculos

jobs <- jobs %>% arrange(desc(Qtd.Vínculos.Ativos)) %>% tail(-3) %>%
  group_by(CEP) %>%
  summarise(jobs = sum(Qtd.Vínculos.Ativos))

sum(jobs$jobs)
sum(as.numeric(emp$Qtd.Vínculos.Ativos))

st_write(jobs,paste0("Inputs/RAIS/",tolower(state),"_jobs.shp"))

#### Intersect jobs and grid ####

jobs_grid <- function(state,metro,cd_state,size){
  
  hex <- st_read(paste0("Inputs/Grids/",state,"_hex",size,"_MR.shp")) %>%
    st_join(st_read(paste0("Inputs/Census_Shapefiles/",cd_state,"MUE250GC_SIR.shp")) %>%
              filter(CD_GEOCODM==metro) %>% st_transform(4326),by="st_intersects") %>%
    st_join(st_read(paste0("Inputs/RAIS/",tolower(state),"_jobs.shp")),by = "st_contains") %>%
    mutate(jobs = as.numeric(jobs)) %>%
    select(GEO_ID,jobs) %>%
    group_by(GEO_ID) %>%
    summarise(jobs = sum(jobs,na.rm=T)) %>%
    mutate(jobs=ifelse(is.na(jobs),0,jobs)) %>%
    st_drop_geometry() %>%
    mutate(GEO_ID = as.character(GEO_ID))
  
  hex <- st_read(paste0("Outputs/Shapefiles/",tolower(state),"_hex",size,"_data_MR.shp")) %>%
    select(GEO_ID,hi_wh,hi_bl,md_wh,md_bl,lo_wh,lo_bl,pop,income,white,black,hi_wh_w,hi_bl_w,md_wh_w,md_bl_w,lo_wh_w,lo_bl_w,CD_GEOCODM) %>%
    mutate(GEO_ID = as.character(GEO_ID)) %>%
    left_join(hex,by="GEO_ID")
  
  st_write(hex,paste0("Outputs/Shapefiles/",tolower(state),"_hex",size,"_data_MR.shp"),delete_layer = T)
  
  #sum(hex500$jobs,na.rm=T)
  #sum(emp$QtdVncA,na.rm=T)
  #hex <- hex %>% filter(jobs != 0)
}

tibble(state = rep(states,each = 4),cd_state = rep(st_codes,each=4),metro = rep(cd_metro,each=4),size=rep(c(500,1000,2500,5000),4)) %>% pmap(jobs_grid)

#### Centroids to OTP ####

read.csv2("C:/Users/taina.bittencourt/Documents/SP/destinations.csv") %>%
  write.csv("C:/Users/taina.bittencourt/Documents/SP/destinations.csv")


OTP_centroids <- function(state,cd_state,metro,size){
  st_read(paste0("Outputs/Shapefiles/",tolower(state),"_hex",size,"_data_MR.shp")) %>%
    filter(CD_GEOCODM == metro) %>%
    st_centroid() %>%
    mutate(coords = geometry) %>%
    unnest(coords) %>% group_by(GEO_ID) %>%
    mutate(key = c("X","Y")) %>%
    spread(key,coords) %>% ungroup() %>%
    select(GEO_ID,X,Y) %>%
    st_drop_geometry() %>%
    write.csv(paste0("OTP/",state,"/",tolower(state),"_od_",size,".csv"))
}

tibble(state = rep(states[4],each=4), cd_state = rep(st_codes[4],each=4), metro = rep(cd_metro[4],each=4),size = c(500,1000,2500,5000)) %>% pmap(OTP_centroids)


#### Accessibility levels ####

test <- st_read("Outputs/Shapefiles/pr_hex500_access_2step.shp")

access_levels <- function(state,cd_state,metro,size){
  
  # Join travel time matrix with jobs database by destination
  hex <- read.csv(paste0("OTP/",toupper(state),"/",state,"_traveltime_matrix_",size,".csv")) %>%
    mutate(GEO_ID_i = as.character(origin), GEO_ID_j = as.character(destination)) %>%
    left_join(st_read(paste0("Outputs/Shapefiles/",tolower(state),"_hex",size,"_data_MR.shp")) %>%
                st_drop_geometry() %>%
                mutate(GEO_ID_i = GEO_ID,
                       workers = hi_wh+hi_bl+md_wh+md_bl+lo_wh+lo_bl) %>%
                select(GEO_ID_i,workers), by="GEO_ID_i") %>%
    left_join(st_read(paste0("Outputs/Shapefiles/",tolower(state),"_hex",size,"_data_MR.shp")) %>%
                st_drop_geometry() %>%
                mutate(GEO_ID_j = GEO_ID) %>%
                select(CD_GEOCODM,GEO_ID_j,jobs), by="GEO_ID_j") %>%
    filter(CD_GEOCODM == metro) %>%
    mutate(travel_time = travel_time / 60, walk_distance = walk_distance / 1000)
  
  #n_jobs <- hex %>% group_by(GEO_ID_j) %>% summarise(jobs = mean(jobs, na.rm = T)) %>% ungroup()
  #n_jobs <- sum(n_jobs$jobs,na.rm=T)
  
  # Step 1 - destinations
  hex_dest <- hex %>%
    filter(jobs >= 1) %>%
    mutate(jobs_30 = ifelse(travel_time <= 30,workers,0),
           jobs_45 = ifelse(travel_time <= 45,workers,0),
           jobs_60 = ifelse(travel_time <= 60,workers,0),
           jobs_90 = ifelse(travel_time <= 90,workers,0),
           jobs_120 = ifelse(travel_time <= 120,workers,0)) %>%
    group_by(GEO_ID_j) %>%
    summarise(jobs = median(jobs,na.rm=T),
              jobs_30 = sum(jobs_30,na.rm=T),
              jobs_45 = sum(jobs_45,na.rm = T),
              jobs_60 = sum(jobs_60,na.rm=T),
              jobs_90 = sum(jobs_90,na.rm=T),
              jobs_120 = sum(jobs_120,na.rm=T)) %>%
  mutate_at(c("jobs_30","jobs_45","jobs_60","jobs_90","jobs_120"),~(p = jobs/(.)))
  
  # Step 2 - origins
  hex_orig <- hex %>% select(-jobs) %>%
    filter(workers >= 1) %>%
    left_join(hex_dest,by="GEO_ID_j") %>%
    mutate(jobs_30 = ifelse(travel_time <= 30,jobs_30,0),
           jobs_45 = ifelse(travel_time <= 45,jobs_45,0),
           jobs_60 = ifelse(travel_time <= 60,jobs_60,0),
           jobs_90 = ifelse(travel_time <= 90,jobs_90,0),
           jobs_120 = ifelse(travel_time <= 120,jobs_120,0)) %>%
    group_by(GEO_ID_i) %>%
    summarise_at(c("jobs_30","jobs_45","jobs_60","jobs_90","jobs_120"),~sum(.,na.rm = T))
  
  # Join with socioeconomic data
  hex <- st_read(paste0("Outputs/Shapefiles/",state,"_hex",size,"_data_MR.shp")) %>%
    mutate(GEO_ID_i=GEO_ID) %>%
    left_join(hex_orig, by = "GEO_ID_i") %>%
    select(-GEO_ID_i)
  
  st_write(hex,paste0("Outputs/Shapefiles/",tolower(state),"_hex",size,"_access_2step.shp"), delete_layer = TRUE)
}

tibble(state = rep(states,each = 4),cd_state = rep(st_codes,each=4),metro = rep(cd_metro,each=4),size=rep(c(500,1000,2500,5000),4)) %>% pmap(access_levels)


#### Medians ####

access_medians <- function(state,size){
  
  access <- st_read(paste0("Outputs/Shapefiles/",tolower(state),"_hex",size,"_access_2step.shp")) %>%
    mutate_at(c("hi_wh","hi_bl","md_wh","md_bl","lo_wh","lo_bl"),~round(.,digits = 0)) %>%
    st_set_geometry(NULL) %>%
    filter(!is.na(jobs_30))
  
  ac_30 <- access %>%
    summarise_at(c("hi_wh","hi_bl","md_wh","md_bl","lo_wh","lo_bl"),~weightedMedian(jobs_30,.,na.rm=T)) %>%
    mutate(time = "30")
  ac_45 <- access %>%
    summarise_at(c("hi_wh","hi_bl","md_wh","md_bl","lo_wh","lo_bl"),~weightedMedian(jobs_45,.,na.rm=T)) %>%
    mutate(time = "45")
  ac_60 <- access %>%
    summarise_at(c("hi_wh","hi_bl","md_wh","md_bl","lo_wh","lo_bl"),~weightedMedian(jobs_60,.,na.rm=T)) %>%
    mutate(time = "60")
  ac_90 <- access %>%
    summarise_at(c("hi_wh","hi_bl","md_wh","md_bl","lo_wh","lo_bl"),~weightedMedian(jobs_90,.,na.rm=T)) %>%
    mutate(time = "90")
  ac_120 <- access %>%
    summarise_at(c("hi_wh","hi_bl","md_wh","md_bl","lo_wh","lo_bl"),~weightedMedian(jobs_120,.,na.rm=T)) %>%
    mutate(time = "120")
  
  medians <- rbind(ac_30,ac_45,ac_60,ac_90,ac_120) %>% mutate(cell_size = size,state = state)
  
}

tibble(state = rep(states,each = 4), size = rep(c("500","1000","2500","5000"),4)) %>% pmap_df(access_medians) %>% write_csv("Outputs/CSV/acess_medians_2step.csv")

#### Median Graphs ####

medians <- read.csv("Outputs/CSV/acess_medians_2step.csv")%>%
  mutate(state = case_when(state == "SP" ~ "Sao Paulo",
                           state == "PR" ~ "Curitiba",
                           state == "RJ" ~ "Rio de Janeiro",
                           state == "CE" ~ "Fortaleza")) %>%
  mutate(state = factor(state,levels=c("Sao Paulo","Rio de Janeiro","Curitiba","Fortaleza"))) %>%
  mutate(cell_size = as.character(cell_size))


colnames(medians) <- c("Upper-White","Upper-Black","Middle-White","Middle-Black","Lower-White","Lower-Black","Time","Size","City")


medians %>%
  gather(key="Group",value="Accessibility",-c("City","Time","Size")) %>%
  group_by(Group,Time,City) %>%
  mutate(ac_min = min(Accessibility),
         ac_max = max(Accessibility)) %>%
  ungroup() %>%
  filter(Size == "500") %>%
  filter(Time == 45) %>%
  mutate(Group = factor(Group,levels=c("Upper-White","Upper-Black","Middle-White","Middle-Black","Lower-White","Lower-Black"))) %>%
  ggplot(aes(x=Group,y=Accessibility,group=City)) +
  geom_errorbar(aes(x=Group,ymin=ac_min,ymax=ac_max,color=City),width=0.15,alpha=0.6)+
  geom_point(aes(color=City)) +
  geom_line(aes(color=City,group=City)) +
  scale_color_manual(values = magma(6)[2:5]) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ylim(c(0.20,1.2))

ggsave("med_access_120_hex500.jpeg",device = "jpeg",path = "Outputs/Graphics",dpi = 600)


read.csv("Outputs/CSV/acess_medians_2step.csv") %>%
  mutate(amp = hi_wh - lo_bl,
         dif = (hi_wh - hi_bl) + (hi_bl - md_wh) + (md_wh - md_bl) + (md_bl - lo_wh) + (lo_wh - lo_bl)) %>%
  group_by(state,cell_size) %>%
  filter(amp == max(amp))

#### Ginis ####
access_ginis <- function(state,size){
  
  gini <-  st_read(paste0("Outputs/Shapefiles/",tolower(state),"_hex",size,"_access_2step.shp")) %>%
    mutate_at(c("hi_wh","hi_bl","md_wh","md_bl","lo_wh","lo_bl","pop"),~round(.,digits = 0)) %>%
    st_set_geometry(NULL) %>%
    filter(!is.na(jobs_30_p)) %>%
    filter(pop != 0) %>%
    mutate(pop = round(pop,0)) %>%
    expandRows("pop")
  rel_gini <- gini %>% summarise_at(c("jobs_30","jobs_45","jobs_60","jobs_90","jobs_120"),~(p = ineq::Gini(.))) %>% mutate(variable = "rel_gini")
  abs_gini <- gini %>% summarise_at(c("jobs_30","jobs_45","jobs_60","jobs_90","jobs_120"),~(p = ineq::Gini(.)*mean(.,na.rm=T))) %>% mutate(variable = "abs_gini")
  abs_gini_p <- gini %>% summarise_at(c("jobs_30_p","jobs_45_p","jobs_60_p","jobs_90_p","jobs_120_p"),~(p = ineq::Gini(.)*mean(.,na.rm=T))) %>% mutate(variable = "abs_gini_p")
  colnames(abs_gini_p) <-c("jobs_30","jobs_45","jobs_60","jobs_90","jobs_120","variable")
  gini <- rbind(rel_gini,abs_gini,abs_gini_p) %>% as.data.frame() %>% mutate(cell_size = size,state = state)
  return(gini)
  }

tibble(state = rep(states,each = 4), size = rep(c("500","1000","2500","5000"),4)) %>% pmap_df(access_ginis) %>% write_csv("Outputs/CSV/access_gini_2step.csv")


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
                      state == "ba" ~ c(-38.63,-38.05),
                      state == "ce" ~ c(-38.77,-38.3))}
zoom_y <- function(state){
  zoom_y <- case_when(state == "pr" ~ c(-25.68,-25.25),
                      state == "sp" ~ c(-23.87,-23.27),
                      state == "rj" ~ c(-23.1,-22.6),
                      state == "ba" ~ c(-13.01,-12.6),
                      state == "ce" ~ c(-4.05,-3.65))}

# Scale bar zoom
bar_scale <- function(state){
  bar_scale <- case_when(state == "pr" ~ 5,
                         state == "sp" ~ 10,
                         state == "rj" ~ 7,
                         state == "ba" ~ 5,
                         state == "ce" ~ 5)}

# Subtitle position
pos_legend <- function(state){
  pos_legend <- case_when(state == "pr" ~ c(0.73, 0.13),
                          state == "sp" ~ c(0.73, 0.13),
                          state == "rj" ~ c(0.73, 0.15),
                          state == "ba" ~ c(0.73,0.13),
                          state == "ce" ~ c(0.73,0.13))}

pos_legend2 <- function(state){
  pos_legend <- case_when(state == "pr" ~ c(0.79, 0.13),
                          state == "sp" ~ c(0.79, 0.13),
                          state == "rj" ~ c(0.79, 0.15),
                          state == "ba" ~ c(0.79,0.13),
                          state == "ce" ~ c(0.79,0.13))}

# Map for numeric variables
PlotMap <- function(base,state,variable,name_variable) {
  
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
  #breaks <- getJenksBreaks(population, 6, subset = NULL)
  
  # Pretty breaks - percentage of the group in each cell
  #breaks <- c(population,max(variable,na.rm = T))
  #windowsFonts(A=windowsFont("Arial Unicode MS"))
  #pm <- number_format(accuracy = 0.5,scale = 10000,suffix = '\u2031')
  #base$breaks <- cut(variable,breaks = breaks,include.lowest = TRUE,labels = breaks[2:6])
  #breaks_scale <- levels(base$breaks)
  #labels <- pm(rev(as.numeric(breaks_scale)))
  
  breaks <- c(0,0.2,0.4,0.6,0.8,1,1.5,100)
  #breaks <- c(0,100,300,500,700,1000,1500,3500)*1000
  #breaks <- c(0,0.05,0.15,0.30,0.50,0.65,0.75,1)
  
  # Cut variable based on breaks
  base$breaks <- cut(variable,breaks = breaks,include.lowest = TRUE,labels = breaks[2:8])
  breaks_scale <- levels(base$breaks) %>% as.numeric()
  #labels <-c(breaks[2:7],"++")
  #labels <- breaks[-1]/1000
  #labels <- c("5%","15%","30%","50%","65%","75%","100%")
  labels <- c("0.2","0.4","0.6","0.8",'1.0',"1.5","++")
  
  # Plot map
  map <- ggplot() +
    geom_sf(data = back %>% filter(DN == "0"), mapping = aes(), fill = "#f0f0f0", size = 0.3, color = "white") +
    theme_map() +
    theme(legend.position = pos_legend) + 
    geom_sf(data = hex,mapping=aes(),fill = "#C5C5C5",color = NA) +
    geom_sf(data = base, aes(fill = breaks), color = NA)  +
    scale_fill_manual(
      values = rev(magma(10)[3:9]),
      breaks = rev(breaks_scale),
      name = name_variable,
      drop = FALSE,
      labels = rev(labels),
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
    geom_sf(data = transport %>% filter(local == "mun"), mapping = aes(), color = "#252525", size = 0.7) +
    geom_sf(data = transport %>% filter(local == "metro"), mapping = aes(), color = "#606060", size = 0.7,linetype = "dashed") +
    coord_sf(xlim=zoom_x,ylim=zoom_y) +
    ggsn::scalebar(data = mun_city, dist = bar_scale, st.color = "#4e4d47", st.size=3, box.fill = c("#4e4d47","#f0f0f0"), box.color = "#4e4d47", border.size = 0.2, height=0.015, dist_unit = "km", location = "bottomright", model = 'WGS84',transform = TRUE, anchor = c(x=zoom_x[2], y=zoom_y[1])) +
    ggsn::north(data = mun_city, location = "topright", scale = 0.1, symbol = 4, anchor = c(x=zoom_x[2], y=zoom_y[2]))
  
  return(map)
}

# Map for routes frequencies
PlotMap2 <- function(base,state,variable,name_variable) {
  
  breaks <- c(0,1,2,3,4)
  
  base$breaks <- cut(variable,breaks = breaks,include.lowest = TRUE,labels=c("H-H","L-L","L-H","H-L"))
  breaks_scale <- levels(base$breaks)
  labels <- c("H-H","L-L","L-H","H-L")
  
    # Apply map definitions
  zoom_x <- zoom_x(state)
  zoom_y <- zoom_y(state)
  bar_scale <- bar_scale(state)
  pos_legend <- pos_legend2(state)
  
  
  # Plot map
  map <- ggplot() +
    geom_sf(data = back %>% filter(DN == "0"), mapping = aes(), fill = "#f0f0f0", size = 0.3, color = "white") +
    theme_map() +
    theme(legend.position = pos_legend) + 
    geom_sf(data = hex,mapping=aes(),fill = "#C5C5C5",color = NA) +
    geom_sf(data = base, aes(fill = breaks), color = NA)  +
    scale_fill_manual(
      values = rev(c("#f46d44","#539F88","#acdda5","#fdaf61")),
      breaks = rev(breaks_scale),
      name = name_variable,
      labels = rev(labels),
      drop = FALSE,
      guide = guide_legend(
        direction = "horizontal",
        keyheight = unit(3, units = "mm"),
        keywidth = unit(15, units = "mm"),
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
    geom_sf(data = transport %>% filter(local == "mun"), mapping = aes(), color = "#252525", size = 0.7) +
    geom_sf(data = transport %>% filter(local == "metro"), mapping = aes(), color = "#606060", size = 0.7,linetype = "dashed") +
    coord_sf(xlim=zoom_x,ylim=zoom_y) +
    ggsn::scalebar(data = mun_city, dist = bar_scale, st.color = "#4e4d47", st.size=3, box.fill = c("#4e4d47","#f0f0f0"), box.color = "#4e4d47", border.size = 0.2, height=0.015, dist_unit = "km", location = "bottomright", model = 'WGS84',transform = TRUE, anchor = c(x=zoom_x[2], y=zoom_y[1])) +
    ggsn::north(data = mun_city, location = "topright", scale = 0.1, symbol = 4, anchor = c(x=zoom_x[2], y=zoom_y[2]))
  
  return(map)
}

#### Plot Maps ####

for(state in states){
  
  metro <- case_when(state == "PR" ~ cd_metro[1],
                     state == "SP" ~ cd_metro[3],
                     state == "RJ" ~ cd_metro[2],
                     state == "CE" ~ cd_metro[4])
  
  # Data
  hex <- st_read(paste0("Outputs/Shapefiles/",tolower(state),"_hex500_access_2step.shp"))
  
  # Map elements
  mun <- st_read(paste0("Inputs/Map_Elements/",tolower(state),"_mun_MR.shp"))
  mun_city <- st_read(paste0("Inputs/Map_Elements/",tolower(state),"_mun.shp"))
  transport <- st_read(paste0("Inputs/Map_Elements/",tolower(state),"_transport_axes.shp"))
  mask <- st_read(paste0("Inputs/Land_Cover/Shapefiles/",tolower(state),"_cover_MR.shp"))
  back <- st_read("Inputs/Map_Elements/br_mun.shp")
  
  hex_metro <- hex %>% filter(CD_GEOCODM == metro)
  
  # Fixed times
  
  plot <- PlotMap(hex_metro,tolower(state),hex_metro$jobs_45,"Accessibility 45min")
  ggsave(paste0(tolower(state),"_ac_45_2step.jpeg"),plot,device = "jpeg",path = "Outputs/Plots/hex500_access",dpi = 600, height=7,width=7)
  
  hex_metro <- st_read(paste0("Outputs/Shapefiles/",tolower(state),"_hex500_access_2step.shp")) %>%
    filter(CD_GEOCODM == metro) %>% mutate(lo_class = lo_wh + lo_bl) %>%
    filter(lo_class > 0) %>%
    mutate(access = jobs_45) %>%
    filter(access > 0) %>%
    mutate(LISA_CL = case_when(lo_class > median(lo_class) & access > median(access) ~ 1,
                               lo_class < median(lo_class) & access < median(access) ~ 2,
                               lo_class < median(lo_class) & access > median(access) ~ 3,
                               lo_class > median(lo_class) & access < median(access) ~ 4)) %>%
    filter(LISA_CL %in% c("1","2","3","4"))
  
  # LISA
  #plot <- PlotMap2(hex_metro,tolower(state),hex_metro$LISA_CL,"Low-class population | Accessibility")
  #ggsave(paste0(tolower(state),"_ac_low_biv_2step.jpeg"),plot,device = "jpeg",path = "Outputs/Plots/hex500_access",dpi = 600,height=7,width=7)
  
  
}

#### Small corrections ####
#st_read("Outputs/Shapefiles/pr_hex500_access.shp") %>% filter(GEO_ID != "15601") %>% st_write("Outputs/Shapefiles/pr_hex500_access.shp",delete_layer=T)
#st_read("Outputs/Shapefiles/rj_hex500_access.shp") %>% filter(GEO_ID != "20120") %>% st_write("Outputs/Shapefiles/rj_hex500_access.shp",delete_layer=T)
#st_read("Outputs/Shapefiles/sp_hex500_access.shp") %>% filter(GEO_ID != "28508") %>% st_write("Outputs/Shapefiles/sp_hex500_access.shp",delete_layer=T)
#st_read("Outputs/Shapefiles/ce_hex500_access.shp") %>% filter(GEO_ID != "9936") %>% st_write("Outputs/Shapefiles/ce_hex500_access.shp",delete_layer=T)


#### Comparação de Medianas ####

Mediantest <- function(st,vr,sz,gr){
  
  hex <- st_read(paste0("Outputs/Shapefiles/",tolower(st),"_hex",sz,"_access.shp")) %>% st_drop_geometry()
  
  hex <- hex %>% select(gr,vr)
  colnames(hex) <- c("pop","var")
  hex <- rep(hex$var,hex$pop) %>% na.omit()
  
  median <- MedianCI(hex,sides = "two.sided",na.rm = T,method="boot",R=1000)
  median <- c(st,sz,gr,median)
  return(median)}

tibble(st = rep(rep(c("SP","RJ","PR","CE"),each=24),5),
                         vr = rep(c("jobs_30","jobs_45","jobs_60","jobs_90","jobs_120"),each=24*4),
                         sz = rep(rep(c("500","1000","2500","5000"),each=6),20),
                         gr = rep(c("hi_wh","hi_bl","md_wh","md_bl","lo_wh","lo_bl"),16*5)) %>%
  pmap_df(Mediantest) %>% write.csv("Outputs/CSV/medians_access.csv")


#### Stats ####

state <- "CE"

hex <- st_read(paste0("Outputs/Shapefiles/",tolower(state),"_hex500_access_2step.shp")) %>% st_drop_geometry()
breaks <- c(0,0.2,0.4,0.6,0.8,1,1.5,100)
hex$breaks <- cut(hex$jobs_45,breaks = breaks,include.lowest = TRUE,labels = breaks[2:8])
hex <- hex %>% mutate(workers = hi_wh+hi_bl+md_wh+md_bl+lo_wh+lo_bl) %>%
  filter(jobs_45>0)


hex %>% group_by(breaks) %>% summarise(workers = sum(workers)) %>%
  mutate(perc = 100*workers/sum(workers))
