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

#### Downloads ####

## Census sample

# Links to access the data from IBGE website and to store it
states2 <- c(states[states!="SP"],"SP2_RM")[1]
ibge_url <- "ftp://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_Gerais_da_Amostra/Microdados/"
ibge_dir <- "Inputs/Census_Sample/Microdados/"
cod_ibge <- c(paste0(states2,".zip"))

# Download files
tibble(url = paste0(ibge_url,cod_ibge),
       destfile = paste0(ibge_dir,states2,".zip")) %>%
  pmap(download.file)

# Unzip files
tibble(zipfile = paste0(ibge_dir,states2,".zip"),
       exdir = substr(ibge_dir,1,nchar(ibge_dir)-1)) %>%
  pmap(unzip)

# Download documentation file
download.file(url = paste0(ibge_url,"Documentacao.zip"),destfile = "Inputs/Information/Census_Sample.zip")

# Move files from census sample - standadise pattern (we downloaded just the metro region of SP)
file.rename(paste0(ibge_dir,"SP2-RM"),paste0(ibge_dir,"SP"))

## Census universe

# Links to access the data from IBGE website and to store it
states2 <- c(states[states!="SP"],"SP_Estado","SP_Capital")[1]
ibge_url <- "ftp://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_do_Universo/Agregados_por_Setores_Censitarios/"
ibge_dir <- "Inputs/Census_Universe/"
cod_ibge <- c(paste0(states,"_20171016.zip")[-3],"SP_Exceto_a_Capital_20190207.zip","SP_Capital_20190823.zip")[1]

# Download files
tibble(url=paste0(ibge_url,cod_ibge),
       destfile=paste0(ibge_dir,states2,".zip")) %>%
  pmap(download.file)

# Unzip files
tibble(zipfile=paste0(ibge_dir,states2,".zip"),
       exdir=substr(ibge_dir,1,nchar(ibge_dir)-1)) %>%
  pmap(unzip)

# Download documentation file
download.file(url = paste0(ibge_url,"Documentacao_Agregado_dos_Setores_20180416.zip"),destfile = "Inputs/Information/Census_Universe.zip")

# Rename and organize files
tibble(path = paste0(ibge_dir,states[-3],"/Base informa‡oes setores2010 universo ",states[-3],"/CSV"),
       new_path = paste0(ibge_dir,states[-3],"/")) %>%
  pmap(file_move)

# Rename and organize files for SP capital and state
file.rename(paste0(ibge_dir,"Base informa‡oes setores2010 universo SP_Capital"),paste0(ibge_dir,"SP_Capital"))
file.rename(paste0(ibge_dir,"SP Exceto a Capital"),paste0(ibge_dir,"SP_State"))
file_move(paste0(ibge_dir,"SP_Estado/Base informa‡oes setores2010 universo SP_Exceto_Capital/CSV"),paste0(ibge_dir,"SP_State/"))

# Correct files in order to merge capital and state data together 
files <- sprintf("%02d", 1:13)
tibble(from = paste0(ibge_dir,"SP_State/CSV/Pessoa",files,"_SP.csv"),
       to = paste0(ibge_dir,"SP_State/CSV/Pessoa",files,"_SP2.csv")) %>%
  pmap(file.rename)
files <- files[1:2]
tibble(from = paste0(ibge_dir,"SP_Estado/CSV/responsavel",files,"_sp2.csv"),
       to = paste0(ibge_dir,"SP_Estado/CSV/Responsavel",files,"_SP2.csv")) %>%
  pmap(file.rename)
read_delim(paste0(ibge_dir,"SP_Estado/CSV/Pessoa11_SP2.csv"),delim=";",
           locale=locale(decimal_mark=",",grouping_mark=".",encoding="latin1")) %>% select(-Cod_se) %>%
  write_delim(paste0(ibge_dir,"SP_Estado/CSV/Pessoa11_SP2.csv"),delim=";")

# Merge SP files
dir.create(file.path(ibge_dir,"SP"))
dir.create(file.path(paste0(ibge_dir,"SP/"),"CSV"))
files <- list.files(paste0(ibge_dir,"SP_Capital/CSV/"))
files <- gsub("_SP1","_SP", files)
files <- files[-c(5:9)]
for (file in files){
  aux1 <- read_delim(paste0(ibge_dir,"SP_Capital/CSV/",gsub("_SP","_SP1",file)),
                     delim=";",
                     locale=locale(decimal_mark=",",grouping_mark=".",encoding="latin1")) %>%
    select_if(~sum(!is.na(.)) > 0)
  aux2 <- read_delim(paste0(ibge_dir,"SP_Estado/CSV/",gsub("_SP","_SP2",file)),
                     delim=";",
                     locale=locale(decimal_mark=",",grouping_mark=".",encoding="latin1")) %>%
    select_if(~sum(!is.na(.)) > 0)
  colnames(aux2) <- colnames(aux1)
  rbind(aux1,aux2) %>% write.csv(paste0(ibge_dir,"SP/CSV/",file))
}

# Delete unused files
file_delete(paste0(ibge_dir,c("SP_Capital","SP_Estado","PR/Base informa‡oes setores2010 universo PR","RJ/Base informa‡oes setores2010 universo RJ","BA/Base informa‡oes setores2010 universo BA")))

## Census shapefiles

# Links to access the data from IBGE website and to store it
ibge_url <- "ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_de_setores_censitarios__divisoes_intramunicipais/censo_2010/setores_censitarios_shp/"
ibge_dir <- "Inputs/Census_Shapefiles/"

# Download census tracts shapefiles
tibble(url=paste0(ibge_url,tolower(states),"/",tolower(states),"_setores_censitarios.zip"),
       destfile=paste0(ibge_dir,states,"_tracts.zip")) %>%
  pmap(download.file)

# Unzip and rename SP shp
tibble(zipfile=paste0(ibge_dir,states[3],"_tracts.zip"),
       exdir=substr(ibge_dir,1,nchar(ibge_dir)-1)) %>%
  pmap(unzip)
files <- c(".dbf",".shp",".shx",".prj")
tibble(from=paste0(ibge_dir,"33SEE250GC_SIR",files),
       to=paste0(ibge_dir,"35SEE250GC_SIR",files)) %>%
  pmap(file.rename)

# Unzip census tracts shapefiles
tibble(zipfile=paste0(ibge_dir,states[-3],"_tracts.zip"),
       exdir=substr(ibge_dir,1,nchar(ibge_dir)-1)) %>%
  pmap(unzip)

# Download municipailities shapefiles
tibble(url=paste0(ibge_url,tolower(states),"/",tolower(states),"_municipios.zip"),
       destfile=paste0(ibge_dir,states,"_mun.zip")) %>%
  pmap(download.file)

# Unzip municipalities shapefiles
tibble(zipfile=paste0(ibge_dir,states,"_mun.zip"),
       exdir=substr(ibge_dir,1,nchar(ibge_dir)-1)) %>%
  pmap(unzip)

## RAIS 2017

# Links to access the data from RAIS and to store it
rais_url <- "ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/2017/"
rais_dir <- "Inputs/RAIS/"
cod_rais <- c(paste0(states,"2017.7z"))

# Download employees files
tibble(url = paste0(rais_url,cod_rais),
       destfile = paste0(rais_dir,states,"2017.zip")) %>%
  pmap(download.file)

# Unzip employees files
tibble(zipfile = paste0(rais_dir,states,"2017.zip"),
       exdir = substr(rais_dir,1,nchar(rais_dir)-1)) %>%
  pmap(unzip)

# Download employers files
download.file(url = paste0(rais_url,"ESTB2017.7z"),
              destfile = paste0(rais_dir,"ESTB2017",".zip"))

# Unzip employers files
unzip(zipfile = paste0(rais_dir,"ESTB2017",".zip"),
      exdir = substr(rais_dir,1,nchar(rais_dir)-1))

# Download documentation files
rais_url <- "ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/Layouts/"
download.file(url = paste0(rais_url,"v%EDnculos/RAIS_vinculos_layout2017.xls"),
              destfile = "Inputs/Information/Rais_Employees.xls")
download.file(url = paste0(rais_url,"estabelecimento/RAIS_estabelecimento_layout2014a2017.xls"),
              destfile = "Inputs/Information/Rais_Employers.xls")

## RAIS 2014 - SP

# Download employers files
rais_url <- "ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/2014/"
download.file(url = paste0(rais_url,"ESTB2014.7z"),
              destfile = paste0(rais_dir,"ESTB2014",".zip"))

# Unzip employers files
unzip(zipfile = paste0(rais_dir,"ESTB2014",".zip"),
      exdir = substr(rais_dir,1,nchar(rais_dir)-1))

#### Geocode ####

# Data preparation to geocode CEPs on Google Earth
city <- "sp"
emp <- read.delim("Inputs/RAIS/ESTB2017.txt", sep = ";",colClasses = "character") %>%
  filter(Município=="410690")
emp <- read.delim("Inputs/RAIS/ESTB2017.txt", sep = ";",colClasses = "character") %>%
  filter(Município=="330455")
emp <- read.delim("Inputs/RAIS/ESTB2017.txt", sep = ";",colClasses = "character") %>%
  filter(Município=="355030")
emp <- read.delim("Inputs/RAIS/ESTB2017.txt", sep = ";",colClasses = "character") %>%
  filter(Município=="230440")

n_jobs <- emp %>% mutate(Qtd.Vínculos.Ativos = as.numeric(Qtd.Vínculos.Ativos)) %>%
  summarise(sum(Qtd.Vínculos.Ativos,na.rm=T))

cep2 <- unique(emp$CEP.Estab) # Filter unique ceps
cep2 <- as.data.frame(cep2) %>%
  rename("CEP"=cep2) %>%
  mutate(CEP = as.numeric(as.character(CEP))) %>%
  mutate(CEP = str_pad(CEP,8, pad = 0)) %>%
  mutate(CEP=paste0(substr(CEP,1,5),"-",substr(CEP,6,9)))

# Loop

cep <- read.csv(paste0("Inputs/RAIS/",city,"_cep.csv")) # Geocoded database

cep <- cep %>% full_join(cep2,by = "CEP")
     
cep %>% filter(is.na(LAT)) %>%
  filter(as.numeric(substr(CEP,1,5)) >= 1000 & as.numeric(substr(CEP,1,5)) <= 8499) %>%
  filter(as.numeric(substr(CEP,1,5)) <= 5999 | as.numeric(substr(CEP,1,5)) >= 8000) %>%
  slice(1:2500) %>% write.csv("_temp/earth.csv") # Export to Google Earth

cep_geo <- read.csv("_temp/earth_geo.csv") %>% select(CEP,LAT,LON) # Read table of geocoded ceps
cep <- full_join(cep,cep_geo,by="CEP")

cep <- cep %>%
  mutate_all(as.character) %>%
  mutate(LAT=ifelse(is.na(LAT.x),LAT.y,LAT.x),
         LON=ifelse(is.na(LON.x),LON.y,LON.x)) %>%
  select(CEP,LAT,LON) # Clean table

cep <- cep %>%
  filter(as.numeric(substr(CEP,1,5)) >= 1000 & as.numeric(substr(CEP,1,5)) <= 8499) %>%
  filter(as.numeric(substr(CEP,1,5)) <= 5999 | as.numeric(substr(CEP,1,5)) >= 8000)

write.csv(cep,paste0("Inputs/RAIS/",city,"_cep.csv")) # Update geocoded database

# After geocode

cep <- st_read(paste0("Outputs/Shapefiles/",city,"_cep.shp")) %>%
  mutate(CEP.Estab = paste0(substr(CEP,1,5),substr(CEP,7,9))) # Transform to shapefile and clean database in QGis

emp <- cep %>%
  left_join(emp %>% mutate(Qtd.Vínculos.Ativos = as.numeric(Qtd.Vínculos.Ativos)) %>%
              filter(Qtd.Vínculos.Ativos != 0), by = "CEP.Estab") # Filter active jobs
st_write(emp,paste0("Outputs/Shapefiles/",city,"_emp.shp"))

#### Verification (geocode) ####

sp_hex500 <- st_read("Outputs/Shapefiles/sp_hex500.shp")

# Jobs from grid of 1km (NEREUS - FEA/USP)

sp_grid <- st_read("Inputs/RAIS/sp_estab_2014_grid.shp")

sp_grid_data <- sp_grid %>% mutate(jobs = emprego) %>%
  select(id_masp,jobs) %>%
  st_drop_geometry() %>%
  mutate(id_masp = as.character(id_masp))

sp_reapportion <- spReapportion(sp_grid %>% st_transform(4326) %>% as('Spatial'),
                                sp_hex500 %>% as('Spatial'),
                                data = sp_grid_data,
                                old_ID = "id_masp",
                                new_ID = "GEO_ID",
                                data_ID = "id_masp",
                                variables = c("jobs"),
                                mode = "count")

sp_grid <- sp_hex500 %>% left_join(sp_reapportion, by = "GEO_ID")

st_write(sp_grid,"Outputs/SP_2014/sp_jobs_grid.shp", delete_layer = TRUE)

plot <- PlotMap(sp_grid,"sp",sp_grid$jobs,"Formal jobs")
ggsave("sp_jobs_grid.jpeg",plot,device = "jpeg",path = "Outputs/Plots/hex500",dpi = 600)

# Jobs from RAIS (Geocode)

sp_geo <- read.delim("Inputs/RAIS/ESTB2014.txt", sep = ";",colClasses = "character") %>% 
  filter(Município=="355030")

cep <- st_read(paste0("Outputs/Shapefiles/sp_cep.shp")) %>%
  mutate(CEP.Estab = paste0(substr(CEP,1,5),substr(CEP,7,9))) %>%
  mutate(CEP.Estab = as.numeric(CEP.Estab))

sp_geo <- cep %>%
  left_join(sp_geo %>%
              mutate(jobs = as.numeric(Qtd.Vínculos.Ativos)) %>%
              filter(jobs != 0) %>%
              mutate(CEP.Estab = as.numeric(CEP.Estab)), by = "CEP.Estab")

sp_geo <- sp_hex500 %>% st_join(sp_geo,join = st_intersects) %>%
  mutate(jobs = as.numeric(jobs)) %>%
  select(GEO_ID,jobs) %>%
  group_by(GEO_ID) %>% summarise(jobs = sum(jobs,na.rm=T)) %>%
  mutate(jobs=ifelse(is.na(jobs),0,jobs))

st_write(sp_geo,"Outputs/SP_2014/sp_jobs_geo.shp", delete_layer = TRUE)

plot <- PlotMap(sp_geo,"São Paulo",sp_hex500_geo$jobs,"Formal jobs")
ggsave("sp_jobs_geo.jpeg",plot,device = "jpeg",path = "Outputs/Plots/hex500",dpi = 600)

# Accessibility levels

sp_traveltime_matrix <- read.csv("D:/Drive - Taina/Mestrado/Cidades/OTP/SP/sp_traveltime_matrix.csv") %>%
  mutate(GEO_ID_i = as.factor(origin), GEO_ID_j = as.factor(destination))

sp_grid <- sp_traveltime_matrix %>% left_join(sp_grid %>%
                                                mutate(GEO_ID_j = GEO_ID) %>%
                                                select(GEO_ID_j,jobs), by="GEO_ID_j") %>%
  mutate(travel_time = travel_time / 60, walk_distance = walk_distance / 1000)

n_jobs_grid <- sum(sp_grid$jobs, na.rm = T)

sp_grid <- sp_grid %>% mutate(jobs_30 = ifelse(travel_time <= 30, jobs,0),
                              jobs_45 = ifelse(travel_time <= 45, jobs,0),
                              jobs_60 = ifelse(travel_time <= 60, jobs,0),
                              jobs_90 = ifelse(travel_time <= 90, jobs,0),
                              jobs_120 = ifelse(travel_time <= 120, jobs,0)) %>%
  group_by(GEO_ID_i) %>%
  summarise_at(c("jobs_30","jobs_45","jobs_60","jobs_90","jobs_120"),~(p = sum(.,na.rm = T)/100000)) %>%
  mutate_all(.,~(p = . * 100000 / n_jobs_grid))

sp_geo <- sp_traveltime_matrix %>% left_join(sp_geo %>%
                                               mutate(GEO_ID_j = GEO_ID), by="GEO_ID_j") %>%
  mutate(travel_time = travel_time / 60, walk_distance = walk_distance / 1000)

n_jobs_geo <- sum(sp_geo$jobs, na.rm = T)

sp_geo <- sp_geo %>% mutate(jobs_30 = ifelse(travel_time <= 30, jobs,0),
                            jobs_45 = ifelse(travel_time <= 45, jobs,0),
                            jobs_60 = ifelse(travel_time <= 60, jobs,0),
                            jobs_90 = ifelse(travel_time <= 90, jobs,0),
                            jobs_120 = ifelse(travel_time <= 120, jobs,0)) %>%
  group_by(GEO_ID_i) %>%
  summarise_at(c("jobs_30","jobs_45","jobs_60","jobs_90","jobs_120"),~(p = sum(.,na.rm = T)/100000)) %>%
  mutate_all(.,~(p = . * 100000 / n_jobs_geo))

sp_grid <- sp_grid %>%
  select(GEO_ID,jobs_30p,jobs_45p,jobs_60p,jobs_90p,jobs_120p) %>%
  st_drop_geometry() %>%
  filter(!is.na(jobs_30p))

sp_geo <- sp_geo %>%
  select(GEO_ID,jobs_30p,jobs_45p,jobs_60p,jobs_90p,jobs_120p) %>%
  st_drop_geometry() %>%
  filter(!is.na(jobs_30p))

cor(sp_grid$jobs_30p,sp_geo$jobs_30p,method = "pearson")
cor(sp_grid$jobs_45p,sp_geo$jobs_45p,method = "pearson")
cor(sp_grid$jobs_60p,sp_geo$jobs_60p,method = "pearson")
cor(sp_grid$jobs_90p,sp_geo$jobs_90p,method = "pearson")
cor(sp_grid$jobs_120p,sp_geo$jobs_120p,method = "pearson")

# Scatter plot
diff <- left_join(sp_grid %>% rename("grid_30"=jobs_30p),
                  sp_geo %>% rename("geo_30"=jobs_30p),
                  by="GEO_ID") %>%
  mutate(diff_30 = grid_30 - geo_30)

mean <- mean(diff$diff_30,na.rm=T)
sd_p <- mean + sd(diff$diff_30,na.rm=T)
sd_n <- mean - sd(diff$diff_30,na.rm=T)

curve <- ggplot() + 
  theme_classic() +
  theme_map() +
  geom_point(data = diff, aes(x = GEO_ID, y=diff_30),alpha = 0.5,color = "#972C7E") + 
  geom_hline(aes(yintercept = sd_p),color = "#8E8D89", linetype = "dotted",size = 0.6) + 
  geom_hline(aes(yintercept = mean),color = "#8E8D89", linetype = "dashed",size = 0.6) +
  geom_hline(aes(yintercept = sd_n),color = "#8E8D89", linetype = "dotted",size = 0.6) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Difference (Grid - Geocode)")

ggsave("sp_grid_geo_curve.jpeg",curve,device = "jpeg",path = "Outputs/Plots/hex500",dpi = 600)

# Map
diff <- sp_hex500 %>% left_join(diff,by="GEO_ID")

plot <- PlotMap(diff,"São Paulo",diff$jobs,"Formal jobs")
ggsave("sp_grid_geo_map.jpeg",plot,device = "jpeg",path = "Outputs/Plots/hex500",dpi = 600)
         
                         
                         
