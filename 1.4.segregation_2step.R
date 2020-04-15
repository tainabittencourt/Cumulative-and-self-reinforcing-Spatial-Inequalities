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


#### Prepare data for GEODA ####

geoda <- function(st,sz){
  
  seg <- st_read(paste0("Outputs/Shapefiles/",tolower(st),"_hex",sz,"_access_2step.shp")) %>%
    left_join(st_read(paste0("Outputs/Shapefiles/",tolower(st),"_hex",sz,"_seg.shp")) %>%
                select(GEO_ID,Local_Dive,Local_Entr) %>%
    left_join(read_csv(paste0("Outputs/CSV/",tolower(st),"_seg",sz,"_local.csv")) %>%
                rename("GEO_ID" = id) %>%
                mutate(GEO_ID = as.character(GEO_ID)) %>%
                select(GEO_ID,iso_00,iso_11,iso_22,iso_33,iso_44,iso_55),by="GEO_ID") %>%
      st_drop_geometry(),by="GEO_ID") %>%
    filter(jobs_45>0) %>%
    mutate(access = (jobs_45),
           seg = (1/Local_Dive),
           access_log = log(jobs_45),
           seg_log = log(1/Local_Dive)) %>% ungroup()
  
  
  seg %>% st_write(paste0("Outputs/Shapefiles/",tolower(st),"_hex",sz,"_seg.shp"),delete_layer=T)
  
}

tibble(st = rep(c("SP","RJ","PR","CE"),each=4),sz = rep(c("500","1000","2500","5000"),4)) %>%
  pmap(geoda)

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
  
  # Fixed breaks (jenks)
  breaks <- c(-1,0,1.05,1.20,1.35,1.50,1.65,1.8)
  base$breaks <- cut(variable,breaks = breaks,include.lowest = TRUE,labels = breaks[2:8])
  breaks_scale <- levels(base$breaks) %>% as.numeric()
  labels <- breaks[2:8]
  
  # Cut variable based on breaks
  #range_scale <- function(x){(x-min(x))/(max(x)-min(x))}
  #variable <- range_scale(variable)
  #base$breaks <- cut(variable,breaks = 7,include.lowest = TRUE)
  #breaks_scale <- levels(base$breaks)
  #labels <- sub('.*,\\s*', '',breaks_scale)
  #labels <- gsub(']','',labels)
  
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
      values = rev(c("#BD0C1E","#1B5FB9","#3F999B","#E76115")),
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
  hex <- st_read(paste0("Outputs/Shapefiles/",tolower(state),"_hex500_access.shp"))
  
  seg <- st_read(paste0("Outputs/Shapefiles/",tolower(state),"_hex500_seg.shp")) %>%
    left_join(read_csv(paste0("Outputs/CSV/",tolower(state),"_seg500_local.csv")) %>%
                rename("GEO_ID" = id) %>%
                mutate(GEO_ID = as.character(GEO_ID)),by="GEO_ID")
  
  # Map elements
  mun <- st_read(paste0("Inputs/Map_Elements/",tolower(state),"_mun_MR.shp"))
  mun_city <- st_read(paste0("Inputs/Map_Elements/",tolower(state),"_mun.shp"))
  transport <- st_read(paste0("Inputs/Map_Elements/",tolower(state),"_transport_axes.shp"))
  mask <- st_read(paste0("Inputs/Land_Cover/Shapefiles/",tolower(state),"_cover_MR.shp"))
  back <- st_read("Inputs/Map_Elements/br_mun.shp")
  
  hex_metro <- seg %>% filter(CD_GEOCODM == metro)
  
  # Segregation
  
  #plot <- PlotMap(hex_metro,tolower(state),hex_metro$dissimil,"Dissimilarity index")
  #ggsave(paste0(tolower(state),"_dissim.jpeg"),plot,device = "jpeg",path = "Outputs/Plots/hex500_access",dpi = 600)
  
  #plot <- PlotMap(hex_metro,tolower(state),hex_metro$Local_Dive,"Diversity index")
  #ggsave(paste0(tolower(state),"_divers.jpeg"),plot,device = "jpeg",path = "Outputs/Plots/hex500_access",dpi = 600)
  
  #plot <- PlotMap(hex_metro,tolower(state),hex_metro$iso_00,"Upper-class white isolation")
  #ggsave(paste0(tolower(state),"_iso_hi_wh.jpeg"),plot,device = "jpeg",path = "Outputs/Plots/hex500_access",dpi = 600)
  
  #plot <- PlotMap(hex_metro,tolower(state),hex_metro$iso_55,"Lower-class black isolation")
  #ggsave(paste0(tolower(state),"_iso_lo_bl.jpeg"),plot,device = "jpeg",path = "Outputs/Plots/hex500_access",dpi = 600)

  
  hex_metro <- seg %>% mutate(lo_class = lo_wh + lo_bl) %>%
    filter(lo_class > 0) %>%
    filter(low_ac %in% c("1","2","3","4"))
  
  # LISA accessibility and segregation
  #plot <- PlotMap2(hex_metro,tolower(state),hex_metro$lisa_l,"Accessibility | Segregation")
  #ggsave(paste0(tolower(state),"_ac_seg_lisa_2step.jpeg"),plot,device = "jpeg",path = "Outputs/Plots/hex500_access",dpi = 600,height = 7,width = 7)
  
  # LISA accessibility and low class population
  plot <- PlotMap2(hex_metro,tolower(state),hex_metro$low_ac,"Lower-class population | Accessibility")
  ggsave(paste0(tolower(state),"_ac_low_lisa_2step.jpeg"),plot,device = "jpeg",path = "Outputs/Plots/hex500_access",dpi = 600,height = 7,width = 7)
  
  
  }

#### Comparação de Medianas ####

Mediantest <- function(st,vr,sz,gr){
  
  seg <- st_read(paste0("Outputs/Shapefiles/",tolower(st),"_hex",sz,"_seg.shp")) %>%
    st_drop_geometry() %>%
    left_join(read_csv(paste0("Outputs/CSV/",tolower(st),"_seg",sz,"_local.csv")) %>%
                rename("GEO_ID" = id) %>%
                mutate(GEO_ID = as.character(GEO_ID)),by="GEO_ID")
  
  seg <- seg %>% select(gr,vr)
  colnames(seg) <- c("pop","var")
  seg <- rep(seg$var,seg$pop) %>% na.omit()
  
  median <- MedianCI(seg,sides = "two.sided",na.rm = T,method="boot",R=1000)
  median <- c(st,sz,gr,median)
  return(median)}


tibble(st = rep(rep(c("SP","RJ","PR","CE"),each=24),1),
       vr = rep(c("Local_Dive"),each=24*4),
       sz = rep(rep(c("500","1000","2500","5000"),each=6),4*1),
       gr = rep(c("hi_wh","hi_bl","md_wh","md_bl","lo_wh","lo_bl"),16*1)) %>%
  pmap_df(Mediantest) %>% write.csv("Outputs/CSV/medians_seg.csv")

#### Scatter Plots ####

PlotMoran <- function(seg){
  
  plot <- seg %>% ggplot(aes(x = moran_std, y = moran_lag)) +
    geom_point(aes(color = lisa),size = 1.5,alpha = 0.7) +
    scale_color_manual(values = c("#C5C5C5","#BD0C1E","#1B5FB9","#3F999B","#E76115","#C5C5C5","#C5C5C5"),guide = F) +
    #geom_point(size = 0.5,alpha = 0.5,color = "blue") +
    geom_smooth(method = lm,se=T,color = "black") +
    geom_hline(yintercept = 0,linetype = "dotted",color = "dimgrey") +
    geom_vline(xintercept = 0,linetype = "dotted",color = "dimgrey") +
    xlim(-5,5) + 
    ylim(-2.5,5) +
    theme_classic() +
    xlab("Accessibility") +
    ylab("Lagged Segregation")
  }
  

for(state in states){ 
  
  seg <- st_read(paste0("Outputs/Shapefiles/",tolower(state),"_hex500_seg.shp")) %>%
    st_drop_geometry() %>%
    #select(moran_std,moran_lag,lisa) %>%
    select(moran_td_l,moran_lg_l,lisa_l) %>%
    rename("moran_std" = moran_td_l,"moran_lag" = moran_lg_l,lisa = lisa_l) %>%
    mutate(lisa = as.character(lisa)) %>%
    filter(!is.na(lisa))
  
  plot <- PlotMoran(seg)
  ggsave(paste0(tolower(state),"_ac_seg_lisa_2step_log.jpeg"),plot,device = "jpeg",path = "Outputs/Graphics",dpi = 600,height = 3.5,width = 5)
}


#### Global segregation indices ####

indices <- read_delim("Outputs/CSV/all_global_seg_city.txt",delim = " ") %>%
  mutate(state = case_when(state == "SP" ~ "Sao Paulo",
                           state == "PR" ~ "Curitiba",
                           state == "RJ" ~ "Rio de Janeiro",
                           state == "CE" ~ "Fortaleza")) %>%
  mutate(state = factor(state,levels=c("Sao Paulo","Rio de Janeiro","Curitiba","Fortaleza"))) %>%
  mutate(cell_size = as.character(cell_size)) %>%
  group_by(cell_size) %>%
  mutate_at(c("hi_wh","hi_bl","md_wh","md_bl","lo_wh","lo_bl"),scale)

indices <- read_delim("Outputs/CSV/seg_group_MR.csv",delim = ",") %>%
  filter(variable == "isol") %>%
  select(-c(X1,variable)) %>%
  mutate(state = case_when(state == "SP" ~ "Sao Paulo",
                           state == "PR" ~ "Curitiba",
                           state == "RJ" ~ "Rio de Janeiro",
                           state == "CE" ~ "Fortaleza")) %>%
  mutate(state = factor(state,levels=c("Sao Paulo","Rio de Janeiro","Curitiba","Fortaleza"))) %>%
  mutate(cell_size = as.character(cell_size)) %>%
  group_by(cell_size) %>%
  mutate_at(c("hi_wh","hi_bl","md_wh","md_bl","lo_wh","lo_bl"),scale)
  
#indices$is_min <- apply(indices[3:8],1, FUN=min)
#indices$is_max <- apply(indices[3:8],1, FUN=max)

#indices <- indices %>% group_by(cell_size) %>%
 # mutate(is_min = min(is_min),
  #       is_max = max(is_max)) %>%
  #ungroup() %>%
  #mutate_at(c("hi_wh","hi_bl","md_wh","md_bl","lo_wh","lo_bl"),~(p=((.)-is_min)/(is_max-is_min)))

colnames(indices) <- c("Size","City","Upper-White","Upper-Black","Middle-White","Middle-Black","Lower-White","Lower-Black")

indices %>%
  gather(key="Group",value="Isolation",-c("City","Size")) %>%
  group_by(Group,City) %>%
  mutate(is_min = min(Isolation),
         is_max = max(Isolation)) %>%
  ungroup() %>%
  filter(Size == "500") %>%
  mutate(Group = factor(Group,levels=c("Upper-White","Upper-Black","Middle-White","Middle-Black","Lower-White","Lower-Black"))) %>%
  ggplot(aes(x=City,y=Isolation,color=Group)) +
  geom_errorbar(aes(x=City,ymin=is_min,ymax=is_max),alpha=0.6,position="dodge")+
  geom_point(position = position_dodge(0.9)) +
  scale_color_manual(values = magma(8)[2:7]) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("seg_global_MR.jpeg",device = "jpeg",path = "Outputs/Graphics",dpi = 600,height = 3,width=6)
