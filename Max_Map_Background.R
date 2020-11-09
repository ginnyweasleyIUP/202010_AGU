### Max Background Map

library(plyr)
library(dplyr)
library(tidyverse)
library(ncdf4)

# This is the data for the land sea ice mask from Mo --> I saved it in lsm_evo_trace.RData
# There is no need to reextract it!

# ## load TRACE21ka land sea mask as background
# nc <- nc_open('/stacydata/data/trace21ka/processed/trace.01-36.22000BP.PFTFRAC_decavg_400BCE.nc')
# trace_pfts <- ncvar_get(nc, 'PFTFRAC')
# nc_close(nc)
# trace_lsm_lgm <- trace_pfts[1,,,1]
# trace_lsm_lgm[!is.nan(trace_lsm_lgm)] <- 1
# trace_lsm_lgm[is.nan(trace_lsm_lgm)] <- 0
# temp <- trace_lsm_lgm[1:48,]
# trace_lsm_lgm[1:48,] <- trace_lsm_lgm[49:96,]
# trace_lsm_lgm[49:96,] <- temp
# rm(temp)
# trace_lsm_evo <- trace_lsm_lgm #+ trace_lsm_hol
# 
# ## load ICE-5G ice shelfs as background (used within TRACE)
# nc <- nc_open('/stacywork/fredweasley/data/Peltier_ICE-5G/ice5g_v1.2_21.0k_3.75deg_remapbil.nc')
# ice5g_lgm <- ncvar_get(nc, 'sftgif')
# nc_close(nc)
# ice5g_lgm[ice5g_lgm == 100] <- 1
# ice5g_lgm[is.na(ice5g_lgm)] <- 0
# temp <- ice5g_lgm[1:48,]
# ice5g_lgm[1:48,] <- ice5g_lgm[49:96,]
# ice5g_lgm[49:96,] <- temp
# ice5g_evo <- ice5g_lgm #+ ice5g_hol
# trace_lsm_evo[ice5g_evo != 0] <- ice5g_evo[ice5g_evo != 0] + 2

#save(trace_lsm_evo, file = "lsm_evo_trace.RData")


# This is where the plotting starts

load("lsm_evo_trace.RData")

source("Functions/projection_ptlyr.R")

#Example Point Layer --> exchange for own values
# Unfortunately until now I have not found a way to add the shape or color information in the layer-level
# For now you either have to find it out yourself
# Or you make separate Point_Lyr files like here and then add the shape and color information in the plotting below.
# Please still use the layer-level, otherwise the projection does not work. Or you rewrite the projection function...
Point_Lyr_1 <- data.frame(
  long = c(-150,-90,-30, 30, 60,150),
  lat = c(-75,-45,-15, 15, 45, 75),
  layer = c(1,1,1,1,1,1)
)
Point_Lyr_2 <- data.frame(
  long = c(150,90,30, -30, -60,-150),
  lat = c(-75,-45,-15, 15, 45, 75),
  layer = c(1,1,1,1,1,1)
)
Point_Lyr_3 <- data.frame(
  long = c(-155,-95,-35, 35, 65,155),
  lat = c(-85,-55,-25, 25, 55, 85),
  layer = c(1,1,1,1,1,1)
)


Point_Lyr_1_p <- projection_ptlyr(Point_Lyr_1, as.character('+proj=robin +datum=WGS84'))
Point_Lyr_2_p <- projection_ptlyr(Point_Lyr_2, as.character('+proj=robin +datum=WGS84'))
Point_Lyr_3_p <- projection_ptlyr(Point_Lyr_3, as.character('+proj=robin +datum=WGS84'))

source("Functions/STACYmap_PMIL.R")

plot <- STACYmap(gridlyr = trace_lsm_evo[,48:1], colorscheme = c("#96c1e3", "#EAC999", "#fcfcfc"))+
  guides(fill = F)+
  new_scale_color() +
  geom_point(data = Point_Lyr_1_p, aes(x = long, y = lat, shape = '1', color = '1'),# shape = 20,
             size = 3, alpha = 0.7, show.legend = c(shape =TRUE)) +
  geom_point(data = Point_Lyr_2_p, aes(x = long, y = lat, shape = '2', color = '1'),# shape = 20,
             size = 3, alpha = 0.7, show.legend = c(shape =TRUE)) +
  geom_point(data = Point_Lyr_3_p, aes(x = long, y = lat, shape = '3', color = '2'),# shape = 20,
             size = 3, alpha = 0.7, show.legend = c(shape =TRUE)) +
  scale_color_manual(name = "blabla color", labels = c("lab 1.1", "lab 1.2"), 
                     values = c('#DD2525','#000000')) +
  scale_shape_manual(name = "blabla shape", labels = c("lab 2.1", "lab 2.2", "lab 2.3"), 
                     values = c(17, 18, 20)) +
  theme(panel.border = element_blank(),
        legend.background = element_blank(),
        axis.text = element_blank(),
        text = element_text(size = 12),
        legend.title = element_text(size = 12))

plot

plot %>% ggsave(filename = paste('Example_File', 'pdf', sep = '.'), plot = ., path = 'Plots', 
           width = 10, height = 7, units = 'cm', dpi = 'print', device = "pdf")
