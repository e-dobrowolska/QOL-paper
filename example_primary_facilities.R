# starter
{
  #library(tidyverse)
  library(sf)
  library(rgdal)
  library(ggplot2)
  library(spdep)
  
  setwd("C:/Users/ewado/OneDrive/Dokumenty/Studia/Artykul_QOL/Dane")
  
  grid_qol<-readOGR(".", "dataset_qol")
  grid.df<-as.data.frame(grid_qol)
  grid.sf<-st_as_sf(grid_qol)
  grid.sp<-as(grid_qol, "SpatialPolygons")
}

## Example - part of the index from Primary Facilities (id_{pharmacy,1}) 
{
  crds=coordinates(grid_qol)
  idx=c(302, 331, 369, 316, 336, 360, 327, 330, 364)
  
  # pharmacies in cells + residents
  ggplot(grid.sf[idx,])+
    geom_sf(mapping = aes(geometry=geometry), fill=ifelse(grid.sf$ID[idx]==336, "grey70", "grey95"))+
    geom_text(x=crds[idx,1], y=crds[idx,2],label=grid.sf$pharmcy[idx], vjust=-1)+
    geom_text(x=crds[idx,1], y=crds[idx,2],label=paste0("(", grid.sf$RES[idx], ")"), vjust=1)+
    labs(title="Number of pharmacies in the cell",
         subtitle = "Number of residents - in brackets")+
    theme_void()+
    theme(plot.title=element_text(hjust=0.5),
          plot.subtitle = element_text(hjust=0.5))
  
  # Per capita pharmacies
  perc <- ifelse(grid.sf$RES>0, grid.sf$pharmcy/grid.sf$RES, grid.sf$pharmcy)
  Neighbours<-poly2nb(as(grid_qol, "SpatialPolygons"))
  perc_neighbours<-unlist(lapply(seq.int(1, nrow(grid_qol)), function(n) sum(perc[Neighbours[[n]]])))
  score = perc+0.5*perc_neighbours
  
  ggplot(grid.sf[idx,])+
    geom_sf(mapping = aes(geometry=geometry), fill=ifelse(grid.sf$ID[idx]==336, "grey70", "grey95"))+
    geom_text(x=crds[idx,1], y=crds[idx,2],label=round(perc[idx],5))+
    geom_text(x=crds[idx,1], y=crds[idx,2],label=paste0("(", round(score[idx],5), ")"), vjust=2)+
    labs(title="Number of pharmacies - per capita",
         subtitle="Cell's score - in brackets: \n per capita pharmacies + 0.5*(sum of per capita neighbours' pharmacies)")+
    theme_void()+
    theme(plot.title=element_text(hjust=0.5),
          plot.subtitle = element_text(hjust=0.5))
  
  # Primary index  
  threshold<-quantile(score, 0.75)
  idPrim<-ifelse(score>threshold, 9, score/threshold*9)
  idPrim_up<-ceiling(idPrim)
  
  ggplot(grid.sf[idx,])+
    geom_sf(mapping = aes(geometry=geometry, fill=idPrim_up[idx]))+
    geom_label(x=crds[idx,1], y=crds[idx,2],label=idPrim_up[idx])+
    scale_fill_viridis_c()+
    labs(title="Contribution from Primary Facilities (pharmacies)")+
    theme_void()+
    theme(plot.title=element_text(hjust=0.5),
          plot.subtitle = element_text(hjust=0.5),
          legend.position="None")
}