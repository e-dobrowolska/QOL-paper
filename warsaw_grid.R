#############################################################

#                 Warsaw grid (census 2021)                 #

#############################################################

### Starter ###
{
  library(rgdal)
  library(sf)
  library(ggplot2)
  library(osmdata)
  
  # The data (for Poland, used below) & instructions for download are available at
  # https://portal.geo.stat.gov.pl/en/news/data-on-residents-in-a-kilometre-grid-census-2021-available-for-download/
  setwd("...") # path to the folder with data
  
  grid_pl<-readOGR(".", "GRID_NSP2021_RES")
  grid_pl<-spTransform(grid_pl, CRS("+proj=longlat +datum=NAD83"))
  grid_pl.sp<-as(grid_pl, "SpatialPolygons")
  grid_pl.df<-as.data.frame(grid_pl)
  
  # The data for Polish poviats is available at 
  # https://gis-support.pl/baza-wiedzy-2/dane-do-pobrania/granice-administracyjne/
  # download "Powiaty"
  pov<-readOGR(".", "powiaty") # 380
  pov<-spTransform(pov, CRS("+proj=longlat +datum=NAD83")) 
  pov.waw<-pov[pov$jpt_nazwa_=="powiat Warszawa",]
}

### Cut the "Warsaw" grid from the "Poland" grid ###
{
  over_results<-over(grid_pl.sp, pov.waw) # 0-1, polygons overlay/don't overlay
  warsaw_cells<-which(over_results$jpt_nazwa_=="powiat Warszawa") # indices of Warsaw cells 
  waw.sp<-grid_pl.sp[warsaw_cells, ] 
  waw.df<-grid_pl.df[warsaw_cells, ]
  waw.df$ID=1:nrow(waw.df)
  grid.waw<-SpatialPolygonsDataFrame(waw.sp, waw.df) # merge polygons with data
  
  writeOGR(grid.waw, ".", "grid_waw_2021", driver = "ESRI Shapefile") # save new shapefile
  
}

### Picture - visualization ###
{
  # plot
  plot(grid.waw)
  plot(grid[seq(1, 100, 20),])
  
  # ggplot
  grid.sf <- st_as_sf(grid.waw)
}

### Better picture - population density + district borders ###
{
  # choropleth
  choropleth(grid.waw, grid.waw$RES)
  
  # ggplot picture
  ggplot()+
    geom_sf(grid.sf, mapping = aes(geometry=geometry, fill=RES))+
    scale_fill_viridis_c(name="Population")+
    ggtitle("Population density in Warsaw")+
    theme_minimal()+
    theme(plot.title=element_text(hjust=0.5))
  
  # Retrieve borders from OSM
  pov.waw.sf <- st_as_sf(pov.waw, crs = "+proj=longlat +datum=NAD83")
  
  warsaw_districts.sf <- opq(st_bbox(grid.sf)) %>% 
    add_osm_feature(key='admin_level', value='9') %>% 
    osmdata_sf()
 
  buffer <- st_buffer(pov.waw.sf, dist = 0.0007)
  borders <- st_intersection(warsaw_districts.sf$osm_lines %>% 
                                   st_transform(crs = "+proj=longlat +datum=NAD83"), buffer)
  
  # ggplot picture + borders
  ggplot()+
    geom_sf(grid.sf, mapping = aes(geometry=geometry, fill=RES), col="transparent", alpha=0.5)+
    geom_sf(borders, mapping = aes(geometry=geometry))+
    scale_fill_viridis_c(option="magma", name="Population")+
    ggtitle("Population density in Warsaw")+
    theme_minimal()+
    theme(plot.title=element_text(hjust=0.5))
  
}

# Visualization - elderly + children
{
  
  elderly <- as.numeric(grid.df$RES_65_)/grid.df$RES*100
  children <- as.numeric(grid.df$RES_0_14)/grid.df$RES*100
  men <- as.numeric(grid.df$RES_MALE)/grid.df$RES*100
  
  # elderly
  ggplot()+
    geom_sf(grid.sf, mapping = aes(geometry=geometry, fill=elderly))+
    scale_fill_viridis_c(name="%")+
    ggtitle("Senior citizens - % of population")+
    theme_minimal()+
    theme(plot.title=element_text(hjust=0.5))
  
  # children
  ggplot()+
    geom_sf(grid.sf, mapping = aes(geometry=geometry, fill=children))+
    scale_fill_viridis_c(name="%")+
    ggtitle("Children - % of population")+
    theme_minimal()+
    theme(plot.title=element_text(hjust=0.5))
  
  # men
  ggplot()+
    geom_sf(grid.sf, mapping = aes(geometry=geometry, fill=men))+
    scale_fill_viridis_c(name="%")+
    ggtitle("Male residents - % of population")+
    theme_minimal()+
    theme(plot.title=element_text(hjust=0.5))
  
}

## Warsaw districts
{
  # Districts borders (in shapefile) are available at 
  # https://gis-support.pl/baza-wiedzy-2/dane-do-pobrania/granice-administracyjne/
  # download "Dzielnice Warszawy"
  districts<-readOGR('.','dzielnice_Warszawy')
  districts<-spTransform(districts, CRS("+proj=longlat +datum=NAD83"))
  plot(districts) #plot
  
  crds<-coordinates(grid.waw)
  crds.sp<-SpatialPoints(crds, proj4string= CRS("+proj=longlat +datum=NAD83"))

  over_results<-over(crds.sp, districts)  # which grid is in which polygon? (district name)
  grid.waw$district<-unlist(over_results)
  grid.sf<-st_as_sf(grid.waw)
  
  # plot districts
  districts.sf<-st_as_sf(districts)
  ggplot()+
    geom_sf(districts.sf, mapping = aes(geometry=geometry), fill="white", size=0.8, col="black")+
    theme_void()
  
  # plot grid + districts 
  grid.sf<-st_as_sf(grid.waw)
  ggplot()+
    geom_sf(grid.sf, mapping = aes(geometry=geometry, fill=district))+
    geom_sf(districts.sf, mapping = aes(geometry=geometry), fill="transparent", size=0.8, col="black")+
    theme_void()
  
  writeOGR(grid.waw,'.', 'districts_grid', driver = "ESRI Shapefile") # save with information about districts
}