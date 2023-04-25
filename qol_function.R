#############################################################

#         Index function & Visualisations in Warsaw         #

#############################################################

# Starter
{
  library(tidyverse)
  library(sf)
  library(rgdal)
  library(sp)
  library(ggplot2)
  library(osmdata)
  library(ggplot2)
  library(spdep)
  library(gridExtra)
  library(raster)
  
  setwd("...")
  
  grid_qol<-readOGR(".", "dataset_qol")
  grid.df<-as.data.frame(grid_qol)
  grid.sf<-st_as_sf(grid_qol)
  grid.sp<-as(grid_qol, "SpatialPolygons")
}

# QOL index - function for sub-indices
{
  index_qol<-function(dataset, PopulationVariable, primary, secondary, tertiary, additional, WeightNeighbours=.5, WeightPrimary=9, WeightSecondary=0.3, WeightTertiary=0.2, WeightAdditional=0.1, AdditionalLimit =0.3, PerCapita=T, Neighbours){
    
    # prepare the data
    # a "base" we will add components of the subindex here and later add them up
    dt<-data.frame(ID=1:nrow(dataset), RES=dataset[,PopulationVariable])
    dt<-cbind(dt, dataset[,c(primary,secondary, tertiary, additional)], by="ID")
    
    # for the neighbours
    dtNeighbours<-data.frame(ID=1:nrow(dataset), RES=dataset[,PopulationVariable])
    
    for(variable in c(primary, secondary, tertiary, additional)){
      dtNeighbours[,variable]<-unlist(lapply(seq.int(1, nrow(dt)), function(n) sum(dt[Neighbours[[n]],variable])))
    }
    
    # primary facilities (i_{cat,1})
    if(PerCapita){
      primary.pc<-ifelse(dt$RES>0, dt[,primary]/dt$RES, dt[,primary])
      primary.Neighbours.pc<-unlist(lapply(seq.int(1, nrow(dt)), function(i) sum(primary.pc[Neighbours[[i]]])))
      dt$primary.corrected<-primary.pc+primary.Neighbours.pc*WeightNeighbours
    }else{
      dt$primary.corrected<-dt[,primary]+dtNeighbours[,primary]*WeightNeighbours
    } 
    
    treshold<-quantile(dt$primary.corrected, 0.75)
    dt$ind.primary<-ifelse(dt$primary.corrected>treshold, WeightPrimary, ceiling(dt$primary.corrected/treshold*WeightPrimary))
    
    # Secondary and tertiary facilities
    dt$ind.secondary<-ifelse(dt[,secondary]>0, WeightSecondary, ifelse(dtNeighbours[,secondary]>0, WeightSecondary*WeightNeighbours, 0))
    dt$ind.tertiary1<-ifelse(dt[,tertiary[1]]>0, WeightTertiary, ifelse(dtNeighbours[,tertiary[1]]>0, WeightTertiary*WeightNeighbours, 0))
    dt$ind.tertiary2<-ifelse(dt[,tertiary[2]]>0, WeightTertiary, ifelse(dtNeighbours[,tertiary[2]]>0, WeightTertiary*WeightNeighbours, 0))
    
    # additional facilities
    sum_additional = rowSums(dt[,additional])
    sum_additional_neighbours = rep(0, nrow(dtNeighbours))
    
    for(variable in additional){
      sum_additional_neighbours<-sum_additional_neighbours+ifelse(dt[,variable]==0, ifelse(dtNeighbours[,variable]>0, 1,0), sum_additional_neighbours)
    }
    dt$ind.additional<-pmin(sum_additional*WeightAdditional+sum_additional_neighbours*WeightAdditional*WeightNeighbours, AdditionalLimit)
    
    # The sub-index is the sum of the components 
    index<-rowSums(dt[,c("ind.primary", "ind.secondary", "ind.tertiary1", "ind.tertiary2", "ind.additional")])
    return(index)
  }
}

# Facilities in categories
{
  # Dining
  Primary.dining<-"fast_fd"
  Secondary.dining<-"restrnt"
  Tertiary.dining<-c("ice_crm", "cafe")
  Additional.dining<-c("bar", "pub", "birgrtn","fod_crt")
  
  # Transport
  Primary.transport<-"deprtrs"
  Secondary.transport<-"sbwy_nt"
  Tertiary.transport<-c("parking", "bcycl_r")
  Additional.transport<-c("bt_rntl", "cr_rntl", "taxi")
  
  # Healthcare "scl_fcl","nrsng_h"
  Primary.health<-"pharmcy"
  Secondary.health<-"doctors"
  Tertiary.health<-c("clinic", "dentist")
  Additional.health<-c("hospitl", "vetrnry","altrntv","bld_dnt","cnsllng","labrtry","occptn_","optmtrs",
                       "physthr","podtrst","psychth", "rhblttn","smpl_cl","spch_th")
  
  # Shopping
  Primary.shopping<-"convnnc"
  Secondary.shopping<-"grngrcr"
  Tertiary.shopping<-c("bakery", "sprmrkt")
  Additional.shopping<-c("butcher","coffee","chocolt","cheese","hlth_fd","tea","bby_gds","boutiqu","clothes","jewelry","shoes",
                         "scnd_hn", "florist", "hardwar","furnitr", "newsgnt", "antiqus","elctrnc", "alcohol","bevergs","cnfctnr","deli",
                         "dairy","pastry","seafood","spices","wine","dprtmn_","mall","bag","fabric","fshn_cc","leather","sewing","watches",
                         "vrty_st","beauty","chemist","cosmtcs","hrng_ds","herblst","mdcl_sp","ntrtn_s","perfmry","applinc","dtyrslf","elctrcl",
                         "grdn_cn","glaziry","housewr","securty","paint","bed","carpet","curtain","doors","floorng","kitchen","lightng",
                         "wndw_bl","computr","mbl_phn","rdtchnc","bicycle","car","cr_prts","caravan","fishing","mltry_s","mtrcycl","outdoor",
                         "scb_dvn","ski","tyres","camera","collctr","craft","frame","model","music","mscl_ns","video","vid_gms","anime","gift",
                         "lottery","statnry","ticket","bookmkr","copyshp","dry_cln","weapons")
  
  # Sport
  Primary.sport<-"soccer"
  Secondary.sport<-"fitness"
  Tertiary.sport<-c("bsktbll", "vllybll")
  Additional.sport<-c("badmntn","judo","tennis","gymnstc", "sports", "swmmng_", "stadium","sprts_c", "X9pin","X10pin","amrcn_f",
                      "archery","athltcs","basebll","bchvlly","billrds","boules","boxing","canoe","chess","climbng","clmbng_",
                      "cricket","crossft","cycling","equstrn","fencing","handbll","ic_sktn","ic_hcky","hrs_rcn","karting","pol_dnc",
                      "racquet","rc_car","rllr_sk","rowing","running","sailing","shootng","skatbrd","skiing","squash",
                      "tbl_tnn","tbl_scc","yoga")
  
  # Education
  Primary.education<-"school"
  Secondary.education<-"kndrgrt"
  Tertiary.education<-c("library", "unvrsty")
  Additional.education<-c("drvng_s","lngg_sc", "msc_sch", "childcr", "college")
  
  # Nature
  Primary.nature<-"are_ntr"
  Secondary.nature<-"grn_ndv"
  Tertiary.nature<-c("meadow", "wtr_bdy")
  Additional.nature<-c("flowrbd","hill", "stone")
  
  # Other 
  other<-c("chrgng_",  "vhcl_ns",  "cr_shrn",  "cnfrnc_",  "evnts_v",  "studio",   "drnkng_", 
           "shelter",  "rcyclng",  "intrnt_",  "frry_tr",  "give_bx",  "bbq",      "bndstnd",  "bwlng_l",
           "ntr_rsr",  "sauna",    "stadium",  "gvrnmnt",   "diplmtc",  "car_wsh", 
           "fuel",     "bank",     "atm",      "police",   "theatre",  "fountan",  "cinema",   "casino",   "plc_f_w", 
           "pcnc_st",  "infrmtn",  "motel",    "museum",   "thm_prk",  "viewpnt",  "zoo",      "artwork",  "br_d_ch", 
           "nghtclb",  "plantrm",  "courths",  "fr_sttn",  "amsmnt_",  "hotel",    "gust_hs",  "hostel",   "attrctn", 
           "castle",   "monumnt",  "ruins",    "adlt_g_",  "bch_rsr",  "dance",    "dog_prk",  "escp_gm",  "ftnss_c", 
           "garden",   "hrs_rdn",  "ice_rnk",  "marina",   "mntr_gl",  "plygrnd",  "archlg_",  "townhll",  "gallery",
           "pst_ffc", "hrdrssr", "prcl_lc", "outpost", "carpntr","elctrcn","jewellr","opticin","phtgrph","shoemkr","tailor", 
           "accntnt","archtct", 'enginer', "it", "lawyer", "tx_dvsr","trvl_gn", "tattoo", "laundry")
  
}

# Visualization
{
  Neighbours<-poly2nb(grid.sp)
  
  # Per-capita indices
  {
    Index = data.frame(ID = grid.df$ID)
    Index$dining <- index_qol(grid.df, "RES", Primary.dining, Secondary.dining, Tertiary.dining, Additional.dining, Neighbours=Neighbours)
    Index$transport <- index_qol(grid.df, "RES", Primary.transport, Secondary.transport, Tertiary.transport, Additional.transport, Neighbours=Neighbours)
    Index$health <- index_qol(grid.df, "RES", Primary.health, Secondary.health, Tertiary.health, Additional.health, Neighbours=Neighbours)
    Index$shopping <- index_qol(grid.df, "RES", Primary.shopping, Secondary.shopping, Tertiary.shopping, Additional.shopping, Neighbours=Neighbours)
    Index$sport <- index_qol(grid.df, "RES", Primary.sport, Secondary.sport, Tertiary.sport, Additional.sport, Neighbours=Neighbours)
    Index$education <- index_qol(grid.df, "RES", Primary.education, Secondary.education, Tertiary.education, Additional.education, Neighbours=Neighbours)
    Index$nature <- index_qol(grid.df, "RES", Primary.nature, Secondary.nature, Tertiary.nature, Additional.nature, Neighbours=Neighbours)
    
    SumsOther <- rowSums(grid.df[,other])
    Index$other <- SumsOther/max(SumsOther)*10
    
    Index$general <- rowMeans(Index[,-1])
    
    # General Index
    GeneralPlot<-ggplot()+
      geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=Index$general))+
      scale_fill_viridis_c(guide=guide_legend(title="Index value"), limits=c(0, 10), breaks=seq(0, 10, by=1))+
      ggtitle("QOLI")+
      theme_minimal()+
      theme(plot.title=element_text(hjust=0.5))
    
    # Dining Index
    DiningPlot<- ggplot()+
      geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=Index$dining))+
      scale_fill_viridis_c(guide=guide_legend(title="Index value"), limits=c(0, 10), breaks=seq(0, 10, by=1))+
      ggtitle("Dining Index")+
      theme_minimal()+
      theme(plot.title=element_text(hjust=0.5))
    
    # Transport Index
    TransportPlot <- ggplot()+
      geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=Index$transport))+
      scale_fill_viridis_c(guide=guide_legend(title="Index value"), limits=c(0, 10), breaks=seq(0, 10, by=1))+
      ggtitle("Transport Index")+
      theme_minimal()+
      theme(plot.title=element_text(hjust=0.5))
    
    # Healthcare Index
    HealthcarePlot <- ggplot()+
      geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=Index$health))+
      scale_fill_viridis_c(guide=guide_legend(title="Index value"), limits=c(0, 10), breaks=seq(0, 10, by=1))+
      ggtitle("Healthcare Index")+
      theme_minimal()+
      theme(plot.title=element_text(hjust=0.5))
    
    # Shopping Index
    ShoppingPlot <- ggplot()+
      geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=Index$shopping))+
      scale_fill_viridis_c(guide=guide_legend(title="Index value"), limits=c(0, 10), breaks=seq(0, 10, by=1))+
      ggtitle("Shopping Index")+
      theme_minimal()+
      theme(plot.title=element_text(hjust=0.5))
    
    # Sport Index
    SportPlot <- ggplot()+
      geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=Index$sport))+
      scale_fill_viridis_c(guide=guide_legend(title="Index value"), limits=c(0, 10), breaks=seq(0, 10, by=1))+
      ggtitle("Sport Index")+
      theme_minimal()+
      theme(plot.title=element_text(hjust=0.5))
    
    # Education Index
    EducationPlot <- ggplot()+
      geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=Index$education))+
      scale_fill_viridis_c(guide=guide_legend(title="Index value"), limits=c(0, 10), breaks=seq(0, 10, by=1))+
      ggtitle("Education Index")+
      theme_minimal()+
      theme(plot.title=element_text(hjust=0.5))
    
    # Nature Index
    NaturePlot <- ggplot()+
      geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=Index$nature))+
      scale_fill_viridis_c(guide=guide_legend(title="Index value"), limits=c(0, 10), breaks=seq(0, 10, by=1))+
      ggtitle("Nature Index")+
      theme_minimal()+
      theme(plot.title=element_text(hjust=0.5))
    
    # Others Index
    OthersPlot <- ggplot()+
      geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=Index$other))+
      scale_fill_viridis_c(guide=guide_legend(title="Index value"), limits=c(0, 10), breaks=seq(0, 10, by=1))+
      ggtitle("Others Index")+
      theme_minimal()+
      theme(plot.title=element_text(hjust=0.5))
    
    MultiPlot<-grid.arrange(GeneralPlot, DiningPlot, TransportPlot, HealthcarePlot, ShoppingPlot, SportPlot, EducationPlot, NaturePlot, OthersPlot)
    MultiPlot
    
  }  
  
  # Non-Per-capita indices
  {
    Index = data.frame(ID = grid.df$ID)
    Index$dining <- index_qol(grid.df, "RES", Primary.dining, Secondary.dining, Tertiary.dining, Additional.dining, Neighbours=Neighbours, PerCapita = F)
    Index$transport <- index_qol(grid.df, "RES", Primary.transport, Secondary.transport, Tertiary.transport, Additional.transport, Neighbours=Neighbours, PerCapita = F)
    Index$health <- index_qol(grid.df, "RES", Primary.health, Secondary.health, Tertiary.health, Additional.health, Neighbours=Neighbours, PerCapita = F)
    Index$shopping <- index_qol(grid.df, "RES", Primary.shopping, Secondary.shopping, Tertiary.shopping, Additional.shopping, Neighbours=Neighbours, PerCapita = F)
    Index$sport <- index_qol(grid.df, "RES", Primary.sport, Secondary.sport, Tertiary.sport, Additional.sport, Neighbours=Neighbours, PerCapita = F)
    Index$education <- index_qol(grid.df, "RES", Primary.education, Secondary.education, Tertiary.education, Additional.education, Neighbours=Neighbours, PerCapita = F)
    Index$nature <- index_qol(grid.df, "RES", Primary.nature, Secondary.nature, Tertiary.nature, Additional.nature, Neighbours=Neighbours, PerCapita = F)
    
    SumsOther <- rowSums(grid.df[,intersect(other, names(grid.df))])
    Index$other <- SumsOther/max(SumsOther)*10
    
    Index$general <- rowMeans(Index[,-1])
    
    # General Index
    GeneralPlot<-ggplot()+
      geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=Index$general))+
      scale_fill_viridis_c(guide=guide_legend(title="Index value"), limits=c(0, 10), breaks=seq(0, 10, by=1))+
      ggtitle("QOLI")+
      theme_minimal()+
      theme(plot.title=element_text(hjust=0.5))
    
    # dining Index
    DiningPlot<- ggplot()+
      geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=Index$dining))+
      scale_fill_viridis_c(guide=guide_legend(title="Index value"), limits=c(0, 10), breaks=seq(0, 10, by=1))+
      ggtitle("Dining Index")+
      theme_minimal()+
      theme(plot.title=element_text(hjust=0.5))
    
    # Transport Index
    TransportPlot <- ggplot()+
      geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=Index$transport))+
      scale_fill_viridis_c(guide=guide_legend(title="Index value"), limits=c(0, 10), breaks=seq(0, 10, by=1))+
      ggtitle("Transport Index")+
      theme_minimal()+
      theme(plot.title=element_text(hjust=0.5))
    
    # Healthcare Index
    HealthcarePlot <- ggplot()+
      geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=Index$health))+
      scale_fill_viridis_c(guide=guide_legend(title="Index value"), limits=c(0, 10), breaks=seq(0, 10, by=1))+
      ggtitle("Healthcare Index")+
      theme_minimal()+
      theme(plot.title=element_text(hjust=0.5))
    
    # Shopping Index
    ShoppingPlot <- ggplot()+
      geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=Index$shopping))+
      scale_fill_viridis_c(guide=guide_legend(title="Index value"), limits=c(0, 10), breaks=seq(0, 10, by=1))+
      ggtitle("Shopping Index")+
      theme_minimal()+
      theme(plot.title=element_text(hjust=0.5))
    
    # Sport Index
    SportPlot <- ggplot()+
      geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=Index$sport))+
      scale_fill_viridis_c(guide=guide_legend(title="Index value"), limits=c(0, 10), breaks=seq(0, 10, by=1))+
      ggtitle("Sport Index")+
      theme_minimal()+
      theme(plot.title=element_text(hjust=0.5))
    
    # Education Index
    EducationPlot <- ggplot()+
      geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=Index$education))+
      scale_fill_viridis_c(guide=guide_legend(title="Index value"), limits=c(0, 10), breaks=seq(0, 10, by=1))+
      ggtitle("Education Index")+
      theme_minimal()+
      theme(plot.title=element_text(hjust=0.5))
    
    # Nature Index
    NaturePlot <- ggplot()+
      geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=Index$nature))+
      scale_fill_viridis_c(guide=guide_legend(title="Index value"), limits=c(0, 10), breaks=seq(0, 10, by=1))+
      ggtitle("Nature Index")+
      theme_minimal()+
      theme(plot.title=element_text(hjust=0.5))
    
    # Others Index
    OthersPlot <- ggplot()+
      geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=Index$other))+
      scale_fill_viridis_c(guide=guide_legend(title="Index value"), limits=c(0, 10), breaks=seq(0, 10, by=1))+
      ggtitle("Others Index")+
      theme_minimal()+
      theme(plot.title=element_text(hjust=0.5))
    
    MultiPlot<-grid.arrange(GeneralPlot, DiningPlot, TransportPlot, HealthcarePlot, ShoppingPlot, SportPlot, EducationPlot, NaturePlot, OthersPlot)
    MultiPlot
  }  
  
  
  ### Histogram ###
  ggplot(Index, aes(x=general))+
    geom_histogram(fill="dodgerblue4", binwidth = 0.5)+
    stat_function(fun = function(x) 
      dnorm(x, mean = mean(Index$general), sd = sd(Index$general))*0.5*601, col="red", size=0.8)+
    ggtitle("Index Distribution")+
    xlab("General Index Value")+
    ylab("Number of cells")+
    theme_minimal()+
    theme(plot.title=element_text(hjust=0.5))
  
  ### QOL vs distance ###
  crds <- coordinates(grid)
  centre<-SpatialPoints(data.frame(x=21.01506, y=52.2309))
  sp<-SpatialPoints(crds)
  Index$d_centre<-pointDistance(sp, centre, T)
  
  ggplot(Index, aes(x=d_centre, y=general))+
    geom_point(col="dodgerblue4")+
    ggtitle("")+
    xlab("Distance from the city centre in meters")+
    ylab("General Index")+
    theme_minimal()+
    theme(plot.title=element_text(hjust=0.5))
  
  cor(Index$general, Index$d_centre) # -0.7295435
}

## Statistics table for the districts
{
  districts <- readOGR( '.', 'districts_grid') # file created in "warsaw_grid.R"
  Index$district<-districts$district
  
  Index.summary<-Index %>% 
    group_by(district) %>%
    summarize(mean_general = round(mean(general),2),
              mean_dining = round(mean(dining),2),
              mean_transport = round(mean(transport),2),
              mean_health = round(mean(health),2),
              mean_shopping = round(mean(shopping),2),
              mean_sport = round(mean(sport),2),
              mean_education = round(mean(education),2),
              mean_nature = round(mean(nature),2),
              mean_other = round(mean(other),2))
  
  Index.summary <- Index.summary[order(-Index.summary$mean_general),]
  write.csv(Index.summary, 'index_summary.csv') 
}





