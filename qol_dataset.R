#############################################################

#      The datasets creation (OSM + NDVI + transport)       #

#############################################################

# Starter
{
  library(tidyverse)
  library(sf)
  library(rgdal) #readOGR
  library(sp)
  library(GISTools)
  library(ggplot2)
  library(osmdata)
  # devtools::install_github("e-dobrowolska/p2p")
  library(p2p) # pts2poly, poly2poly_i, poly2poly_a - my own package
  
  setwd(...) #path to the folder with data (in this case, Warsaw grid)  
  
  WAW_box <- getbb("Warsaw, Poland")
  WAW_box[1,2]<-21.27115 # getbb gets one coordinate wrong 
  
  grid.waw<-readOGR(".", "grid_waw_2021") #created in the file "warsaw_grid.R"
  grid.df<-as.data.frame(grid.waw)
  grid.sf<-st_as_sf(grid.waw)
  grid<-as(grid.waw, "SpatialPolygons")
  grid<-spTransform(grid, CRS("+proj=longlat +datum=NAD83"))
  
  
  # The data for Polish poviats is available at 
  # https://gis-support.pl/baza-wiedzy-2/dane-do-pobrania/granice-administracyjne/
  # download "Powiaty"
  pow.sf <- st_read("powiaty.shp")
  pow.sf <- st_transform(pow.sf, crs = "+proj=longlat +datum=NAD83")
  waw.pow.sf <- pow.sf %>% filter(jpt_nazwa_=='powiat Warszawa')
  
  df = grid.df[,c("ID", "RES")] # a base - we will add variables regarding facilities here 
}

### 1. OSM data - primary facilities ###
{
  # Convenience store, fast food, pharmacy - polygons + points outside polygons
  pts_poly <- data.frame(keys = c("shop", "amenity", "amenity"),
                                values = c("convenience", "fast_food", "pharmacy"))
  
  for(i in 1:nrow(pts_poly)){
    points <- pts2poly(pts_poly$keys[i],pts_poly$values[i], grid.sf, outside=T, multi=T)
    polygons <- poly2poly_i(pts_poly$keys[i], pts_poly$values[i], grid.sf, multi=T)
    df[,pts_poly$values[i]]<-points[,-1]+polygons[,-1]
  }
  
  # Soccer, school - only polygons (points are noise/falsely reported data)
  poly <- data.frame(keys = c("sport", "amenity"),
                       values = c("soccer", "school"))
  
  for(i in 1:nrow(poly)){
    polygons <- poly2poly_i(poly$keys[i], poly$values[i], grid.sf, multi=T)
    df[,poly$values[i]]<-polygons[,-1]
  }
  
  # Cell's area covered with forest/park
  area_nature = poly2poly_a(c("landuse", "leisure"), c("forest", "park"), grid.sf)
  df$area_nature<-area_nature$forest+area_nature$park

}

### 2. OSM data - binary POI ###
###    Secondary, tertiary, additional facilities
###    Facilities in the category 'other'
{
  # We need a data frame of keys & values for OSM queries.
  # I chose manually the ones I needed, you can use 
  # available_features() - for the list of available keys
  # available_tags(feature) - for the list of available values (for the given key)
  # or search online: https://wiki.openstreetmap.org/wiki/Map_features

    amenity = c("bar", "cafe", "ice_cream", "pub", "restaurant", "college", "kindergarten",
                  "language_school", "library", "music_school", "university", "bicycle_rental",
                  "boat_rental", "car_rental", "car_wash", "fuel", "parking", "taxi", "atm", "bank", 
                  "dentist", "doctors", "hospital", "veterinary", "arts_centre", "casino", 
                  "cinema", "community_centre", "fountain", "social_centre", "theatre", "police", "post_office",
                  "prison", "townhall", "waste_disposal", "animal_shelter", "childcare", "grave_yard", "internet_cafe",
                  "marketplace", "place_of_worship", "vending_machine","driving_school", "bureau_de_change",
                "clinic", "nightclub", "planetarium", "courthouse", "fire_station", "parcel_locker", "toy_library",
                "biergarten","food_court", "charging_station", "vehicle_inspection", "car_sharing", "conference_centre",
                "events_venue", "gambling", "studio", "drinking_water", "telephone", "shelter",
                "recycling", "ferry_terminal", "give_box", "bbq")
    
    craft = c("carpenter", "electrician", "jeweller", "optician", "photographer", "printer", "shoemaker", "tailor")
    
    historic = c("archaeological_site", "building", "castle", "monument", "ruins", "tower")
    
    leisure = c("adult_gaming_centre", "beach_resort", "dance", "dog_park", "escape_game",
                "fitness_centre", "garden", "horse_riding", "ice_rink", "marina", "miniature_golf",
                "playground", "sports_centre", "stadium", "swimming_pool", "water_park", "amusement_arcade",
                "bandstand", "bowling_alley", "disc_golf_course", "fishing", "nature_reserve",
                "sauna", "trampoline_park")
    
    office = c("accountant", "architect", "charity", "company", "diplomatic", "engineer", 
               "financial", "government", "it", "lawyer", "newspaper", "political_party",
               "security", "tax_advisor", "travel_agent")
    
    shop = c("alcohol", "bakery", "butcher", "coffee", "chocolate", "cheese",
              "greengrocer", "health_food", "tea", "supermarket", "baby_goods", "boutique",
              "clothes", "jewelry", "shoes", "second_hand", "hairdresser", "tattoo", "florist", "hardware",
              "furniture", "antiques", "electronics", "sports", "art", "games", "books", "newsagent",
              "stationery", "laundry", "outpost", "party", "pet", "toys", "tobacco", "kiosk", 
              "beverages", "brewing_supplies", "confectionery", "deli", "dairy", "frozen_food",
              "pasta", "pastry", "seafood", "spices", "wine", "water", "department_store", "mall",
              "bag", "fabric", "fashion_accessories", "leather", "sewing", "watches", "tailor",
              "variety_store", "beauty", "chemist", "cosmetics", "hearing_aids", "herbalist",
              "medical_supply", "nutrition_supplements", "perfumery", "agrarian", "appliance",
              "doityourself", "electrical", "garden_centre", "glaziery", "houseware", "security",
              "paint", "bed", "candles", "carpet", "curtain", "doors", "flooring",
              "kitchen", "lighting", "tiles", "window_blind", "computer","mobile_phone",
              "radiotechnics", "vacuum_cleaner", "atv", "bicycle", "car", "car_parts",
              "caravan", "fuel", "fishing", "golf", "hunting", "jetski", "military_surplus",
              "motorcycle", "outdoor", "scuba_diving", "ski", "snowmobile", "sports", "tyres",
              "trailer", "art", "camera", "collector", "craft", "frame", "games", "model", "music",
              "musical_instrument", "video", "video_games", "anime", "books", "gift",
              "lottery", "stationery", "ticket", "bookmaker", "copyshop", "dry_cleaning", 
              "e-cigarette", "weapons")
    
    healthcare = c("alternative", "audiologist", "birthing_center", "blood_bank",
                   "blood_donation", "counselling", "dialysis", "hospice", "laboratory",
                   "midwife", "nurse", "occupational_therapist", "optometrist", 
                   "physiotherapist", "podiatrist", "psychotherapist", "rehabilitation",
                   "sample_collection", "speech_therapist", "vaccination_centre")
    
    sport<-c("athletic", "basketball", "badminton", "fitness", "gymnastics", "judo", "karate", "archery",
             "ultimate", "volleyball", "tennis", "9pin", "10pin", "american_football", "aikido", "athletics", 
             "australian_football", "bandy", "baseball", "beachvolleyball", "billiards",
             "bmx", "boules", "bobsleigh", "bowls", "boxing", "canadian_football",
             "canoe", "chess", "cliff_diving", "climbing", "climbing_adventure",
             "cricket", "crossfit", "croquet", "curling", "cycle_polo", "cycling",
             "darts", "dog_agility", "dog_racing", "equestrian", "fencing", "field_hockey",
             "five-a-side", "floorball", "four_square", "free_flying", "futsal", "handball",
             "hapkido", "ice_skating", "ice_hockey", "horse_racing", "ice_stock", "karting",
             "kickboxing", "kitesurfing", "korfball", "lacrosse", "obstacle_course", 
             "orienteering", "paddle_tennis", "parachuting", "parkour", "pickleball",
             "pilates", "pole_dance", "racquet", "rc_car", "roller_skating", "rowing",
             "running", "sailing", "scuba_diving", "shooting", "shot-put", "skateboard",
             "skiing", "snooker", "squash", "sumo", "table_tennis", "table_soccer",
             "taekwondo", "water_polo", "weightlifting", "wrestling", "yoga")
    
    tourism<-c("artwork", "attraction", "gallery", "hostel", "guest_house", "hotel","zoo",
               "information", "motel", "museum", "picnic_site", "theme_park", "viewpoint")
    
    public_transport<-c("ferry_terminal")
    
    railway<-"subway_entrance"
    
    landuse <- "flowerbed"
    
    natural <- c("hill", "stone")
    
    data = data.frame(key = c(rep("amenity", length(amenity)),
                              rep("craft", length(craft)),
                              rep("historic", length(historic)),
                              rep("leisure", length(leisure)),
                              rep("office", length(office)),
                              rep("shop", length(shop)),
                              rep("healthcare", length(healthcare)),
                              rep("sport", length(sport)),
                              rep("tourism", length(tourism)),
                              rep("public_transport", length(public_transport)),
                              rep("railway", length(railway)),
                              rep("landuse", length(landuse)),
                              rep("natural", length(natural))
    ), 
    value = c(amenity, craft, historic, leisure, office, shop, healthcare, sport, tourism, public_transport, railway, landuse, natural))
    
    # Now we extract the information about the presence/absence of the facility in the cell: 
    
    binary_facilities<-pts2poly(data$key, data$value, grid.sf, sleep=8, binary=T)
    df<-merge(df, binary_facilities, by="ID")
    
}

### 3. OSM data - Nature ###
{
  meadow <- poly2poly_i(key = "landuse", value = "meadow", grid.sf, binary=T, minsize=250)
  df$meadow <- meadow$meadow
  
  water_body <- poly2poly_i(key = "natural", value = "water", grid.sf, binary=T, minsize=50)
  df$water_body <- water_body$water
}

### 4. NDVI data - city's "greenness" ###
{
  # "ndvi_2023.csv" - see https://rpubs.com/iv3e/ndvi, or the file "ndvi.Rmd" on github
  # satelite imagery files  - https://scihub.copernicus.eu/
  ndvi<-read.csv("ndvi_2023.csv")
  ndvi<-ndvi[,3:5] # only the columns we need
  ndvi<-ndvi %>% distinct(X, Y, .keep_all=TRUE) # remove duplicates
  
  ndvi.sp<-SpatialPoints(ndvi[,1:2], proj4string=CRS("+proj=longlat +datum=NAD83"))
  ndvi.sp$NDVI<-ndvi$NDVI
  
  ndvi$grid<-over(ndvi.sp, grid) # which point in which grid cell?
  ndvi$is_green<-ifelse(ndvi$NDVI>0.5, 1, 0) # NDVI>0.5 means that the point is green
  
  total_points <- ndvi %>% # group points by grid cells
    group_by(grid) %>% 
    summarize(total_ndvi_points = n())
  
  total_points<-as.data.frame(total_points)
  colnames(total_points)<-c("ID", "total_ndvi_points")
  
  green_points<-aggregate(ndvi$is_green, by=list(ndvi$grid), sum, na.rm=TRUE) # sum green points in cells
  colnames(green_points)<-c("ID", "green_ndvi_points")
  
  ndvi_df = data.frame(ID=1:dim(grid.df)[1])
  
  ndvi_df<-merge(ndvi_df, total_points, by="ID",  all.x=TRUE, sort=TRUE)
  ndvi_df$total_ndvi_points[is.na(ndvi_df$total_ndvi_points)]<-0
  
  ndvi_df<-merge(ndvi_df, green_points, by="ID",  all.x=TRUE, sort=TRUE)
  ndvi_df$green_ndvi_points[is.na(ndvi_df$green_ndvi_points)]<-0
  
  ndvi_df$green_pct<-ifelse(ndvi_df$total_ndvi_points>0, ndvi_df$green_ndvi_points/ndvi_df$total_ndvi_points, 0)
  df$green_ndvi<-ifelse(ndvi_df$green_pct>0.25, 1, 0) # the cell is green if > 25% of its points is green
}

### 5. Public transport data ###
{
  # data available at 
  # https://openmobilitydata.org/p/ztm-warszawa/720/latest
  
  txt_locations<-read.csv("stops.txt")
  txt_time<-read.csv("stop_times.txt")
  
  #locations of stops
  stop_locations<-data.frame(x=txt_locations$stop_lon, y=txt_locations$stop_lat, stop_id=txt_locations$stop_id)
  
  total_departures <- txt_time %>% # group departures by stops
    group_by(stop_id) %>% 
    summarize(departures = n())
  
  total_departures <- total_departures %>% as.data.frame()
  
  locations_departures<-merge(stop_locations, total_departures, by="stop_id") # location + departures
  locations_departures<-locations_departures[,-1] # we dont need id
  
  stops.sp<-SpatialPoints(locations_departures[,1:2], proj4string=CRS("+proj=longlat +datum=NAD83"))
  stops.sp$departures<-locations_departures$departures
  
  stops.sp$grid<-over(stops.sp, grid) # which stop in which grid cell?
  
  stops.ag<-aggregate(stops.sp$departures, by=list(stops.sp$grid), sum) # sum departures in grid cells
  colnames(stops.ag)<-c("ID", "departures")
  
  df<-merge(df, stops.ag, by="ID", all.x=T)
  df$departures[is.na(df$departures)] = 0
}

### 6.  Save the data ###
{
  new_grid<-SpatialPolygonsDataFrame(grid, df, match.ID = F)
  writeOGR(new_grid, ".", "dataset_qol", driver = "ESRI Shapefile")
}







