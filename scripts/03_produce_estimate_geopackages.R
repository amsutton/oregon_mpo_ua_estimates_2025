#### Produce 2025 Estimates for Oregon Metropolitan Planning Areas (for M.P. Organizations)
#### and Urban Areas (UAs produced by Census, MPAs delineated by MPOs)

#Portland State University
#Population Research Center

#by: Aja Sutton
#June 2026

#### load packages, set environment ####
{
  if (!require("pacman")) install.packages("pacman")
  
  pacman::p_load(tidyverse,data.table,sf,here,janitor)
  
  here::i_am('scripts/03_produce_estimate_geopackages.R')
}

#Instructions for project:
# Gather geographic datasets (last three most recent Decennial Census joined to block
#                             boundaries, County boundaries, MPO boundaries, UA boundaries)
#  Convert Census block polygons to centroids with population values
#  Split desired MPO and UA boundaries by County lines to create
# new MPO_by_Co and UA_by_Co datasets
#  Compare most recent resulting dataset boundaries to previous years' boundaries, note
#   discrepancies/changes
#  Spatial summarize four versions of Census block population centroids
# into MPO, UA,  MPO_by_Co and UA_by_Co datasets 


{
  blocks = st_read(here('data/clean_blocks/combined_block_centroids.gpkg'))
  blocks = st_transform(blocks,6558)
  
  counties=st_read(here('data/clean_counties.gpkg'))
  counties = st_transform(counties,6558)
  
  mpos = st_read(here('data/clean_mpo_boundaries.gpkg'))
  mpos = mpos %>%
    filter(year=="2025",
           str_detect(mpo_name,'Albany|Bend|Corvallis|Eugene|Grants Pass|Middle Rogue|Rogue Valley|METRO|Salem'))  %>%
    mutate(year ="2020",
           mpo_name = case_when(str_detect(mpo_name,"Albany") ~ "Albany",
                                str_detect(mpo_name,"Bend") ~ "Bend",
                                str_detect(mpo_name,"Central Lane") ~ "Central Lane",
                                str_detect(mpo_name,"Corvallis") ~ "Corvallis",
                                str_detect(mpo_name,"Eugene/Springfield") ~ "Eugene/Springfield",
                                str_detect(mpo_name,"Eugene-Springfield") ~ "Eugene/Springfield",
                                str_detect(mpo_name,"Middle Rogue") ~ "Middle Rogue",
                                str_detect(mpo_name,"Salem-Keizer") ~ "Salem-Keizer",
                                str_detect(mpo_name,"Salem/Keizer") ~ "Salem-Keizer",
                                str_detect(mpo_name,"Rogue Valley") ~ "Rogue Valley",
                                str_detect(mpo_name,"METRO") ~ "Portland Metro")) %>%
    st_transform(6558)
  
  uas = st_read(here('data/clean_UA_boundaries.gpkg'))
  uas = uas %>%
    filter(year=="2025",
           str_detect(ua_name,'Albany|Bend|Corvallis|Eugene|Grants Pass|Medford|Portland|Salem')) %>%
    mutate(year = "2020") %>%
    st_transform(6558)
}

#please note the st_write() command has append=FALSE to overwrite previous version of the gpkg! 
#removing this code in all four save points in this function when files are already present 
#will error out the function call. append=TRUE will add the new version of the data to the old file
#regardless of whether similar data exist (i.e. you can create duplicates this way, careful!).

build_split_mpo_ua_geom_boundaries = function(blocks,mpos,uas,counties,year_of_estimates){
  
  if (st_crs(mpos) != st_crs(uas)) stop("st_crs(mpos) != st_crs(uas)")
  if (st_crs(mpos) != st_crs(blocks)) stop("st_crs(mpos) != st_crs(blocks)")
  if (st_crs(mpos) != st_crs(counties)) stop("st_crs(mpos) != st_crs(counties)")
  

  uas = uas %>%
    filter(year=="2020") %>%
    st_as_sf()
  mpos = mpos %>%
    filter(year=="2020") %>%
    st_as_sf()
  
  
    if(FALSE %in% st_is_valid(mpos)) mpos = st_make_valid(mpos)
    if(FALSE %in% st_is_valid(mpos)) stop("invalid MPO geometry persists")
    
  yearid = c('2000','2010','2020')

  mpo_totals = data.frame()
  mpo_by_co = data.frame()

  for(i in 1:length(yearid)){
    print(yearid[i])
    
    temp_blocks = blocks %>%
      filter(year==yearid[i]) %>%
      st_as_sf()
    temp_counties = counties %>%
      filter(year==yearid[i]) %>%
      st_as_sf()
    
    #blocks in MPO boundaries
    mpo_totals2 = st_join(mpos,temp_blocks)
    mpo_totals = rbind(mpo_totals,mpo_totals2)
    
    #blocks in county boundaries
    blocks_in_mpos = st_intersection(mpos,temp_blocks)
    mpo_by_co2 = st_join(temp_counties,blocks_in_mpos)
    
    mpo_by_co = rbind(mpo_by_co,mpo_by_co2)
    
  }
    
    mpo_by_co %>%
      rename(year=year.x) %>%
      select(year,mpo_name,cname,value,geom) %>% 
      na.omit() %>%
      group_by(year,mpo_name,cname) %>%
      reframe(estimate = sum(value),
              geom) %>% 
      distinct() %>% 
      st_as_sf() %>%
      st_collection_extract(type = "POLYGON") %>%
      st_transform(4326) %>%
      st_write(here(paste0('data/clean_mpo_ua_joint_boundaries/mpo_by_co_',year_of_estimates,'.gpkg')),append=FALSE)
    
    mpo_totals %>%
      rename(year=year.x) %>%
      select(year,mpo_name,value,geom) %>%
      na.omit() %>%
      group_by(year,mpo_name) %>%
      reframe(estimate = sum(value),
              geom) %>% 
      distinct() %>%
      st_as_sf() %>%
      st_collection_extract(type = "POLYGON") %>%
      st_transform(4326) %>%
      st_write(here(paste0('data/clean_mpo_ua_joint_boundaries/mpo_totals_',year_of_estimates,'.gpkg')),append=FALSE)


    
    ua_totals = data.frame()
    ua_by_co = data.frame()
    
    for(i in 1:length(yearid)){
      print(yearid[i])
      
      temp_blocks = blocks %>%
        filter(year==yearid[i]) %>%
        st_as_sf()
      temp_counties = counties %>%
        filter(year==yearid[i]) %>%
        st_as_sf()
      
      #blocks in ua boundaries
      ua_totals2 = st_join(uas,temp_blocks)
      ua_totals = rbind(ua_totals,ua_totals2)
      
      #blocks in county boundaries
      blocks_in_uas = st_intersection(uas,temp_blocks)
      ua_by_co2 = st_join(temp_counties,blocks_in_uas)
      
      ua_by_co = rbind(ua_by_co,ua_by_co2)
      
    }
    
   
    ua_totals %>%
      rename(year=year.x) %>%
      select(year,ua_name,value,geom) %>%
      na.omit() %>%
      group_by(year,ua_name) %>%
      reframe(estimate = sum(value),
              geom) %>% 
      distinct() %>%
      st_as_sf() %>%
      st_collection_extract(type = "POLYGON") %>%
      st_transform(4326) %>%
      st_write(here(paste0('data/clean_mpo_ua_joint_boundaries/ua_totals_',year_of_estimates,'.gpkg')),append=FALSE)
    
    ua_by_co %>%
      rename(year=year.x) %>%
      select(year,ua_name,cname,value,geom) %>%
      na.omit() %>%
      group_by(year,ua_name,cname) %>%
      reframe(estimate = sum(value),
              geom) %>% 
      distinct() %>%
      st_as_sf() %>%
      st_collection_extract(type = "POLYGON") %>%
      st_transform(4326) %>%
      st_write(here(paste0('data/clean_mpo_ua_joint_boundaries/ua_by_co_',year_of_estimates,'.gpkg')),append=FALSE)
  
}


#N.B.: If during the below loop you see that the geometry type after writing each
#gpkg is Point, or especially Unknown, you may have an issue! 
#Look into geometry validity (st_is_valid)
#of the geometries used to create the new objects, and also consider whether
#there are "slivers" or similar nonsense geometries (can usually be dealt with)
#in combination with searching for them, and 
#using st_collection_extract(x,type = "POLYGON")

year_of_estimates = "2025"

#this could be made to be more memory efficient, but it isn't bad
build_split_mpo_ua_geom_boundaries(blocks,mpos,uas,counties,year_of_estimates)




#### join geopackages into one ####

#we could've done this in the function above, 
#but this way we can add other things if we want in the future.
#just a bit more explicit.

#get all smaller geopackages and build a main geopackage containing all of them
temp <- list.files(
  path = here(paste0(here("data/clean_mpo_ua_joint_boundaries"))),
  pattern = "*\\.gpkg$", 
  full.names = TRUE) 

layernames <- list.files(
  path = here(paste0(here("data/clean_mpo_ua_joint_boundaries"))),
  pattern = "*\\.gpkg$", 
  full.names = FALSE) 
layernames = str_split_i(layernames,pattern = ".gpkg",i=1)

#had to manually create first layer to produce .gpkg file
firstlayer = st_read(temp[1]) 

firstlayer %>%
  st_write(here('results/clean/2025_oregon_mpo_ua_boundaries.gpkg'),layer=paste0(layernames[1]),append=FALSE)

st_layers(here('results/clean/2025_oregon_mpo_ua_boundaries.gpkg'))

layernames = layernames[2:length(layernames)]
temp = temp[2:length(temp)]


for (i in 1:length(temp)){
  
  layer = st_read(temp[i])
  
  layer %>%
    st_write(here('results/clean/2025_oregon_mpo_ua_boundaries.gpkg'),layer=paste0(layernames[i]),append=FALSE)
}

#looks good; crs = 4326 for simplicity
st_layers(here('results/clean/2025_oregon_mpo_ua_boundaries.gpkg'))
temp = st_read(here('results/clean/2025_oregon_mpo_ua_boundaries.gpkg'),layer='mpo_by_co_2025')
glimpse(temp)

