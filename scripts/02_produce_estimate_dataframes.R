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
  
  here::i_am('scripts/02_produce_estimate_dataframes.R')
}


#Instructions for project:
# Gather geographic datasets (last three most recent Decennial Census 
# joined to block boundaries, County boundaries, MPO boundaries, UA boundaries)
#  Convert Census block polygons to centroids with population values
#  Split desired MPO and UA boundaries by County lines to create
# new MPO_by_Co and UA_by_Co datasets
#  Compare most recent resulting dataset boundaries to previous years' 
# boundaries, note discrepancies/changes
#  Spatial summarize four versions of Census block population centroids
# into MPO, UA,  MPO_by_Co and UA_by_Co datasets 


#### load clean data ####
{
blocks = st_read(here('data/clean_blocks/combined_block_centroids.gpkg'))
blocks = st_transform(blocks,6558)

#may be helpful to inspect if you want to make sure 
#the population totals are reasonable
# blocks %>%
#   group_by(year) %>%
#   reframe(sum=sum(value))

counties = st_read(here('data/clean_counties.gpkg'))

mpos = st_read(here('data/clean_mpo_boundaries.gpkg'))
mpos = mpos %>% filter(year =="2025") %>% mutate(year = "2020")

uas = st_read(here('data/clean_UA_boundaries.gpkg'))
uas = uas %>% filter(year =="2025") %>% mutate(year = "2020")

}


# #these are the years we have to cover below
# unique(mpos$year) #no 2000; MPOs didn't exist until 2009
# unique(uas$year)

# #check we have the right MPO and UA names
# unique(mpos$mpo_name) #no 2000; MPOs didn't exist until 2009
# unique(uas$ua_name)

parse_mpo_ua = function(blocks,mpos,uas,counties,yearid){

  #sf package won't let you do spatial operations if the CRSes don't match,
  #but this will stop the function before we get there -- more efficient.
    if (st_crs(mpos) != st_crs(uas)) stop("st_crs(mpos) != st_crs(uas)")
    if (st_crs(mpos) != st_crs(blocks)) stop("st_crs(mpos) != st_crs(blocks)")
    if (st_crs(mpos) != st_crs(counties)) stop("st_crs(mpos) != st_crs(counties)")
  
    blocks = blocks %>%
      filter(year==yearid) %>%
      st_as_sf() 
    counties = counties %>%
      filter(year==yearid) %>%
      st_as_sf()
    uas = uas %>%
      filter(year=="2020") %>%
      st_as_sf()
    mpos = mpos %>%
      filter(year=="2020") %>%
      st_as_sf()

    
    #blocks in MPO boundaries
    mpo_totals = st_intersection(blocks,mpos)
    
    #blocks in county boundaries
    mpo_by_co = st_intersection(mpo_totals,counties)

    mpo_by_co =
      mpo_by_co %>%
      select(year,mpo_name,cname,value) %>%
      group_by(year,mpo_name,cname) %>%
      reframe(estimate = sum(value)) %>%
      mutate(mpo_name = as.character(mpo_name),
             year= as.character(year),
             cname=as.character(cname),
             estimate = as.integer(estimate))

    fwrite(mpo_by_co,here(paste0('data/clean_csvs/mpo_by_co_',yearid,'.csv')))


    mpo_totals =
      mpo_totals %>%
      select(year,mpo_name,value) %>%
      group_by(year,mpo_name) %>%
      reframe(estimate = sum(value)) %>%
      mutate(mpo_name = as.character(mpo_name),
             year= as.character(year),
             estimate = as.integer(estimate))

    fwrite(mpo_totals,here(paste0('data/clean_csvs/mpo_totals_',yearid,'.csv')))

    
    #blocks in UA boundaries
    ua_totals = st_intersection(blocks,uas)
    #blocks in county boundaries
    ua_by_co = st_intersection(ua_totals,counties)

    ua_totals =
      ua_totals %>%
      select(year,ua_name,value) %>%
      group_by(year,ua_name) %>%
      reframe(estimate = sum(value))

    ua_by_co =
      ua_by_co %>%
      select(year,ua_name,cname,value) %>%
      group_by(year,ua_name,cname) %>%
      reframe(estimate = sum(value))

    fwrite(ua_totals,here(paste0('data/clean_csvs/ua_totals_',yearid,'.csv')))
    fwrite(ua_by_co,here(paste0('data/clean_csvs/ua_by_co_',yearid,'.csv')))

}

#CSVs only
years = c("2000","2010","2020")

for (i in 1:length(years)){
  print(years[i])
 parse_mpo_ua(blocks,mpos,uas,counties,yearid=years[i])
}

#NB: Eugene is not an MPO in the most recent boundary release from the FTA,
#but it is still a UA (those are census boundaries).

#### join CSVs ####

#get all CSVs and bring each data type together for reporting
temp <- list.files(
  path = here(paste0(here("data/clean_csvs"))),
  pattern = "*\\.csv$",
  full.names = TRUE)

#make sure that you don't have old files from a previous run in above if you
#believe there are errors, because this will explicitly pull all csv files 
#from clean_csvs unless you're more specific.

#index the map_dfr() calls by inspecting files (this is why we add yearid to 
#the end of the file name in the function call; makes for easy indexing):
temp

#MPOs have two files each == 2010, 2020
mpo_by_co = map_dfr(temp[1:3],
                    fread)

mpo_totals = map_dfr(temp[4:6],
                     fread)

#UAs have three files each == 2000, 2010, 2020
ua_by_co = map_dfr(temp[7:9],
                   fread)

ua_totals = map_dfr(temp[10:12],
                    fread)


#format the data and save in results
mpo_by_co %>%
  arrange(desc(year)) %>%
  pivot_wider(names_from = year,values_from = estimate) %>%
  rename(county_name=cname,
         population_2000=`2000`,
         population_2010=`2010`,
         population_2020=`2020`) %>%
  fwrite(here('results/clean/2025_mpo_by_co.csv'))

ua_by_co %>%
  arrange(desc(year)) %>%
  arrange(ua_name) %>%
  pivot_wider(names_from = year,values_from = estimate) %>% 
  rename(county_name=cname,
         population_2000=`2000`,
         population_2010=`2010`,
         population_2020=`2020`) %>% 
  fwrite(here('results/clean/2025_ua_by_co.csv'))


#identify boundary changes by hand in code following this (make maps),
#then can update the significant_boundary_change = case_when() call 
#below manually
mpo_totals %>%
  arrange(desc(year)) %>%
  pivot_wider(names_from = year,values_from = estimate) %>%
  rename(population_2000=`2000`,
         population_2010=`2010`,
         population_2020=`2020`) %>%
  mutate(significant_boundary_change = case_when(mpo_name =="Bend" ~"YES",  #verified by hand below
                                                 mpo_name =="Corvallis" ~"NO",
                                                 mpo_name =="Albany" ~"NO",
                                                 mpo_name =="Middle Rogue" ~"NO",
                                                 mpo_name =="Rogue Valley" ~"NO",
                                                 mpo_name =="Central Lane" ~"NO",
                                                 mpo_name =="Portland Metro" ~"NO",
                                                 mpo_name =="Salem-Keizer" ~"NO",
                                                 mpo_name =="Eugene/Springfield" ~"YES")
         ) %>%
  fwrite(here('results/clean/2025_mpo_totals.csv'))

ua_totals %>%
  arrange(desc(year)) %>%
  pivot_wider(names_from = year,values_from = estimate) %>%
  rename(population_2000=`2000`,
         population_2010=`2010`,
         population_2020=`2020`) %>%
  mutate(significant_boundary_change = "NO") %>% #verified by hand below
  fwrite(here('results/clean/2025_ua_totals.csv'))


#### Compare boundary changes: 2025 vs 2024 ####
#boundary changes for uas
#we really want to know if they have changed since 2020
#reload ua geo data with correct years
uas = st_read(here('data/clean_UA_boundaries.gpkg'))
uas %>%
  filter(year=="2024"|year=="2025") %>%
  ggplot() +
  geom_sf(aes(colour=year),fill=NA) +
  scale_colour_viridis_d() +
  geom_sf_text(data=uas %>% group_by(ua_name) %>% slice_head(n = 1),aes(label=ua_name),size=1) +
  theme_void()
ggsave(here('results/compare_boundaries/uas.png'),width = 10,height=5,unit="in",dpi=350)

# 
# ggplot() +
#   geom_sf(data=counties %>% filter(cname %in% ua_by_co$cname), colour="grey10",fill=NA) +
#   #  geom_sf_text(data=uas %>% select(ua_name,geometry) %>% distinct,aes(label=ua_name)) +
#   geom_sf(data=uas %>% filter(year=="2000") %>% select(geom), aes(colour="2000"),fill=NA) +
#   geom_sf(data=uas %>% filter(year=="2010") %>% select(geom), aes(colour="2010"),fill=NA) +
#   geom_sf(data=uas %>% filter(year=="2020") %>% select(geom), aes(colour="2020"),fill=NA) +
#   scale_colour_manual(values = rev(c("goldenrod","darkred","darkblue"))) +
#   geom_sf_text(data =counties%>% filter(cname %in% ua_by_co$cname),aes(label=cname),size=1) +
#   geom_sf_text(data=uas %>% select(ua_name,geom) %>% distinct(),aes(label=ua_name),size=1) +
#   theme_void()
# 
# ggsave(here('results/compare_boundaries/uas_by_county_2000_2010_2020.png'),width = 10,height=10,unit="in",dpi=350)
# 
# ggplot() +
#   geom_sf(data=counties %>% filter(cname %in% ua_by_co$cname, year!="2025"), colour="grey10",fill=NA) +
#   #  geom_sf_text(data=uas %>% select(ua_name,geometry) %>% distinct,aes(label=ua_name)) +
#   geom_sf(data=uas, aes(colour=year),fill=NA) +
#   scale_colour_manual(values = rev(c("goldenrod","darkred","darkblue"))) +
#   geom_sf_text(data =counties%>% filter(cname %in% ua_by_co$cname, year!="2025") %>% distinct(),aes(label=cname),size=1) +
#   geom_sf_text(data=uas %>% filter(year=="2020") %>% distinct(),aes(label=ua_name),size=1) +
#   geom_sf_text(data=uas %>% filter(year=="2010") %>% distinct(),aes(label=ua_name),size=1) +
#   geom_sf_text(data=uas %>% filter(year=="2000") %>% group_by(ua_name) %>% slice_head(n = 1),aes(label=ua_name),size=1) +
#   theme_void() +
#   facet_wrap(~year)
# ggsave(here('results/compare_boundaries/uas_by_county_2000_2010_2020_simple.png'),width = 10,height=10,unit="in",dpi=350)
# 
# ggplot() +
#   geom_sf(data=counties %>% filter(cname %in% ua_by_co$cname), colour="grey10",fill=NA) +
#   #  geom_sf_text(data=uas %>% select(ua_name,geometry) %>% distinct,aes(label=ua_name)) +
#   geom_sf(data=mpos %>% filter(year=="2010"), aes(colour="2010"),fill=NA) +
#   geom_sf(data=mpos %>% filter(year=="2020"), aes(colour="2020"),fill=NA) +
#   geom_sf(data=mpos %>% filter(year=="2025"), aes(colour="2025"),fill=NA) +
#   scale_colour_manual(values = rev(c("goldenrod","darkred","darkblue"))) +
#   geom_sf_text(data =counties%>% filter(cname %in% ua_by_co$cname),aes(label=cname),size=1) +
#   geom_sf_text(data=mpos %>% filter(year=="2010"),aes(label=mpo_name),size=1) +
#   geom_sf_text(data=mpos %>% filter(year=="2020"),aes(label=mpo_name),size=1) +
#   geom_sf_text(data=mpos %>% filter(year=="2025"),aes(label=mpo_name),size=1) +
#   theme_void() 
# ggsave(here('results/compare_boundaries/mpo_by_county_2010_2020_2025.png'),width = 10,height=10,unit="in",dpi=350)



#MPO boundaries as of July 1, 2025 from Oregon GeoHub:
#https://geohub.oregon.gov/datasets/oregon-geo::metropolitan-planning-organizations/about
mpos = st_read(here('data/clean_mpo_boundaries.gpkg'))

mpos %>%
  filter(year=="2024"|year=="2025") %>%
  ggplot() +
  geom_sf(aes(colour=year),fill=NA) +
  scale_colour_viridis_d() +
  geom_sf_text(data=mpos %>% group_by(mpo_name) %>% slice_head(n = 1),aes(label=mpo_name),size=1) +
  theme_void()
ggsave(here('results/compare_boundaries/mpos.png'),width = 10,height=5,unit="in",dpi=350)


# 
# ggplot() +
#   geom_sf(data=counties %>% filter(cname %in% ua_by_co$cname, year!="2000"), colour="grey10",fill=NA) +
#   #  geom_sf_text(data=uas %>% select(ua_name,geometry) %>% distinct,aes(label=ua_name)) +
#   geom_sf(data=mpos %>% filter(year!="2000"), aes(colour=year),fill=NA) +
#   scale_colour_manual(values = rev(c("goldenrod","darkred","darkblue"))) +
#   geom_sf_text(data =counties%>% filter(cname %in% ua_by_co$cname, ,year!="2000"),aes(label=cname),size=1) +
#   geom_sf_text(data=mpos %>% filter(year=="2025"),aes(label=mpo_name),size=1) +
#   geom_sf_text(data=mpos %>% filter(year=="2020"),aes(label=mpo_name),size=1) +
#   geom_sf_text(data=mpos %>% filter(year=="2010"),aes(label=mpo_name),size=1) +
#   theme_void() +
#   facet_wrap(~year)
# ggsave(here('results/compare_boundaries/mpo_by_county_2010_2020_simple.png'),width = 10,height=10,unit="in",dpi=350)



