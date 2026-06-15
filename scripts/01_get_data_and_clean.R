#### Produce 2025 Estimates for Oregon Metropolitan Planning Areas (for M.P. Organizations)
#### and Urban Areas (UAs produced by Census, MPAs delineated by MPOs)

#Portland State University
#Population Research Center

#by: Aja Sutton
#April 2026

#### load packages, set environment ####
{
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,data.table,sf,tidycensus,tigris,here,crsuggest,janitor,ggblend)


options(tigris_use_cache = TRUE)

tidycensus::census_api_key(Sys.getenv("CENSUS_API_KEY"), overwrite = FALSE, install = FALSE)

#DUE: APRIL 24, 2026

here::i_am('scripts/01_get_data_and_clean.R')
}


#Instructions for project:
# 1. Gather geographic datasets (last three most recent Decennial Census joined to block
# boundaries, County boundaries, MPO boundaries, UA boundaries)
# 2. Convert Census block polygons to centroids with population values
# 3. Split desired MPO and UA boundaries by County lines to create
# new MPO_by_Co and UA_by_Co datasets
# 4. Compare most recent resulting dataset boundaries to previous years' boundaries, note
# discrepancies/changes
# 5. Spatial summarize four versions of Census block population centroids
# into MPO, UA,  MPO_by_Co and UA_by_Co datasets 
 



#load MPO boundaries and UA boundaries, and
#identify the counties they fall within
#(easier to call API with for blocks later)

#MPO boundaries as of July 1, 2025 from Oregon GeoHub:
#https://geohub.oregon.gov/datasets/oregon-geo::metropolitan-planning-organizations/about
mpo20 = st_read(here('data/mpo2025/transgis_mpo_export.shp'))
table(mpo20$NAME)
# st_crs(mpo)
# suggest_crs(mpo) #6558 -- NAD83(2011) / Oregon North meters
mpo20 = st_transform(mpo20,6558)

# #from: https://geodata.bts.gov/datasets/usdot::metropolitan-planning-organizations/about

#MPOs were updated in 2009, 2012, 2013, 2015, 2017 and 2024 before the 2025 update.
#We take the 2009 for 2010 (most recent for that census) and 2017 for the 2020 (ditto).
mpo10 = st_read('data/mpo.gdb',layer = 'mpo_2009') %>% st_transform(6558)
#mpo20 = st_read('data/mpo.gdb',layer = 'mpo_2017') %>% st_transform(6558)

#st_layers('data/mpo.gdb')



excludelist= c('Longview/Kelso/Rainier',"Walla Walla Valley")
`%nin%` = Negate(`%in%`)
# mpo = mpo %>%
#   filter(NAME %nin% excludelist)
mpo20 = mpo20 %>%
  filter(NAME %nin% excludelist)
mpo10 = mpo10 %>%
  filter(NAME %nin% excludelist)


# plot(mpo$geometry,border="blue")
# plot(mpo2$geometry,border="red",add=TRUE)


#tidy up mpo object for easy match and clean later
mpo20 =
  mpo20 %>%
  select(NAME,COUNTY,geometry) %>%
  clean_names() %>%
  rename(mpo_county=county,
         mpo_name=name) %>%
  mutate(year="2020")

# mpo20 =
#   mpo20 %>%
#   select(NAME,COUNTY,Shape) %>%
#   clean_names() %>%
#   rename(mpo_county=county,
#          mpo_name=name,
#          geometry=Shape) %>%
#   mutate(year="2020")
mpo10 =
  mpo10 %>%
  select(NAME,COUNTY,Shape) %>%
  clean_names() %>%
  rename(mpo_county=county,
         mpo_name=name,
         geometry=Shape) %>%
  mutate(year="2010",
         mpo_county = ifelse(mpo_county == "Mult, Clack, Wash","Multnomah/Clackamas/Washington", mpo_county))


mpo = rbind(mpo10,mpo20)

mpo = mpo %>%
  filter(str_detect(mpo_name,'Albany|Bend|Corvallis|Eugene|Grants Pass|Middle Rogue|Rogue Valley|METRO|Salem')) %>%
  select(year,everything())

# mpo = mpo %>%
#   filter(str_detect(mpo_name,'Albany|Bend|Corvallis|Eugene|Grants Pass|Middle Rogue|Rogue Valley|METRO|Salem'))  %>%
#   mutate(mpo_name = case_when(str_detect(mpo_name,"Albany") ~ "Albany",
#                               str_detect(mpo_name,"Bend") ~ "Bend",
#                               str_detect(mpo_name,"Central Lane") ~ "Central Lane",
#                               str_detect(mpo_name,"Corvallis") ~ "Corvallis",
#                               str_detect(mpo_name,"Eugene/Springfield") ~ "Eugene/Springfield",
#                               str_detect(mpo_name,"Eugene-Springfield") ~ "Eugene/Springfield",
#                               str_detect(mpo_name,"Middle Rogue") ~ "Middle Rogue",
#                               str_detect(mpo_name,"Salem-Keizer") ~ "Salem-Keizer",
#                               str_detect(mpo_name,"Salem/Keizer") ~ "Salem-Keizer",
#                               str_detect(mpo_name,"Rogue Valley") ~ "Rogue Valley",
#                               str_detect(mpo_name,"METRO") ~ "Portland Metro"))

mpo %>%
  group_by(year) %>%
  distinct(mpo_name,mpo_county) %>%
  pivot_wider(names_from = year, values_from = mpo_county) %>%
    view

mpo %>%
  ggplot() +
  geom_sf(aes(colour= mpo_name),fill=NA) +
  facet_wrap(~year)

st_write(mpo,here('data/clean_mpo_boundaries.gpkg'),append=FALSE)



#county boundaries by year; let's start with the most recent, 2020
counties00 = counties(state = "OR",year=2000)
counties10 = counties(state = "OR",year=2010)
counties20 = counties(state = "OR",year=2020)
counties25 = counties20

# plot(counties00$geometry,border ="red")
# plot(counties10$geometry,border="blue",add=TRUE)
# plot(counties20$geometry,border="darkgreen",add=TRUE)

counties00$year = "2000"
counties10$year = "2010"
counties20$year = "2020"
counties25$year = "2025"

counties00 =
  counties00 %>%
  clean_names() %>%
  select(countyfp00,name00,geometry) %>%
  rename(cfips=countyfp00,
         cname=name00) %>%
  st_transform(6558) %>%
  mutate(year="2000")

counties10 =
  counties10 %>%
  clean_names() %>%
  select(countyfp10,name10,geometry) %>%
  rename(cfips=countyfp10,
         cname=name10) %>%
  st_transform(6558) %>%
  mutate(year="2010")

counties20 =
  counties20 %>%
  clean_names() %>%
  select(countyfp,name,geometry) %>%
  rename(cfips=countyfp,
         cname=name) %>%
  st_transform(6558) %>%
  mutate(year="2020")

counties25 =
  counties25 %>%
  clean_names() %>%
  select(countyfp,name,geometry) %>%
  rename(cfips=countyfp,
         cname=name) %>%
  st_transform(6558) %>%
  mutate(year="2025")

counties = rbind(counties00,counties10,counties20,counties25)
counties = counties %>%select(year,everything())

st_write(counties,here('data/clean_counties.gpkg'),append=FALSE)

# counties = st_read(here('data/clean_counties.gpkg'))
# mpos = st_read(here('data/clean_mpo_boundaries.gpkg'))

#only counties that fall into the MPO boundaries; simple way to disentangle
mpo_counties =
  st_filter(counties,mpo) %>%
  select(year,cname) %>%
  clean_names() %>%
  rename(name =cname) %>%
  distinct()
 
# mpo_counties %>%
#   ggplot() +
#   geom_sf() +
#   facet_wrap(~year)

#convert census block polygons to centroids with population values

#load_variables(2020,dataset = "pl")# %>% filter(concept=="TOTAL POPULATION") #%>% filter(str_detect(name,"P0"))

#specifying counties to 1. reduce file sizes to 
#2. keep the API from rejecting the call for being too big
blocks00 = get_decennial(geography = "block",state = "OR", county=paste0(unique(mpo_counties$name)),year = 2000,variables="P001001",geometry = TRUE) #sf1
blocks10 = get_decennial(geography = "block",state = "OR", county=paste0(unique(mpo_counties$name)), year = 2010,variables="P001001",geometry = TRUE) #sf1
blocks20 = get_decennial(geography = "block",state = "OR", county=paste0(unique(mpo_counties$name)), year = 2020,variables='P1_001N',geometry = TRUE) #from redistricting data, pl dataset
#same as 2020!
blocks25 = blocks20

blocks00$year = "2000"
blocks10$year = "2010"
blocks20$year = "2020"
blocks25$year = "2025"

blocks = rbind(blocks00,blocks10,blocks20,blocks25)
blocks = st_transform(blocks,4326)
st_geometry(blocks) = "geometry"

blocks = blocks %>%
  distinct()

st_write(blocks,here('data/clean_blocks/combined_blocks.gpkg'),append=FALSE)

# blocks = st_read(here('data/clean_blocks/combined_blocks.gpkg'))
# blocks = st_transform(blocks,6558)


# blocks %>%
#   st_as_sf() %>%
#   st_simplify() %>%
#   ggplot() +
#   geom_sf(aes(colour=year)) +
#   scale_colour_viridis_d() %>%
# ggsave(here('results/test_blocks_boundaries.png'))
# 
blockpoints =
  blocks %>%
  clean_names() %>%
  mutate(geometry = st_centroid(geometry)) %>%
  select(year,geoid,name,variable,value,geometry) %>%
  st_transform(4326) 

st_write(blockpoints,here('data/clean_blocks/combined_block_centroids.gpkg'),append=FALSE)

# blocks %>%
#   st_as_sf() %>%
#   ggplot() +
#     geom_sf(aes(colour=year,fill=NA)) +
#     scale_colour_viridis_d() +
#     theme_void()
# ggsave(here('results/test_blocks_boundaries_compareyears.png'), width = 10, height = 10, unit="in",dpi=350)



# Split desired MPO and UA boundaries by County lines to create
# new MPO_by_Co and UA_by_Co datasets

# tidy up ua objects and combine

#From Census Bureau's files:
#https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.2000.html#form-dropdown-1556094155
ua00 = st_read(here('data/ua2000/ua99_d00.shp'))
st_crs(ua00) =4326
ua00 = ua00 %>% filter(str_detect(NAME,"OR"),!str_detect(NAME,", WA"))

ua10 = st_read(here('data/ua2010/cb_2012_us_uac10_500k.shp'))
ua10 = ua10 %>% filter(str_detect(NAME10,", OR"),!str_detect(NAME10,", WA"))

 ua20 = st_read(here('data/ua2020/tl_2020_us_uac20.shp'))
 ua20 = ua20 %>% filter(str_detect(NAME20,", OR"),!str_detect(NAME20,", WA"))

#corrected 2020 most recent to 2025
# ua25 = st_read(here('data/ua2025/cb_2020_us_ua20_corrected_500k.shp'))
# ua25 = ua25 %>% filter(str_detect(NAME20,", OR"),!str_detect(NAME20,", WA"))

ua00 =
  ua00 %>%
  clean_names() %>%
  filter(lsad_trans == "Urbanized Area") %>%
  select(name,geometry) %>%
  mutate(year="2000") %>%
  st_transform(6558)


ua10 =
  ua10 %>%
  clean_names() %>%
  filter(str_detect(namelsad10,"Urbanized Area")) %>%
  select(name10,geometry) %>%
  rename(name=name10) %>%
  mutate(year="2010") %>%
  st_transform(6558)

ua20 = ua20 %>%
  clean_names() %>%
  filter(str_detect(namelsad20,"Urban Area")) %>%
  select(name20,geometry) %>%
  rename(name=name20) %>%
  mutate(year="2020") %>%
  st_transform(6558)

# ua25 = ua25 %>%
#   clean_names() %>%
#   filter(str_detect(namelsad20,"Urban Area")) %>%
#   select(name20,geometry) %>%
#   rename(name=name20) %>%
#   mutate(year="2025") %>%
#   st_transform(6558)

uas = rbind(ua00,ua10,ua20)

ua_names = c("Albany","Bend","Corvallis","Eugene","Grants Pass","Medford","Portland","Salem")
ua_names <- paste(ua_names, collapse = "|")
uas =
  uas %>%
  select(year,everything()) %>%
  filter(str_detect(name,ua_names))

uas = uas %>% st_make_valid()

st_write(uas,here('data/clean_UA_boundaries.gpkg'),append=FALSE)


uas %>%
  ggplot() +
  geom_sf() +
  facet_wrap(~year)

