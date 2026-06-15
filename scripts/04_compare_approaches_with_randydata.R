{
  if (!require("pacman")) install.packages("pacman")
  
  pacman::p_load(tidyverse,data.table,sf,tidycensus,tigris,here,crsuggest,janitor,ggblend)
  
  
  options(tigris_use_cache = TRUE)
  
  tidycensus::census_api_key(Sys.getenv("CENSUS_API_KEY"), overwrite = FALSE, install = FALSE)
  
  #DUE: APRIL 24, 2026
  
  here::i_am('scripts/04_compare_approaches_with_randydata.R')
}


#compare approach 1, approach 2, and Randy's estimates

#load approach 1 data
temp <- list.files(
  path = here(paste0(here("data/clean_csvs"))),
  pattern = "*\\.csv$",
  full.names = TRUE)

mpo_by_co = map_dfr(temp[1:2],
                    fread)

mpo_by_co$descrip = "MPO by County"
mpo_by_co$source = "Approach 1"

mpo_totals = map_dfr(temp[3:4],
                     fread)

mpo_totals$descrip = "MPO Totals"
mpo_totals$source = "Approach 1"
mpo_totals$cname = NA
mpo_totals = mpo_totals %>% select(colnames(mpo_by_co))

ua_by_co = map_dfr(temp[5:7],
                   fread)

ua_by_co$descrip = "UA by County"
ua_by_co$source = "Approach 1"

ua_totals = map_dfr(temp[8:10],
                    fread)

ua_totals$descrip = "UA Totals"
ua_totals$source = "Approach 1"
ua_totals$cname = NA
ua_totals = ua_totals %>% select(colnames(ua_by_co)) 

approach1_ua = rbind(ua_totals,ua_by_co)
approach1_mpo = rbind(mpo_totals,mpo_by_co)

approach1_mpo = approach1_mpo %>% rename(name=mpo_name)
approach1_ua = approach1_ua %>% rename(name=ua_name)

approach1 = rbind(approach1_mpo,approach1_ua)

#load approach 2 data
temp <- list.files(
  path = here(paste0(here("data/clean_csvs/2025 boundaries only"))),
  pattern = "*\\.csv$",
  full.names = TRUE)

mpo_by_co = map_dfr(temp[1:3],
                    fread)

mpo_by_co$descrip = "MPO by County"
mpo_by_co$source = "Approach 2"

mpo_totals = map_dfr(temp[4:6],
                     fread)

mpo_totals$descrip = "MPO Totals"
mpo_totals$source = "Approach 2"
mpo_totals$cname = NA
mpo_totals = mpo_totals %>% select(colnames(mpo_by_co))

ua_by_co = map_dfr(temp[7:9],
                   fread)

ua_by_co$descrip = "UA by County"
ua_by_co$source = "Approach 2"

ua_totals = map_dfr(temp[10:12],
                    fread)

ua_totals$descrip = "UA Totals"
ua_totals$source = "Approach 2"
ua_totals$cname = NA
ua_totals = ua_totals %>% select(colnames(ua_by_co)) 

approach2_ua = rbind(ua_totals,ua_by_co)
approach2_mpo = rbind(mpo_totals,mpo_by_co)

approach2_mpo = approach2_mpo %>% rename(name=mpo_name)
approach2_ua = approach2_ua %>% rename(name=ua_name)

approach2 = rbind(approach2_mpo,approach2_ua)

#load Randy's data

ua = fread('data/clean_csvs/randy estimates/randy_ua_estimates.csv')
mpo = fread('data/clean_csvs/randy estimates/randy_mpo_estimates.csv')

ua =
  ua %>% 
  pivot_longer(cols = `Pop 2020`:`Pop 2000`,names_to = "year",values_to = "estimate") %>%
  mutate(year = str_remove_all(year,"Pop "),
         cname= ifelse(is.na(cname),NA,cname)) %>%
  select(colnames(approach2))

mpo =
  mpo %>% 
  pivot_longer(cols = `Pop 2020`:`Pop 2000`,names_to = "year",values_to = "estimate") %>%
  mutate(year = str_remove_all(year,"Pop "),
         cname= ifelse(is.na(cname),NA,cname)) %>%
  select(colnames(approach2))

randy = rbind(mpo,ua)


dat = rbind(approach1,approach2,randy)


mpo_totals  =
  dat %>%
  filter(str_detect(descrip,"MPO Totals")) %>%
  group_by(name,cname,descrip) %>%
  pivot_wider(names_from = year, values_from = estimate) %>%
  select(descrip, source, name,cname,`2020`,`2010`,`2000`)  %>%
  mutate(name = ifelse(str_detect(name,"Portland|METRO"),"Portland",name),
         name = ifelse(str_detect(name,"Salem"),"Salem-Keizer",name),
         name = str_remove_all(name,", OR"),
         cname= ifelse(is.na(cname),NA,cname)) %>%
    arrange(descrip,name)

mpobyco  =
  dat %>%
  filter(str_detect(descrip,"MPO by County")) %>%
  group_by(name,cname,descrip) %>%
  pivot_wider(names_from = year, values_from = estimate) %>%
  select(descrip, source, name,cname,`2020`,`2010`,`2000`)  %>%
  mutate(name = ifelse(str_detect(name,"Portland|METRO"),"Portland",name),
         name = ifelse(str_detect(name,"Salem"),"Salem-Keizer",name),
         name = str_remove_all(name,", OR"),
         cname= ifelse(is.na(cname),NA,cname)) %>%
  arrange(descrip,name,cname) 

ua_totals  =
  dat %>%
  filter(str_detect(descrip,"UA Totals")) %>%
  group_by(name, descrip) %>%
  pivot_wider(names_from = year, values_from = estimate) %>%
  select(descrip, source, name,cname,`2020`,`2010`,`2000`)  %>%
  mutate(name = ifelse(str_detect(name,"Portland|METRO"),"Portland",name),
         name = ifelse(str_detect(name,"Salem"),"Salem-Keizer",name),
         name = str_remove_all(name,", OR"),
         cname= ifelse(is.na(cname),NA,cname)) %>%
  arrange(descrip,name)

uabyco  =
  dat %>%
  filter(str_detect(descrip,"UA by County")) %>%
  group_by(name, descrip) %>%
  pivot_wider(names_from = year, values_from = estimate) %>%
  select(descrip, source, name,cname,`2020`,`2010`,`2000`)  %>%
  mutate(name = ifelse(str_detect(name,"Portland|METRO"),"Portland",name),
         name = ifelse(str_detect(name,"Salem"),"Salem-Keizer",name),
         name = str_remove_all(name,", OR"),
         cname= ifelse(is.na(cname),NA,cname)) %>%
  arrange(descrip,name,cname)
  
dat = rbind(mpo_totals,mpobyco,ua_totals,uabyco) 
dat = dat %>% ungroup()
fwrite(dat,here('data/compare_approaches_with_randydata.csv'))

