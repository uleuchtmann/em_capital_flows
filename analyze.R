library(tidyverse)
library(tsibble)
library(magrittr)

source("get_data.R")
source("indices.R")

countries <- get_countries()
weo_c <- get_county_data()


# define country groups
thegroups <- countries %>% 
  mutate(group = "em_lowincome") %>% 
  mutate(group = if_else(`Advanced economies`, "advanced", group)) %>% 
  mutate(group = if_else(`Emerging market and developing economies` & 
                           EM_Export == "fuel" & 
                           Income != "low",
                         "em_fuel", group)) %>% 
  mutate(group = if_else(`Emerging market and developing economies` & 
                           EM_Export != "fuel" & 
                           Income != "low",
                         "em_nonfuel", group)) %>% 
  mutate(group = if_else(ISO %in% c("CHN", "HKG", "MAC"), "china_prc", group)) %>% 
  select(ISO, group)

thegroups2 <- countries %>% 
  mutate(group = "em_lowincome") %>% 
  mutate(group = if_else(`Advanced economies`, "advanced", group)) %>% 
  mutate(group = if_else(`Emerging market and developing economies` & 
                           Income != "low",
                         "em_other", group)) %>% 
  mutate(group = if_else(ISO %in% c("CHN", "HKG", "MAC"), "china_prc", group)) %>% 
  select(ISO, group)

thegroups3 <- countries %>% 
  mutate(group = "em_exchina") %>% 
  mutate(group = if_else(`Advanced economies`, "advanced", group)) %>% 
  mutate(group = if_else(ISO %in% c("CHN", "HKG", "MAC"), "china_prc", group)) %>% 
  select(ISO, group)

  
# identify missing data 2010-2022
# real GDP
missing_gdp <- weo_c %>% 
  filter_index(2010 ~ 2022) %>% 
  select(NGDP_R, NGDPD) %>% 
  filter(!complete.cases(.)) %>% 
  pull(ISO) %>%  unique()
# current account
missing_ca <- weo_c %>% 
  filter_index(2010 ~ 2022) %>% 
  select(BCA, NGDPD) %>% 
  filter(!complete.cases(.)) %>% 
  pull(ISO) %>%  unique()
# investment
missing_inv <- weo_c %>% 
  filter_index(2010 ~ 2022) %>% 
  select(NID_NGDP, NGDPD) %>% 
  filter(!complete.cases(.)) %>% 
  pull(ISO) %>%  unique()
# investment
missing_sav <- weo_c %>% 
  filter_index(2010 ~ 2022) %>% 
  select(NGSD_NGDP, NGDPD) %>% 
  filter(!complete.cases(.)) %>% 
  pull(ISO) %>%  unique()


# real GDP levels per country groups
weo_c %>% 
  filter(!(ISO %in% c(missing_gdp))) %>% 
  filter_index(2010 ~ 2022) %>% 
  left_join(thegroups3, by = "ISO") %>% 
  group_by(group) %>% 
  tornqvist(NGDP_R, NGDPD, index_name = "GDP") %T>%
  {print(ggplot(., aes(year, GDP, color = group)) + geom_line())} %>% 
  print(n=100) %>% 
  write_excel_csv("GDP-ems.csv")

# current account in dollars per country group
weo_c %>% 
  filter(!(ISO %in% c(missing_gdp, missing_ca))) %>% 
  filter_index(2010 ~ 2022) %>% 
  left_join(thegroups, by = "ISO") %>% 
  group_by(group) %>% 
  summarize(CA = sum(BCA)) %T>%
  {print(ggplot(., aes(year, CA, color = group)) + geom_line())} %>% 
  print(n=100) %>% 
  write_excel_csv("CA-per-country-group.csv")

# saving-investment ratio per country group
weo_c %>% 
  filter(!(ISO %in% c(missing_gdp, missing_ca, 
                      missing_inv, missing_sav))) %>% 
  filter_index(2010 ~ 2022) %>% 
  left_join(thegroups, by = "ISO") %>% 
  mutate(sav_inv = NGSD_NGDP / NID_NGDP) %>% 
  group_by(group) %>% 
  wsum(sav_inv, NGDPD, index_name = "saving_investment") %T>% 
  {print(ggplot(., aes(year, saving_investment, color = group)) + geom_line())} %>% 
  print(n=100) %>% 
  write_excel_csv("savinv-per-country-group.csv")
  
