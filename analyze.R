library(magrittr)

# all EMs ex China with 2021 GDP > $100bln
thecountries <- weo_c %>% 
  left_join(countries, by = "ISO") %>% 
  filter(`Emerging market and developing economies`, ISO != "CHN") %>% 
  filter(!(ISO %in% c("UKR", "IRQ", "QAT"))) %>% 
  filter(year == 2021, NGDPD > 100) %>% 
  arrange(desc(NGDPD)) %>% 
  pull(ISO)
countries %>% filter(ISO %in% thecountries) %>%  
  select(ISO, Country, EM_Export) %>% 
  print(n=34)



# compare growth rates and C/A of commodity-exporting EMs to other EMs
weo_c %>% 
  filter(ISO %in% thecountries) %>% 
  left_join(select(countries, ISO, EM_Export), by = "ISO") %>% 
  mutate(episode = 0) %>% 
  mutate(episode = ifelse(year %in% 2014:2019, 1, episode)) %>% 
  mutate(episode = ifelse(year %in% 2020     , 2, episode)) %>% 
  mutate(episode = ifelse(year %in% 2021:2022, 3, episode)) %>% 
  #filter(episode != 0) %>% 
  filter_index(1995 ~ 2022) %>% 
  group_by(EM_Export) %>% 
  summarize(GDP = sum(NGDPD), C.A = sum(BCA)) %T>%
  {print(ggplot(., aes(year, GDP, color = EM_Export)) + geom_line())} %>% 
  ggplot(aes(year, C.A, color = EM_Export)) + geom_line()


