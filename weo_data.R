library(tidyverse)
library(readxl)
library(tsibble)

countries <- read_csv("/home/ulrich/R/data/countries.csv", na = "n/a") %>% 
  mutate(`EM Net External Position` = factor(`EM Net External Position`, 
                                             levels = c("Creditor", "Debtor")),
         Income = factor(Income, 
                         c("high", "upper-middle", "lower-middle", "low"),
                         ordered = TRUE),
         EM_Export = factor(ifelse(`Fuel Exporting EM`,"fuel",
                              ifelse(`Nonfuel Primary Products Exporting EM`,"nonfuel.primary",
                                ifelse(`Emerging market and developing economies`,"secondary",
                                  NA))),
                            c("fuel", "nonfuel.primary", "secondary"))) %>% 
  select(!c(`Fuel Exporting EM`,`Nonfuel Primary Products Exporting EM`))

groups <- read_xlsx("/home/ulrich/R/data/WEOOct2022alla.xlsx", 
                 na = c("", "n/a", "--")) %>%
  select(`WEO Country Group Code`, `Country Group Name`) %>% 
  distinct() %>% 
  arrange(`WEO Country Group Code`)

subjects_c <- read_xlsx("/home/ulrich/R/data/WEOOct2022all.xlsx", 
                      na = c("", "n/a", "--")) %>% 
  select(`WEO Subject Code`, `Subject Descriptor`, `Subject Notes`, Units, 
         Scale) %>% 
  distinct() %>% 
  arrange(`WEO Subject Code`)

subjects_g <- read_xlsx("/home/ulrich/R/data/WEOOct2022alla.xlsx", 
                       na = c("", "n/a", "--")) %>% 
  select(`WEO Subject Code`, `Subject Descriptor`, `Subject Notes`, Units, 
         Scale) %>% 
  distinct() %>% 
  arrange(`WEO Subject Code`)

weo_c <- read_xlsx("/home/ulrich/R/data/WEOOct2022all.xlsx", 
                       na = c("", "n/a", "--")) %>% 
  pivot_longer(as.character(seq(1980, 2027)), 
               names_to = "year", values_to = "value") %>% 
  select(ISO, `WEO Subject Code`, year, value) %>% 
  pivot_wider(names_from = `WEO Subject Code`, values_from = value) %>% 
  mutate(NOMEX = NGDP / NGDPD) %>% 
  mutate(year = as.integer(year)) %>% 
  as_tsibble(index = year, key = ISO)

weo_g <- read_xlsx("/home/ulrich/R/data/WEOOct2022alla.xlsx", 
                    na = c("", "n/a", "--")) %>% 
  pivot_longer(as.character(seq(1980, 2027)), 
               names_to = "year", values_to = "value") %>% 
  select(`WEO Country Group Code`, `WEO Subject Code`, year, value) %>% 
  pivot_wider(names_from = `WEO Subject Code`, values_from = value) %>% 
  mutate(year = as.integer(year)) %>% 
  as_tsibble(index = year, key = `WEO Country Group Code`)

