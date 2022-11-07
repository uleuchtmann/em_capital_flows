get_countries <- function(path = "./data/") {
  readr::read_csv(paste0(path, "countries.csv"), na = "n/a") %>% 
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
}

get_groups <- function(path = "./data/") {
  readxl::read_xlsx(paste0(path, "WEOOct2022alla.xlsx"), 
            na = c("", "n/a", "--")) %>%
  select(`WEO Country Group Code`, `Country Group Name`) %>% 
  distinct() %>% 
  arrange(`WEO Country Group Code`)
}

get_country_subjects <- function(path = "./data/") {
  readxl::read_xlsx(paste0(path, "WEOOct2022all.xlsx"), 
            na = c("", "n/a", "--")) %>% 
  select(`WEO Subject Code`, `Subject Descriptor`, `Subject Notes`, Units, 
           Scale) %>% 
  distinct() %>% 
  arrange(`WEO Subject Code`)
}

get_group_subjects <- function(path = "./data/") {
  readxl::read_xlsx(paste0(path, "WEOOct2022alla.xlsx"), 
            na = c("", "n/a", "--")) %>% 
  select(`WEO Subject Code`, `Subject Descriptor`, `Subject Notes`, Units, 
           Scale) %>% 
  distinct() %>% 
  arrange(`WEO Subject Code`)
}

get_county_data <- function(path = "./data/") {
  readxl::read_xlsx(paste0(path, "WEOOct2022all.xlsx"), 
            na = c("", "n/a", "--")) %>% 
  pivot_longer(as.character(seq(1980, 2027)), 
                 names_to = "year", values_to = "value") %>% 
  select(ISO, `WEO Subject Code`, year, value) %>% 
  pivot_wider(names_from = `WEO Subject Code`, values_from = value) %>% 
  mutate(NOMEX = NGDP / NGDPD) %>% 
  mutate(year = as.integer(year)) %>% 
  as_tsibble(index = year, key = ISO)
}

get_group_data <- function(path = "./data/") {
  readxl::read_xlsx(paste0(path, "WEOOct2022alla.xlsx"), 
            na = c("", "n/a", "--")) %>% 
  pivot_longer(as.character(seq(1980, 2027)), 
                 names_to = "year", values_to = "value") %>% 
  select(`WEO Country Group Code`, `WEO Subject Code`, year, value) %>% 
  pivot_wider(names_from = `WEO Subject Code`, values_from = value) %>% 
  mutate(year = as.integer(year)) %>% 
  as_tsibble(index = year, key = `WEO Country Group Code`)
}



