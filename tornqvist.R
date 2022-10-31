library(tidyverse)
library(tsibble)

thedata <- tsibble(
  year = rep(1981:1983,4),
  country = rep(LETTERS[1:4],1,each=3), 
  continent = rep(letters[1:2],1,each=6), 
  values = rnorm(12,mean=100),
  sizes = rnorm(12,mean=10),
  another = rnorm(12, mean=50), 
  index = year,
  key = country
)
thedata

tornqvist <- function(.data, x, weight, index_name = "X") {
  thesums <- .data %>% 
    summarize(.sumwtilde = sum({{weight}}))
  theoutput <- .data %>% 
    left_join(thesums, by = c(index_var(.data), group_vars(.data))) %>% 
    group_by_key() %>% 
    mutate(.w = {{weight}} / .sumwtilde) %>% 
    mutate(.w2 = slider::slide_dbl(.w, mean, .before = 1L, .complete = TRUE)) %>% 
    mutate(.xdot = difference(log({{x}}))) %>% 
    group_by(!!!syms(group_vars(.data))) %>% 
    summarize(.Xdot = sum(.xdot * .w2)) %>% 
    group_by_key() %>% 
    mutate(.lnX = slider::slide_dbl(.Xdot, sum, na.rm = TRUE, .before = Inf), 
           .keep = "unused") %>% 
    mutate(.X = exp(.lnX), .keep = "unused") %>% 
    ungroup() 
    names(theoutput)[names(theoutput) == ".X"] <- index_name 
    return(theoutput)
}

thedata %>% 
  group_by(continent) %>% 
  tornqvist(values, sizes, index_name = "value_index") 

