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

wsum <- function(.data, x, weight, index_name = "X") {
  thesums <- .data %>% 
    summarize(.sumwtilde = sum({{weight}}))
  theoutput <- .data %>% 
    left_join(thesums, by = c(index_var(.data), group_vars(.data))) %>% 
    group_by_key() %>% 
    mutate(.w = {{weight}} / .sumwtilde) %>% 
    group_by(!!!syms(group_vars(.data))) %>% 
    summarize(.X = sum({{x}} * .w)) %>% 
    ungroup() 
  names(theoutput)[names(theoutput) == ".X"] <- index_name 
  return(theoutput)
}

