library(tidyverse)
library(magrittr)

row_to_decimal <- function(row){
  row %>% unite("gamma", starts_with("b"),sep="") %>% strtoi(base = 2)
}

invert_binary <- function(row){
  row %>% mutate(across(everything(), ~ if_else(.x == 1, 0, 1)))
}

find_rating <- function(bits, focus_bits, on=1, off=0){
  for (b in names(bits)){
    
    # b <- "b1"
    # focus_bits <- oxygen_bits
    b_ <- sym(b)
    flt <- map(focus_bits[b], function(x){
      if_else(sum(as.numeric(x) == 1) > sum(as.numeric(x) == 0), 
              on, off)})

    # message(b)
    # message("1 = ",sum(as.numeric(focus_bits[b][[1]]) == 1))
    # message("0 = ",sum(as.numeric(focus_bits[b][[1]]) == 0))
    # print(focus_bits)
    # message(flt)
    
    focus_bits <- focus_bits %>% filter(!!b_ == flt)
    
    if(nrow(focus_bits) == 1) break
  }
  return(focus_bits %>% row_to_decimal())
}

data <- data.frame(readLines("input3.txt"))
# data <- data.frame(readLines("test3.txt"))
names(data) <- "bits"

bits <- data %>% 
          separate(bits, into = 
                    paste("b",seq(0,str_length(data[1,])), sep=""), 
                  sep="") %>% select(-b0)

#### Part 1

gamma <- map_df(bits, function(x){
  if_else(sum(as.numeric(x) == 1) - sum(as.numeric(x) == 0) < 0, 1, 0)})

(gamma %>% row_to_decimal()) * (gamma %>% invert_binary() %>% row_to_decimal())


#### Part 2
oxygen_bits <- bits
co2_bits <- bits

find_rating(bits, oxygen_bits) * find_rating(bits, oxygen_bits, on=0, off=1)

