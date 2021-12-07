
library(tidyverse)

# test
crabs <- data.frame(c(16,1,2,0,4,2,7,1,2,14))

crabs <- data.frame(read.csv("input7.txt", header = FALSE) %>% t())
           
names(crabs) <- "h"

shifts <- c(min(crabs$h):max(crabs$h))

map_df(shifts, \(x) 
    crabs %>% mutate(fuel = abs(x - h),
                     aim = x,
                     part2 = (fuel*(fuel+1))/2)) %>% 
  
  group_by(aim) %>%
  summarise(fuel = sum(fuel),
            part2 = sum(part2)) %>%
  ungroup() %>%
  filter(fuel == min(fuel) | part2 == min(part2))