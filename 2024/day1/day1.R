library(tidyverse)
library(glue)

loc <- r"[C:\Users\k1926273\OneDrive - King's College London\Code\advent\]"
day <- "day1"
fle <- glue("{loc}{day}/input1.txt")
df <- read_delim(fle,delim = "   ", col_names = FALSE)

# PART 1
df %>% mutate(X1 = sort(df$X1),
              X2 = sort(df$X2),
              difference = X1 - X2) %>%
  summarise(total = sum(abs(difference))) %>%
  pull(total)

# PART 2
map_dbl(df$X1, \(x1){
  x1 * ((df$X2 == x1) %>% sum())
}) %>% sum()

