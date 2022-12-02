library(tidyverse)

input <- read_csv("C:/tmp/input2.txt", col_names = c("moves"))

input <- input %>% 
  mutate(p2 = gsub(".*[ ]", "", moves),
         p1 = gsub("[ ].*", "", moves))

# vector shifter
shifter <- function(x, n = 1){
  if (n == 0) x else c(tail(x, n), head(x, -n))
}

# number of symbols
n <- 3

# get all combinations of scores
combinations <- expand.grid(p1=c(LETTERS[1:n]),
            p2=LETTERS[(27-n):26]) %>%
  summarise(moves = paste(p1, p2))

# create score pattern
winlose <- 
  map(1:n-1,
    ~{shifter(c(3,0,0), .x)}) %>% unlist() + 
  map(1:n-1,
    ~{shifter(c(0,0,6), .x)}) %>% unlist()
move_score <- 
  map(1:n,
      ~{rep(.x,n)}) %>% unlist()

combinations$winlose <- winlose
combinations$move_score <- move_score
combinations$score <- combinations$move_score + combinations$winlose
combinations <- combinations %>% 
  mutate(p2 = gsub(".*[ ]", "", moves),
         p1 = gsub("[ ].*", "", moves))
left_join(input, combinations)$score %>% sum()

# Part 2
result <- read_csv("X,0
Y,3
Z,6", col_names = c("result","outcome"))

input <- left_join(input, result, by=c("p2"="result"))

# get moves and cacluate score
left_join(input, combinations, by=c("p1"="p1",
                                    "outcome"="winlose")) %>%
  mutate(total_score = outcome+move_score) %>%
  pull(total_score) %>% sum()
