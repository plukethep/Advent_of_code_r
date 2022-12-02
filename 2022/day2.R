library(tidyverse)

input <- read_csv("C:/tmp/input2.txt", col_names = c("moves"))%>% 
              mutate(p2 = gsub(".*[ ]", "", moves),
                     p1 = gsub("[ ].*", "", moves))

# vector shifter
shifter <- function(x, n = 1){
  if (n == 0) x else c(tail(x, n), head(x, -n))
}

# create score pattern for all combos
n <- 3

# get all combinations of letters
combinations <- expand.grid(p1=c(LETTERS[1:n]),
                            p2=LETTERS[(27-n):26]) %>%
  mutate(moves = paste(p1, p2)) 
# create scores
combinations$winlose <- 
  map(1:n-1, ~{shifter(c(3,0,0), .x)}) %>% unlist() + 
  map(1:n-1, ~{shifter(c(0,0,6), .x)}) %>% unlist()
combinations$move_score <- map(1:n, ~{rep(.x,n)}) %>% unlist()

left_join(input, combinations) %>% 
  mutate(score = move_score + winlose) %>% 
  pull(score) %>% sum()

# Part 2
# create result mapping
result <- data.frame(result=c("X","Y","Z"), outcome=c(0,3,6))

# join input to the desired result mapping
input <- left_join(input, result, by=c("p2"="result"))

# get moves and calculate score
left_join(input, combinations, by=c("p1"="p1",
                                    "outcome"="winlose")) %>%
  mutate(total_score = outcome+move_score) %>%
  pull(total_score) %>% sum()
