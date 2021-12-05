library(tidyverse)
library(magrittr)

mark_num <- function(tab, num){
  tab %>% mutate(across(everything(), ~ifelse(.x == num, 999, .x)))
}

check_grid <- function(tab){
  
  down <- unlist(map_df(tab, function(x) ifelse(min(x) == 999, TRUE, FALSE)))
  across <- tab %>% rowwise() %>% mutate(m = 
                                            ifelse(min(c_across(everything())) == 999, 
                                                       TRUE, FALSE)) %$% m
  if(sum(down) + sum(across) > 0) return(TRUE) else return(FALSE)
}

calc_score <- function(tab){
  
  tab <- tab %>% mutate(across(everything(), 
                                function(x) map(x, 
                                                function(y) ifelse(y == 999, 0, y)))) %>%
                  summarise(across(everything(), function(x) sum(as.numeric(x), na.rm = TRUE))) %>%
                  unlist() %>%
                  sum()
  return(tab)
}

calls <- unlist(read.table("input4.txt", sep=",", nrows=1)[1,])

# load files
file_rows <- length(readLines("input4.txt"))

tabs <- map(seq(2,file_rows, 6),
               function(x) read.table("input4.txt", nrows=5, skip=x))

for(c in calls){
  message("#############")
  print(c)
  
  # mark each card with new 
  tabs <- map(tabs, function(x) mark_num(x,c))

  # check to see if complete
  complete <- map(tabs, function(x) check_grid(x))

  #### PART 1
  # if(sum(unlist(complete)) > 0){
  #   # print(unlist(complete))
  #   
  #   winner <- seq(1,length(tabs))[unlist(complete)]
  #   print(paste(winner, "WINNER!"))
  #   winner <- tabs[[winner]]
  #   
  #   print(calc_score(winner) * c)
  #   
  #   exit()
  # }
  
  #### PART 2
  print(paste(sum(unlist(complete)), " - completed"))
  if(sum(unlist(complete)) == 99){
    # print(unlist(complete))

    last_winner <- seq(1,length(tabs))[!unlist(complete)]
  }
  
  if(sum(unlist(complete)) == 100){
    
    last_winner <- tabs[[last_winner]]
    print(paste(last_winner, "LOSER!"))
    print(calc_score(last_winner) * c)
    exit
  }
}
