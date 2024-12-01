library(tidyverse)
library(magrittr)

get_val <- function(data, x, y){
  as.numeric(substr(data[y,],x,x))
}

go_up <- function(x, y){
  if(y != 1) TRUE else FALSE
}

go_down <- function(data, x, y){
  if(y != max(nrow(data))) TRUE else FALSE
}

go_left <- function(x, y){
  if(x != 1) TRUE else FALSE
}

go_right <- function(data, x, y){
  if(x != str_length(data[1,])) TRUE else FALSE
}

get_surround <- function(data, x, y){
  surround <- c()
  
  # get above
  if(go_up(x,y)){
    surround <- c(surround, get_val(data, x, y - 1))
  }
  
  # get down
  if(go_down(data, x,y)){
    surround <- c(surround, get_val(data, x, y + 1))
  }
  
  # get left
  if(go_left(x,y)){
    surround <- c(surround, get_val(data, x-1, y))
  }
  
  # get right
  if(go_right(data, x,y)){
    surround <- c(surround, get_val(data, x+1, y))
  }
  
  return(surround)
}

get_surround_dip <- function(data, x, y, visited, id){
  surround <- data.frame()
  
  if(visited){
    surround <- rbind(surround, c(x, y, visited, id))
    names(surround) <- c("x", "y", "visited", "id")
    return(surround)
  }
  
  surround <- rbind(surround, c(x, y, TRUE))
  
  # get above
  if(go_up(x,y)){
    u <- get_val(data, x, y - 1)
    
    if(u > get_val(data, x, y) & u != 9){
      surround <- rbind(surround, c(x, y -1, FALSE))
    }
  }
  
  # get down
  if(go_down(data, x,y)){
    d <- get_val(data, x, y + 1)
    
    if(d > get_val(data, x, y) & d != 9){
      surround <- rbind(surround, c(x, y + 1, FALSE))
    }
  }
  
  # get left
  if(go_left(x,y)){
    l <- get_val(data, x-1, y)
    
    if(l > get_val(data, x, y) & l != 9){
      surround <- rbind(surround, c(x-1, y, FALSE))
    }
  }
  
  # get right
  if(go_right(data, x,y)){
    r <- get_val(data, x+1, y)
    
    if(r > get_val(data, x, y) & r != 9){
      surround <- rbind(surround, c(x+1, y, FALSE))
    }
  }
  
  names(surround) <- c("x", "y", "visited")
  surround <- surround %>% mutate(id=id)
  return(surround)
}

heights <- as.data.frame(readLines("test9.txt"))

risk <- c()
risks <- data.frame()

# PART 1
for(y in seq(nrow(heights))){
  for(x in seq(str_length(heights[y,]))){
    
    height <- get_val(heights, x, y)
    surround <- get_surround(heights, x, y)
    
    if(height < min(surround)){
      message("LOW", x, y, " - ", height)
      risk <- c(risk, as.numeric(height) + 1)
      risks <- rbind(risks, c(x,y))
    }
  }
}

sum(risk)
names(risks) <- c("x", "y")
risks <- risks %>% mutate(visited=FALSE)


# PART 2
check <- map_df(seq(nrow(risks)), 
             \(x)
             get_surround_dip(heights, 
                              unlist(risks[x,][1]), 
                              unlist(risks[x,][2]),
                              FALSE,
                              x))
  
while(check %>% filter(visited == FALSE) %>% nrow() > 0){
  check <- map_df(seq(nrow(check)), 
             \(x)
             get_surround_dip(heights, 
                              unlist(check[x,][1]), 
                              unlist(check[x,][2]),  
                              unlist(check[x,][3]),
                              unlist(check[x,][4]))) %>% distinct()
  
}

prod(check %>% group_by(id) %>% summarise(n=n()) %>% arrange(desc(n)) %>% head(3) %$% n)
