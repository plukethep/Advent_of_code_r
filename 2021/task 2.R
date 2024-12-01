library(tidyverse)
library(magrittr)

data <- data.frame(readLines("input2.txt"))
names(data) <- "directions"

data <- data %>% separate(col=directions, into=c("direction","distance"), sep="\\s") %>%
  mutate(distance = as.numeric(distance))


# Part 1
data_1 <- data %>% group_by(direction) %>% summarise(total = sum(distance))

(data_1 %>% filter(direction=="down") %$% total - 
    data_1 %>% filter(direction=="up") %$% total) * data_1 %>% filter(direction=="forward") %$% total


# Part 2

depth <- 0
aim <- 0
horizontal <- 0

for(x in c(1:nrow(data))){
  
  type <- data[x,]$direction
  dist <- data[x,]$distance
  if(type == "forward"){
    horizontal <- horizontal + dist
    depth <- depth + (aim * dist)
  }
  
  if(type == "up"){
    aim <- aim - dist
  }
  
  if(type == "down"){
    aim <- aim + dist
  }
}
depth * horizontal


down X increases your aim by X units.
up X decreases your aim by X units.
forward X does two things:
  It increases your horizontal position by X units.
It increases your depth by your aim multiplied by X.

