library(tidyverse)


depths <- read_csv("input1.csv")

bottom <- depths[nrow(depths),]
depth_shift <- depths[2:nrow(depths),]
depth_shift <- rbind(depth_shift, bottom)
names(depth_shift) <- "shift"

depths <- cbind(depths, depth_shift) %>% 
  # mutate(deeper = depths - shift)
  summarise(deeper = sum(ifelse(depths - shift < 0, TRUE, FALSE)))




## OR more standard way
depths <- as.list(depths)[[1]]

counter <- 0

for(x in seq(2,length(depths))){
  print(paste(x, counter))
  
  if(depths[x] > depths[x-1]){
    counter <- counter + 1
  }
  
}


###### PART 2
depths <- read_csv("input1.csv")

## OR more standard way
depths <- as.list(depths)[[1]]

counter <- 0

for(x in seq(1,length(depths)-2)){
  print(paste(x, counter))

  if(sum(depths[seq(x+1,x+3)]) > sum(depths[seq(x,x+2)])){
    counter <- counter + 1
  }
  
}

