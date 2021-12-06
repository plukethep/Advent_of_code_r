library(tidyverse)
library(gmp)

# Test data
data <- c(3,4,3,1,2)

data <- c(5,1,1,3,1,1,5,1,2,1,5,2,5,1,1,1,4,1,1,5,1,1,4,1,1,1,3,5,1,1,1,1,1,1,1,1,1,4,4,4,1,1,1,1,1,4,1,1,1,
           1,1,5,1,1,1,4,1,1,1,1,1,3,1,1,4,1,4,1,1,2,3,1,1,1,1,4,1,2,2,1,1,1,1,1,1,3,1,1,1,1,1,2,1,1,1,1,1,1,
           1,4,4,1,4,2,1,1,1,1,1,4,3,1,1,1,1,2,1,1,1,2,1,1,3,1,1,1,2,1,1,1,3,1,3,1,1,1,1,1,1,1,1,1,3,1,1,1,1,
           3,1,1,1,1,1,1,2,1,1,2,3,1,2,1,1,4,1,1,5,3,1,1,1,2,4,1,1,2,4,2,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,4,3,
           1,2,1,2,1,5,1,2,1,1,5,1,1,1,1,1,1,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,4,1,1,
           1,1,1,3,1,1,5,1,1,1,1,5,1,4,1,1,1,4,1,3,4,1,4,1,1,1,1,1,1,1,1,1,3,5,1,3,1,1,1,1,4,1,5,3,1,1,1,1,1,
           5,1,1,1,2,2)

input1 <- data
## Part 1
for(x in seq(1,80)){
  
  input1 <- unlist(map(input1, function(x)
                    if(x == 0) list(8,6) else x-1))
    
    # move 8s to the end of the list
    # input1 <- c(input1[input1!=8], input1[input1==8])
    message(x, " : ", length(input1))
}

message(x, "  :  ", input1, " :: ", length(input1))

## Part 2
zeroes <- as.bigz(length(data[data == 0]))
ones <-   as.bigz(length(data[data == 1]))
twos <-   as.bigz(length(data[data == 2]))
threes <- as.bigz(length(data[data == 3]))
fours <-  as.bigz(length(data[data == 4]))
fives <-  as.bigz(length(data[data == 5]))
sixes <-  as.bigz(length(data[data == 6]))
sevens <- as.bigz(length(data[data == 7]))
eights <- as.bigz(length(data[data == 8]))


for(x in seq(1,256)){

  eights_new <- zeroes
  sevens_new <- eights
  sixes_new <- sevens + zeroes
  fives_new <- sixes 
  fours_new <- fives
  threes_new <- fours
  twos_new <- threes
  ones_new <- twos
  zeroes_new <- ones
  
  eights <- eights_new
  sevens <- sevens_new
  sixes <- sixes_new
  fives <- fives_new 
  fours <- fours_new
  threes <- threes_new
  twos <- twos_new
  ones <- ones_new
  zeroes <- zeroes_new
  
  message(paste(c(zeroes, ones, twos, threes, fours, fives, sixes, sevens, eights), collapse=" | "))
  message(x, "  :  ", (zeroes + ones + twos + threes + fours + fives + sixes + sevens + eights))
}

