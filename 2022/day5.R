library(tidyverse)
loc <- "C:/temp/day5.txt"
cols <- 9
skp <- 9

# loc <- "C:/temp/day5test.txt"
# cols <- 3
# skp <- 4

shuffle_up <- function(data, field){
  filler <- data[,field][[1]] %>% unname() %>% na.omit()
  NA_padding <- rep(NA,nrow(data) - length(filler))
  data[,field] <- c(filler,NA_padding)
  return(data)
}


colls <- read_fwf(loc, 
                  fwf_widths(rep(4,cols), paste0("c", 1:cols)),
                  n_max=skp-1) %>% 
  map2_dfc(., names(.), 
          function(x,y){
               names(x) <- y
               x %>% gsub("]|[ ]|\\[|\r", "", .) # %>% rev()
          })
# create blank space at bottom of dataframe
rnum <- nrow(colls)
colls <- rbind(colls,rbind(colls,rbind(colls,rbind(colls,rbind(colls,rbind(colls,rbind(colls, colls)))))))
colls[rnum+1:nrow(colls), ] <- NA

colls <- shuffle_up(colls, "c1")
colls <- shuffle_up(colls, "c2")
colls <- shuffle_up(colls, "c3")


shifts <- read_delim(loc, delim=" ", skip=skp, 
                     col_names = c("c1", "num", "c2", "from", "c3", "to")) %>%
  select(!starts_with("c"))

for (r in 1:nrow(shifts)){
  # print(colls %>% head(10))
  message("ROW:",r)
  # r <- 1
  
  num <- shifts[r, "num"][[1]]
  from <- shifts[r, "from"][[1]] 
  to <- shifts[r, "to"][[1]]
  message(num, from, to)
  
  scoop <- colls[1:num,from][[1]] %>% unname() %>% na.omit() %>% rev()
  message(scoop)
  colls[1:num,from] <- NA
  
  colls <- shuffle_up(colls, from)
 
  new_to <- c(scoop, colls[,to][[1]] %>% 
                     unname() %>% na.omit())
  NA_padding <- rep(NA,nrow(colls) - length(new_to)) # add extra NA padding
  
  colls[,to] <- c(new_to, NA_padding)
}

colls[1,] %>% paste0(collapse="")
