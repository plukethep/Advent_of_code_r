rucks <- read_csv("C:/tmp/input3.txt", col_names=c("rucksacks"))
letvals <- data.frame(vals=seq_along(c(letters, LETTERS)), lets=c(letters, LETTERS))

## PART 1
rucks <- rucks %>% 
            mutate(cmp1 = substr(rucksacks, 1, nchar(rucksacks)/2),
                    cmp2 = substr(rucksacks, nchar(rucksacks)/2 +1, nchar(rucksacks)))

rucks <- rucks %>% rowwise() %>%
  mutate(common = intersect(
                      str_split(cmp1, "")[[1]], 
                      str_split(cmp2, "")[[1]]))

left_join(rucks, letvals, by=c("common"="lets")) %>%
  pull(vals) %>% sum()

## PART 2

rucks2 <- rucks %>% ungroup() %>% # upgroup the previous rowwise
            mutate(lead1 = lead(rucksacks, 1, default=""),
                    lead2 = lead(rucksacks, 2, default="")) %>%
            rowwise() %>%
            mutate(lets = list(intersect(str_split(rucksacks, "")[[1]], 
                          intersect(str_split(lead1, "")[[1]], 
                                    str_split(lead2, "")[[1]])))) 

rucks2[seq(1,nrow(rucks), by=3),] %>% 
  summarise(lets = unlist(lets)) %>% 
  left_join(letvals) %>%
  pull(vals) %>% sum()
