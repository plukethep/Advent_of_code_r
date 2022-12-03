## PART 1
rucks <- read_csv("vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw", col_names=c("rucksacks"))

rucks <- read_csv("C:/tmp/input3.txt", col_names=c("rucksacks"))

rucks <- rucks %>% mutate(cmp1 = substr(rucksacks, 1, nchar(rucksacks)/2),
                 cmp2 = substr(rucksacks, nchar(rucksacks)/2 +1, nchar(rucksacks)))

rucks <- rucks %>% rowwise() %>%
  mutate(common = intersect(
                      str_split(cmp1, "")[[1]], 
                      str_split(cmp2, "")[[1]]))

letvals <- data.frame(vals=seq_along(c(letters, LETTERS)), lets=c(letters, LETTERS))
priorities <-  left_join(rucks, letvals, by=c("common"="lets"))
priorities$vals %>% sum()

## PART 2
