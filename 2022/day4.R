jobs <- read_csv("c:/tmp/input4.txt", col_names=c("elf1", "elf2"))

jobs_set <- map2_dfc(jobs, names(jobs),
        function(x,n){
          tmp <- data.frame(things=c(1:length(x))) %>% select(-things)
          tmp$sq <- map(str_split(x,"-"),
              function(y){
                s <- y %>% as.numeric()
                s[1]:s[2]
              }) 
          names(tmp) <- n
          tmp
        })

combinations <- jobs_set %>% rowwise() %>%
  mutate(btwn = ifelse(identical(intersect(elf1, elf2),elf1) |
                       identical(intersect(elf1, elf2),elf2),
                   TRUE,
                   FALSE)) %>%
  mutate(overlap = ifelse(length(intersect(intersect(elf1, elf2),elf1)) >0  |
                          length(intersect(intersect(elf1, elf2),elf2)) >0,
                       TRUE,
                       FALSE))

# TASK1
combinations %>% pull(btwn) %>% sum()
#TASK2
combinations %>% pull(overlap) %>% sum()
