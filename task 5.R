vents <- read.delim("test5.txt", sep=">", col.names=c("p1","p2"), header=FALSE) %>%
  mutate(p1 = gsub(" -", "", p1),
         p2 = gsub(" ", "", p2)) %>%
  separate(p1, c("x1", "y1"), ",") %>%
  separate(p2, c("x2", "y2"), ",") %>%
  mutate(across(everything(),as.numeric)) #%>%
  # filter(x1 == x2 | y1 == y2) # snip out to restore diagonals for Part 1

# get all intermediate locations  
points <- unlist(map(data.frame(vents %>% t()), 
                     function(x) paste0(seq(x[1], x[3]), ",", seq(x[2], x[4]))))

data.frame(points) %>% group_by(points) %>% summarise(n = n()) %>% filter(n > 1) %>% nrow()
