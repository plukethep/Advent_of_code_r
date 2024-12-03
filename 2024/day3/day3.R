library(tidyverse)
library(glue)
library(here)
text_file <- read_file(glue("{loc}data.txt"))

# PART 1
muls <- text_file %>% str_extract_all("mul[(][0-9]+,[0-9]+[)]") %>% unlist()
x <- str_extract_all(muls, "[(][0-9]+") %>% str_remove_all("[(]") %>% as.numeric()
y <- str_extract_all(muls, "[0-9]+[)]") %>% str_remove_all("[)]") %>% as.numeric()
(x * y) %>% reduce(`+`)

# PART 2
muls <- text_file %>% str_remove_all("\\r\\n") %>%
  str_remove_all("don[']t[(].*do[(]") %>% 
  str_remove_all("don[']t[(][)].*") %>% 
  str_extract_all("mul[(][0-9]+,[0-9]+[)]") %>% 
  unlist()
x <- str_extract_all(muls, "[(][0-9]+") %>% str_remove_all("[(]") %>% as.numeric()
y <- str_extract_all(muls, "[0-9]+[)]") %>% str_remove_all("[)]") %>% as.numeric()
(x * y) %>% reduce(`+`)
