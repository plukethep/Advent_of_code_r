text_file %>% str_remove_all("don[']t[(].*do[(]") %>% 
  str_extract_all("mul[(][0-9]+,[0-9]+[)]") %>% 
  unlist()
x <- str_extract_all(muls, "[(][0-9]+") %>% str_remove_all("[(]") %>% as.numeric()
y <- str_extract_all(muls, "[0-9]+[)]") %>% str_remove_all("[)]") %>% as.numeric()
(x * y) %>% reduce(`+`)
