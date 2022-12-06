key <- gsub("\n", "", key)  
key <- str_split(key, "")[[1]]

msglen <- 14

map_int(1:(length(key)-3),
    function(x){
      if(length(unique(map_chr(0:(msglen-1), ~{key[x + .x]}))) < msglen){
        0L
      }else{
        as.integer(x + (msglen-1))
      }}) %>% match(1:length(key)) %>% na.omit() %>% head(1)
