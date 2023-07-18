library(magrittr)
library(xlsx)
df<- map_dfr(list.files("./data/Grain_Counting"),~{
  
  student_name <-  .x %>% strsplit("_") %>% unlist() %>% 
    .[4] %>% sub(".xlsx","",.)
  
  file<- xlsx::read.xlsx(paste0("./data/Grain_Counting/",.x),sheetIndex = 1) %>%  
    `colnames<-`(stringr::str_to_lower(names(.)))%>% 
    `colnames<-`(gsub("kernal","kernel",names(.))) %>% 
    `colnames<-`(gsub("spikes","spike",names(.)))%>%
    `colnames<-`(gsub("plot.id","plot_id",names(.))) %>% 
    mutate(student=student_name)
}) 
df %<>% mutate(var="Capone",plot_id=159) %>% 
  .[!grepl("na.",names(.))]
df %>% glimpse()