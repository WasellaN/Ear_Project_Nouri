rm(list=ls())
library(magrittr)
library(dplyr)
library(purrr)
library(ggplot2)

df<- map_dfr(list.files("./data/student"),~{
  
  student_name <-  .x %>% strsplit("_") %>% unlist() %>% 
    .[4] %>% sub(".xlsx","",.)
  
  file<- readxl::read_xlsx(paste0("./data/student/",.x)) %>%  
    `colnames<-`(stringr::str_to_lower(names(.)))%>% 
    `colnames<-`(gsub("kernal","kernel",names(.))) %>% 
    `colnames<-`(gsub("spikes","spike",names(.)))%>%
    `colnames<-`(gsub("plot.id","plot_id",names(.))) %>% 
    mutate(student=student_name)
}) 
df %<>% mutate(var="Capone",plot_id=159) %>% 
  .[!grepl("na.",names(.))]
df %>% glimpse()

# line plot 1
df %>% 
  group_by(student,spike) %>% 
  ggplot(aes(flower,spike,color=student))+
  geom_line(alpha=.5)+
  theme(legend.position = "bottom")
# line plot 2
df %>% 
  group_by(student,spike) %>% 
  ggplot(aes(flower,spike,color=student))+
  geom_point()+
  geom_path(alpha=.5)+
  theme(legend.position = "bottom")


