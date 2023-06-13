rm(list=ls())
library(magrittr)
library(dplyr)
library(purrr)
library(ggplot2)

#Printing For_Loop
range_vector <- 1:10
for ( i in range_vector){
  x <- i+3
  print(x)
}
libr

#Challenge: Subplots
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
df_long <- df %>% # df is wide
  tidyr::pivot_longer(starts_with("kernel"),
                      values_to = "kernel",
                      names_to="kerneltype")
df_long %>% 
  group_by(student, spike) %>%
  ggplot(aes(kernel, spike, color=student))+
  geom_point()+
  geom_path() +
  facet_grid(kerneltype~student)+
  theme_minimal()

df_type <- df %>%
  group_by(student) %>% 
  mutate(type=cut(spike,  breaks = 3, labels = c("apical", "central", "basal")))

  
library(ggpol)
p <- df_type%>% 
  ggplot(aes(type,flower,fill=student))+
  geom_boxjitter(aes(color=student),alpha=.4,
                 jitter.shape = 21, jitter.color = NA, 
                 jitter.params = list(height = 0, width = 0.04),
                 outlier.color = NA, errorbar.draw = TRUE)+
  theme_classic()+
  theme(strip.background = element_blank(),
        panel.grid.major.x = element_line(),
        legend.position = "bottom") 


print(p)

factor_levels <- c("basal", "central", "apical")
df_type$type <- factor(df_type$type, levels = factor_levels)
