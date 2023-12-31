rm(list = ls())
detach(package:plyr, unload = TRUE)
library(dplyr)
library(ggplot2)
library(ggpol)
library(viridis)
library(readxl)
library(ggpubr)
install.packages('dyplr')
#read data
readx<- function(p,sh){
  df <- readxl::read_xlsx(p,sheet = sh) %>% 
    mutate(time_id = ifelse(plot_id == 40, "early", ifelse(plot_id == 42, "late", NA))) %>% 
    mutate(plot_id= as.character(plot_id)) %>% 
    mutate(across(starts_with("kernel"),function(x)as.character(x))) %>% 
    tidyr::pivot_longer(starts_with("kernel"),names_to = "kernel.type",values_to = "floret.pos") %>% 
    mutate(floret.pos=strsplit(floret.pos,",")) %>% 
    tidyr::unnest(floret.pos) %>% 
    mutate(floret.pos=as.numeric(floret.pos),
           kernel.size=factor(kernel.type,levels=paste0("kernel.",c("S","M","L"))) %>% as.numeric() %>% 
             # create contrast
             ifelse(.==3,5,.)) %>%
    mutate(var = case_when(var == "Potenzial" ~"potenzial",
                           T ~ var))
}  


p <- "data/Grain_Counting/gc_40_11.xlsx"

graindf40<- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
  readx(p,.x)
  }) %>% filter(!is.na(floret.pos))


p <- "data/Grain_Counting/gc_42_11.xlsx"

graindf42<- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
  readx(p,.x)
}) %>% filter(!is.na(floret.pos))

#Calculate number of grains
##40
graindf_num40 <- graindf40 %>%
  select(var, plot_id, rep, kernel.type) %>% 
  group_by(rep,kernel.type) %>% 
  mutate(grainnum = n()) %>%
  distinct(plot_id, rep, kernel.type, grainnum)

graindf_num_tot40 <- graindf_num40 %>% 
  filter(kernel.type %in% c("kernel.S", "kernel.M", "kernel.L")) %>%
  group_by(plot_id, rep) %>%
  summarize(total_kernels = sum(grainnum), .groups = "drop")
##42
graindf_num42 <- graindf42 %>%
  select(var, plot_id, rep, kernel.type) %>% 
  group_by(rep,kernel.type) %>% 
  mutate(grainnum = n()) %>%
  distinct(plot_id, rep, kernel.type, grainnum)

graindf_num_tot42 <- graindf_num42 %>% 
  filter(kernel.type %in% c("kernel.S", "kernel.M", "kernel.L")) %>%
  group_by(plot_id, rep) %>%
  summarize(total_kernels = sum(grainnum), .groups = "drop")

#combine data for diffrent plots
graindf_num_tot_comb <- bind_rows(graindf_num_tot40, graindf_num_tot42)
graindf_num_comb <- bind_rows(graindf_num40, graindf_num42)


# Create the boxplot with two batches side by side
ggplot(graindf_num_tot_comb, aes(x = plot_id, y = total_kernels, fill = plot_id)) +
  stat_boxplot(geom="errorbar", width=0.5)+
  geom_boxplot() +
  geom_jitter() +
  stat_compare_means(method = "t.test", vjust = -0.5) +
  labs(x = "Sowing Date", y = "Number of Kernels") +
  ggtitle("Total kernels per treatment") +
  scale_x_discrete(labels = c("early", "late")) +
  theme(legend.position = "none") 

#Boxplot with kerneltypes
ggplot(graindf_num_comb, aes(x = plot_id, y = grainnum, fill = plot_id)) +
  stat_boxplot(geom="errorbar", width=0.5)+
  geom_boxplot() +
  geom_jitter(color = "black") +
  stat_compare_means(method = "t.test", vjust = -0.5) +
  labs(x = "Sowing Date", y = "Number of Kernels") +
  scale_x_discrete(labels = c("early", "late")) +
  ggtitle("Total kernels per treatment") +
  theme(legend.position = "none") +
  facet_wrap(~ kernel.type, nrow = 1) 

rm(list = ls())
  

