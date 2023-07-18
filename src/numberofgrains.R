rm(list = ls())
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




#Show data in Boxplot
##For different kerneltypes
ggplot(graindf_num1, aes(x = kernel.type, y = grainnum)) +
  geom_boxplot() +
  labs(x = "Kernel Type", y = "Number of Kernels") +
  ggtitle("Number of Kernels per Kernel Type") +
  theme_minimal()

##For all kernels
ggplot(graindf_num_tot1, aes(x = "", y = total_kernels)) +
  geom_boxplot() +
  labs(x = "gc_40_11", y = "Kernels") +
  ggtitle("Total Kernels per Ear")+
  theme(axis.text.x = element_blank())+
  ylim(0, max(graindf_num_tot$total_kernels))+
  theme_minimal()

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

##Violin plot
ggplot(graindf_num_tot_comb, aes(x = "", y = total_kernels)) +

  geom_violin() +
  labs(x = NULL, y = "Number of Kernels") +
  ggtitle("Total kernels per treatment") +
  facet_wrap(~ plot_id, nrow = 1) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())

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
 

#Spalten in Basal Central Apical

graindf40_acb <- graindf40 %>% 
  

