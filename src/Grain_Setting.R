rm(list = ls())
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggpol)
library(readxl)
#read data
readx<- function(p,sh){
  df <- readxl::read_xlsx(p,sheet = sh) %>% 
    mutate(time_id = ifelse(plot_id == 40, "early", ifelse(plot_id == 42, "late", NA))) %>% 
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

#combine dfs
grain_comb <- bind_rows(graindf40, graindf42)

grain_set <- grain_comb %>% 
  group_by(time_id, rep, spike) %>%
  summarise(total_kernels = n(),
            total_flowers = sum(ifelse(row_number() == 1, flower, 0))) %>% 
  group_by(time_id,rep) %>% 
  summarize(total_kernels =sum(total_kernels),
            total_flowers = sum(total_flowers)) %>% 
  ungroup() %>% 
  mutate(grain.set = total_kernels / total_flowers) %>% 
  mutate(abortion.rate = 1 - grain.set)


#Grain Setting by Sowing Date
ggplot(grain_set, aes(x = factor(time_id), y = grain.set, fill = time_id)) +
  stat_boxplot(geom="errorbar", width=0.5)+
  geom_boxplot() +
  geom_jitter() +
  stat_compare_means(method = "t.test") +
  labs(x = "Sowing Date", y = "Grain Setting Rate [%]") +
  ggtitle("Grain Setting by Sowing Date (Batch 11)") +
  theme(legend.position = "none")


#Floret Count by Sowing Date
ggplot(grain_set, aes(x = factor(time_id), y = total_flowers, fill = time_id)) +
  stat_boxplot(geom="errorbar", width=0.5)+
  geom_boxplot() +
  geom_jitter() +
  stat_compare_means(method = "t.test", vjust = -0.5) +
  labs(x = "Sowing Date", y = "Total amount of florets") +
  ggtitle("Floret Count by Sowing Date (Batch 11)") +
  theme(legend.position = "none")


#Abortion Rate by sowing Date
ggplot(grain_set, aes(x = factor(time_id), y = abortion.rate, fill = time_id)) +
  stat_boxplot(geom="errorbar", width=0.5)+
  geom_boxplot() +
  geom_jitter() +
  stat_compare_means(method = "t.test",  vjust = -0.5) +
  labs(x = "Sowing Date", y = "Abortion Rate [%]") +
  ggtitle("Abortion Rate by Sowing Date (Batch 11)") +
  theme(legend.position = "none")

  



