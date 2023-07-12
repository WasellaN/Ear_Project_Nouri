rm(list = ls())
library(dplyr)
library(ggplot2)
library(ggpol)
library(viridis)

readx<- function(p,sh){
  df <- readxl::read_xlsx(p,sheet = sh) %>% 
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
#reading data
p <- "data/Grain_Counting/gc_42_11.xlsx"

graindf<- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
  readx(p,.x)
}) %>% filter(!is.na(floret.pos))
graindf %>% plot_fun()


#plot for avarage grain number
hypo1 <- function(df){
  p <- graindf %>%
    group_by(plot_id,rep,spike) %>%
    mutate(kernel.num = sum(length(kernel.type))) %>%
    group_by(plot_id,spike) %>%
    mutate(kernel.num = mean(kernel.num)) %>%
    group_by(plot_id) %>%
    #group_by(plot_id, rep, spike) %>%
    #mutate(kernel.var = ) %>%
    mutate(kernel.pos = as.numeric(cut(spike,breaks=3))) %>%
    mutate(kernel.pos = case_when(kernel.pos == 1 ~"basal",
                                  kernel.pos == 2 ~"central",
                                  T ~"apical")) %>%
    mutate(treatment = ifelse(plot_id == 57, "early","late")) %>%
    ungroup()%>%
    ggplot(aes(x = kernel.num, y = factor(spike))) +
    geom_boxplot() +
    labs(x = "Kernel Number", y = "Spike") +
    scale_fill_discrete(name = "Spike")
  return(p)
}
hypo1(graindf)
