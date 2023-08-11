rm(list = ls())
library(dplyr)
library(ggplot2)
library(ggpol)
library(readxl)

readx<- function(p,sh){
  df <- readxl::read_xlsx(p,sheet = sh) %>% 
    mutate(time_id = ifelse(plot_id == 40, "early", ifelse(plot_id == 42, "late", NA))) %>% 
    mutate(plot_id= as.character(plot_id)) %>% 
    mutate(Notes = as.character(Notes)) %>% 
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

readx2<- function(p,sh){
  df <- readxl::read_xlsx(p,sheet = sh) %>% 
    mutate(time_id = ifelse(plot_id == 40, "early", ifelse(plot_id == 42, "late", NA))) %>% 
    mutate(plot_id= as.character(plot_id)) %>% 
    mutate(Notes = as.character(Notes)) %>% 
    mutate(var = case_when(var == "Potenzial" ~"potenzial",
                           T ~ var))
} 


p <- "data/Grain_Counting/gc_40_1.xlsx"

graindf1<- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
  readx2(p,.x)
}) %>% mutate(batch.id= as.character(1)) %>% 
  select(var, rep, plot_id, batch.id, spike, flower)

p <- "data/Grain_Counting/gc_40_11.xlsx"

graindf11<- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
  readx(p,.x)
}) %>% filter(!is.na(floret.pos)) %>% mutate(batch.id= as.character(11)) %>% 
  select(var, rep, plot_id, batch.id, spike, flower) %>% 
  group_by(rep) %>% 
  distinct(var, rep, plot_id, batch.id, spike, flower)




# Combine all tables into one dataframe
combined_df <- bind_rows(graindf1, graindf11)








#Summary SE function
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {

  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

library(plyr)
combined_df_SE <- summarySE(combined_df, measurevar="flower", groupvars=c("batch.id","spike"))

ggplot(combined_df_SE, aes(x=spike, y=flower, group = batch.id, colour=batch.id)) + 
  geom_errorbar(aes(ymin=flower-se, ymax=flower+se), width=.1) +
  coord_flip() +
  geom_line() +
  labs(x = "Spikelet No.", y = "Number of Florets", color = "batch.id") +
  geom_point() +
  ggtitle("Number of Florets between batches")

