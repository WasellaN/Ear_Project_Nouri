rm(list = ls())
library(dplyr)
library(ggplot2)
library(ggpol)
library(viridis)
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

# Print the combined dataframe
print(combined_df)

ggplot(combined_df, aes(x = flower, y = spike, group = as.factor(rep), color = as.factor(rep))) +
  geom_path() +
  labs(x = "Number of Florets", y = "Spike No.") +
  scale_color_gradientn(colours = rainbow(length(unique(combined_df$rep)))) +
  theme_minimal()

ggplot(combined_df, aes(x = flower, y = spike)) +
  geom_boxplot() +
  labs(x = "rep", y = "spike") +
  theme_minimal()

ggplot(combined_df, aes(x = spike, y = flower, group = spike)) +
  stat_boxplot(geom="errorbar", width=0.5)+
  geom_boxplot() +
  coord_flip() +
  labs(x = "spike", y = "flowers") +
  theme_bw()





##Old Code from Combined_time


# # Specify the path to the Excel file
# file_path <- "./data/Grain_Counting/gc_40_1.xlsx"
# 
# # Read all sheets into a list of dataframes
# 
# sheets <- excel_sheets(file_path)
# 
# # Read each sheet and store them in a list
# tables <- lapply(sheets, function(sheet) {
#   read_excel(file_path, sheet = sheet, col_names = TRUE)
# })
# 
# # Combine all tables into one dataframe
# combined_df <- bind_rows(tables) %>% 
#   mutate(plot_id= as.character(plot_id))
# 
# combined_df <- combined_df %>% 
#   mutate(time.id=as.character(1))
# graindf40 <- graindf40 %>% 
#   mutate(time.id=as.character(11))
# 
# combined_df_time <- bind_rows(combined_df, graindf40) %>% 
#   select(rep, spike, flower, time.id)
# 
# ggplot(combined_df, aes(x = spike, y = flower, group = spike, color = batch.id)) +
#   stat_boxplot(geom="errorbar", width=0.5)+
#   geom_boxplot() +
#   geom_path() +
#   coord_flip() +
#   facet_grid(rows = 'batch.id') +
#   labs(x = "spike", y = "flowers") 
