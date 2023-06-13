library(dplyr)
library(ggplot2)

climate <- read.csv('./data/climate.csv')

#Summarizing the years and months so only unique combinations are left
climate %>% 
  dplyr::filter(DFG_year=="DFG2019") %>% 
  group_by(y,m) %>% 
  summarise()

#Directly filtering for unique combinations of y and m
climate %>% 
  dplyr::filter(DFG_year=="DFG2019") %>% 
  dplyr::select(y,m) %>% 
  dplyr::distinct()


#Challenge 1
class(climate$DayTime)
print(climate$DayTime)
climate$DayTime <- as.Date(climate$DayTime)
climate_comp <- climate %>% 
  select(DFG_year, sowing_date, DayTime, Acc_Temperature) %>%
  group_by(DFG_year, sowing_date) %>%
  mutate(., DAS = as.numeric(DayTime - min(DayTime)))

climate_comp %>%
  ggplot(aes(x=DAS,y=Acc_Temperature, color=DFG_year, group = interaction(DFG_year, sowing_date), linetype=sowing_date))+
  geom_line(size = 1.2) +
  xlab("Days after Sowing")+ #x axis title
  ylab("Thermal Sum (°Cd)")+   #y axis title
  guides(color=guide_legend(title="Year"))+ #change legend title 
  theme_bw()+
  theme(
    legend.position = c(0.02, 0.90),
    legend.justification = c(0, 1),
    legend.box.just = "left"
  )+
  facet_grid(~DFG_year)

#challenge 2
ear_sum <- read.csv('./Data/ear_summarized.csv')
#using select and distinct
ear_sum %>%
  select(nitrogen, appl, timeid) %>%
  distinct()
#using group_by and summarise
ear_sum %>%
  group_by(nitrogen, appl, timeid) %>%
  summarise()

# climate %>%glimpse()
climate_long <- climate %>% 
  tidyr::pivot_longer(names_to = "Daily_Terms",
                      values_to = "Daily_value",
                      cols = contains("Daily")) 
# climate_long%>%   names()

#select cols by position
# grep("(Daily|Acc)",names(climate))
climate_long <- climate %>% 
  tidyr::pivot_longer(names_to = "Terms",
                      values_to = "value",
                      # select both patterns
                      cols = grep("(Daily|Acc)",names(.)))

# climate_long%>% names()

## data processing example
climate_long_subset<- climate_long %>% 
  filter(Terms%in%c('Acc_Temperature','Acc_Precipitation')) %>% 
  group_by(DFG_year,sowing_date,Terms) %>%
  summarise(Value=mean(value))

climate_long_subset

#Fig 2
library(scales) %>% suppressMessages()

climate_long %>% 
  filter(Terms%in%c('Acc_Temperature','Acc_Radiation'),
    sowing_date=='Early') %>%
  group_by(DFG_year,sowing_date) %>%
  mutate(DayTime=as.Date(DayTime,format="%Y-%m-%d"),
         DAS=as.numeric(DayTime-min(DayTime))) %>% 
  ggplot(aes(DAS,value,color=DFG_year))+ #sets up the plot and its aesthetics
  geom_line()+ #sets plot to a line
  facet_grid(~Terms)+ # facets the plot by terms
  theme_test()+ #sets theme
  theme(strip.background = element_blank(), #takes away facettitle background
        strip.text = element_text(size=14), #changes size of the text in facettitle
        axis.text = element_text(size=14), #changes size of axis text
        axis.title = element_text(size=14),
        legend.position = c(.1,.1))+ #changes the look of the graph
  scale_y_log10(
    labels = label_number(scale_cut = cut_short_scale()) #label number needs pkg scales
  )+ #changes the scale of y to logarthmic
  xlab('Days after sowing') #names the x axis

# long
climate_long <- climate %>% # climate is wide
  tidyr::pivot_longer(names_to = "Daily_Terms",
                      values_to = "Daily_value",
                      cols = contains("Daily")) 
# wide again
climate_wide<- climate_long%>% 
  tidyr::pivot_wider(names_from = "Daily_Terms",
                     values_from = "Daily_value")

# check if they are the same 
setdiff(names(climate),names(climate_wide))

class(climate)
class(climate_wide)

# change the order of column
all.equal(climate,climate_wide[,names(climate)])

# change the type
climate_wide <- climate_wide[,names(climate)]%>% as.data.frame()

filename <- c('grain_counting_practice_clement.xlsx',
              'grain_counting_practice_hanwenhsu.xlsx',
              'grain_counting_practice_shawon.xlsx')
file_list<- filename %>% strsplit("_")
# tradition way of for loop
res <- c()
for(i in 1:2){
  res <- c(res,file_list[[i]][4])
}
res
class(file_list)
file_list
# alternative in r package purrr
# chr stands for the "character" output.
purrr::map_chr(1:length(file_list),  ~{
  file_list[[.x]][4]
})

# notice that the output of map_chr must be 1 element per iteration.
purrr::map_chr(filename,  ~{
  .x %>% strsplit("_") %>% unlist()
})

# equivalent
purrr::map(filename,  ~{
  .x %>% strsplit("_") %>% unlist()
})

lapply(filename,function(x){
  x %>% strsplit("_") %>% unlist()
})

library(rJava)
library(xlsx)
install.packages('readxl')
library(readxl)

clement <- read_excel('./data/student/grain_counting_practice_clement.xlsx')

#challenge 3
filename <- list.files('./data/student')
filename
for(i in filename){
  paste0('./data/student/', i) %>%
    read_xlsx() %>%
    names() %>%
    print()
}

