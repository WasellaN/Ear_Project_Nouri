rm(list = ls())
library(dplyr)
library(ggplot2)
library(ggpol)
library(viridis)
library(readxl)
#Summary SE function
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
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

# Specify the path to the Excel file
file_path <- "./data/Grain_Counting/gc_40_1.xlsx"

# Read all sheets into a list of dataframes

sheets <- excel_sheets(file_path)

# Read each sheet and store them in a list
tables <- lapply(sheets, function(sheet) {
  read_excel(file_path, sheet = sheet, col_names = TRUE)
})

# Combine all tables into one dataframe
combined_df <- bind_rows(tables)

combined_df <- combined_df %>% 
  mutate(time.id=as.character(1))
graindf40 <- graindf40 %>% 
  mutate(time.id=as.character(11))

combined_df_time <- bind_rows(combined_df, graindf40) %>% 
  select(rep, spike, flower, time.id)

ggplot(combined_df_time, aes(x = spike, y = flower, group = spike, color = time.id)) +
  stat_boxplot(geom="errorbar", width=0.5)+
  geom_boxplot() +
  geom_path() +
  coord_flip() +
  facet_grid(rows = 'time.id') +
  labs(x = "spike", y = "flowers") 

combined_df_time_c <- summarySE(combined_df_time, measurevar="flower", groupvars=c("time.id","spike"))

ggplot(combined_df_time_c, aes(x=spike, y=flower, group = time.id, colour=time.id)) + 
  geom_errorbar(aes(ymin=flower-se, ymax=flower+se), width=.1) +
  coord_flip() +
  geom_line() +
  labs(x = "Spikelet No.", y = "Number of Florets", color = "batch.id") +
  geom_point() +
  scale_color_discrete(guide = guide_legend(override.aes = list(shape = NA)))+
  ggtitle("Number of Florets between batches")

