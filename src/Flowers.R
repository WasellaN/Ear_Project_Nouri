rm(list = ls())
library(dplyr)
library(ggplot2)
library(ggpol)
library(viridis)
library(readxl)

df <- read_xlsx('./data/Grain_Counting/gc_40_1.xlsx', sheet = 10, col_names = TRUE)

# Specify the path to the Excel file
file_path <- "./data/Grain_Counting/gc_40_11.xlsx"

# Read all sheets into a list of dataframes

sheets <- excel_sheets(file_path)

# Read each sheet and store them in a list
tables <- lapply(sheets, function(sheet) {
  read_excel(file_path, sheet = sheet, col_names = TRUE)
})

# Combine all tables into one dataframe
combined_df <- bind_rows(tables)

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


